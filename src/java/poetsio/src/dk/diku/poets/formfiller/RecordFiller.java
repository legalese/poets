package dk.diku.poets.formfiller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import dk.diku.poets.exception.PoetsException;

import dk.diku.poets.gen.thrift.contracts.Application;
import dk.diku.poets.gen.thrift.contracts.BinaryOp;
import dk.diku.poets.gen.thrift.contracts.BinOp;
import dk.diku.poets.gen.thrift.contracts.Case;
import dk.diku.poets.gen.thrift.contracts.CaseExp;
import dk.diku.poets.gen.thrift.contracts.Exp;
import dk.diku.poets.gen.thrift.contracts.Expression;
import dk.diku.poets.gen.thrift.contracts.Function;
import dk.diku.poets.gen.thrift.contracts.IfThenElse;
import dk.diku.poets.gen.thrift.contracts.Lambda;
import dk.diku.poets.gen.thrift.contracts.RecordProj;
import dk.diku.poets.gen.thrift.contracts.RecordUpdate;
import dk.diku.poets.gen.thrift.PoetsServer;

import dk.diku.poets.gen.thrift.reporting.Report;
import dk.diku.poets.gen.thrift.value.Date;
import dk.diku.poets.gen.thrift.value.DateTime;
import dk.diku.poets.gen.thrift.value.Time;
import dk.diku.poets.gen.thrift.value.Value;

import dk.diku.poets.gen.thrift.value.Record;
import dk.diku.poets.gen.thrift.value.Entity;

import dk.diku.poets.poetsserver.ServerUtils;

//import dk.diku.poets.record.PoetsType;
import dk.diku.poets.record.PoetsValue;
import dk.diku.poets.record.PoetsValue.RecV;
import dk.diku.poets.record.RecBuilder;
import dk.diku.poets.record.RecordDecode;
import dk.diku.poets.record.RecordEncode;

import dk.diku.poets.record.Calendar;

public class RecordFiller {

	public static class InferErrorFieldNotFound extends Exception {
		private static final long serialVersionUID = 7526472295622776145L;
		HashMap<String, PoetsValue> env;
		Exp exp;
		public InferErrorFieldNotFound(Exp er, HashMap<String, PoetsValue> env) {
			this.exp = er;
			this.env = env;
		}
		public String getMessage() {
			return new String("FieldNameExp not found in env: " + this.exp + " env = " + env);
		}
	}
	public static class InferError extends Exception {
		private static final long serialVersionUID = 7526472295622776147L;
		String msg;
		public InferError() {
			msg = new String("Failed to infer bindings for some reason");
		}
		public InferError(String er) {
			msg = er;
		}
		public String getMessage() {
			return msg;
		}
	}

	private final Map<Integer, Exp> exprMap;
	private final Integer root;
	private Set<String> boundVars;
	//private PoetsType.RecT rType;

	private Integer nextId;

	private void initNextId() {
		Integer maxId = 0;
		for(Integer i : exprMap.keySet()) {
			if(i > maxId) {
				// found larger in exprMap
				maxId = i + 1;
			}
		}
		nextId = maxId;
	}

	private Integer getFreshId() {
		Integer id = this.nextId;
		this.nextId = this.nextId + 1;
		return id;
	}

	public RecordFiller(Expression expr) {
		// we copy the exp-map from the given expression because when
		// normalizing and substituting in exps we might add new bindings
		// in the map. These bindings should probably not be visible to
		// the user of the RecordFiller
		System.out.println("[RF] expr = " + expr);
		this.exprMap = new HashMap<Integer, Exp>(expr.getExpressions());
		this.root = expr.getRoot();
		this.boundVars = new HashSet<String>();
		initNextId();
	}
	
	private Double toDouble(PoetsValue pv) {
		if(pv instanceof PoetsValue.IntV) {
			return new Double(((PoetsValue.IntV)pv).val);
		} else if(pv instanceof PoetsValue.DoubleV) {
			return new Double(((PoetsValue.DoubleV)pv).val);
		} else {
			return new Double(0);
		}
	}

	private PoetsValue times(PoetsValue v1, PoetsValue v2) {
		// we convert to double if either is a double and otherwise keep
		// the int type
		if(v1 instanceof PoetsValue.DoubleV ||
				v2 instanceof PoetsValue.DoubleV) 
		{
			Double d1 = toDouble(v1);
			Double d2 = toDouble(v2);
			return new PoetsValue.DoubleV(d1 * d2);
		} else {
			// neither is a double -> use int
			System.out.println("[RF] numbers " + v1 + " * " + v2);
			Integer i1 = ((PoetsValue.IntV) v1).val;
			Integer i2 = ((PoetsValue.IntV) v2).val;
			return new PoetsValue.IntV(i1 * i2);
		}
	}

	private PoetsValue div(PoetsValue v1, PoetsValue v2) {
		// we convert to double if either is a double and otherwise keep
		// the int type
		System.out.println("[RF] DIV of " + v1 + " / " + v2);
		if(v1 instanceof PoetsValue.DoubleV ||
				v2 instanceof PoetsValue.DoubleV) 
		{
			Double d1 = toDouble(v1);
			Double d2 = toDouble(v2);
			return new PoetsValue.DoubleV(d1 / d2);
		} else {
			// neither is a double -> use int
			Integer i1 = ((PoetsValue.IntV) v1).val;
			Integer i2 = ((PoetsValue.IntV) v2).val;
			return new PoetsValue.IntV(i1 / i2);
		}
	}

	private boolean toBool(PoetsValue b) 
	throws InferError {
		if(b instanceof PoetsValue.BoolV) {
			return ((PoetsValue.BoolV)b).val;
		} else {
			throw new InferError("toBool: " + b);
		}
	}

	private PoetsValue and(PoetsValue b1, PoetsValue b2) 
	throws InferError {
		// if either value is not a boolean, we throw an InferError
		// (because that would be a type-error) otherwise we simply return
		// a BoolV
		return new PoetsValue.BoolV(toBool(b1) && toBool(b2));
	}


	private boolean isFieldNameRef(Exp exp) {
		return
			exp.isSetFieldNameExp();
	}

	private boolean isFieldProject(Exp exp) {
		return 
			exp.isSetRecordProjExp();
	}

	private void addBinding(
			String fieldName,
			PoetsValue pv,
			HashMap<String, PoetsValue> env)
		throws InferError
	{
		if(env.containsKey(fieldName)) {
			PoetsValue oldValue = env.get(fieldName);
			if(!oldValue.equals(pv)) {
				// conflicting bindings
				throw new InferError("Conflicting bindings");
			}
		} else {
			// putting new key,value
			env.put(fieldName, pv);
		}
	}


	private PoetsValue binaryInfer(
		BinaryOp binOpExp,
		HashMap<String, PoetsValue> env)
		throws InferError, InferErrorFieldNotFound
	{
		PoetsValue retPVal = null;
		BinOp binOp = binOpExp.getOperator();
		Exp leftExp = exprMap.get(binOpExp.getLeftExp());
		Exp rightExp = exprMap.get(binOpExp.getRightExp());

		// first check whether we have an equality check on a fieldName
		System.out.println("[RF] checking " + binOpExp);
		if(binOp.equals(BinOp.EQ)) {
			if(isFieldNameRef(leftExp)) {
				// try to normalize the right-hand side exp
				// and in case of success, add the PoetsValue to the
				// env-mapping
				System.out.println("[RF] leftmost has fieldName");
				try {
					System.out.println("[RF] trying to eval exp: " + rightExp);
					PoetsValue pv = inferEnv(rightExp, env);
					System.out.println("[RF] eval rhs: " + pv);
					String fieldName = leftExp.getFieldNameExp();
					System.out.println("[RF] adding binding to " + fieldName);
					addBinding(fieldName, pv, env);
					// the value of equality check is assumed to be true
					retPVal = new PoetsValue.BoolV(true);
				} catch(InferError m) {
					System.out.println("[RF] caught an error!");
					// we could not normalize the value or we have conflicting
					// bindings so we give up on the whole inference business
					if(env.size() == 0) {
						throw new InferError("Could not normalize exp:" + rightExp + "\n >> " + m);
					} else {
						// there may be valuable bindings in env
						retPVal = new PoetsValue.BoolV(true);
					}
				}
			} else if(isFieldNameRef(rightExp)) {
				System.out.println("[RF] rightmost has fieldName");
				try {
					PoetsValue pv = inferEnv(leftExp, env);
					String fieldName = rightExp.getFieldNameExp();
					addBinding(fieldName, pv, env);
					retPVal = new PoetsValue.BoolV(true);
				} catch(InferError m) {
					// we could not normalize the value so we give up on the
					// whole inference business
					throw new InferError("BinOP.EQ: hmm?");
				}
			} else if(isFieldProject(leftExp)) {
				System.out.println("[RecordFiller] BFH!");
				// BFH warnining: someone should do this right at some point!

				// handle the LHS of the case when exp is
				// res.fname == <some-exp>
				try {
					PoetsValue pv = inferEnv(rightExp, env);
					System.out.println("[RF] inferred RHS to : " + pv);
					// assumption: LHS is "f.n"
					String baseName = 
						exprMap.get((leftExp.getRecordProjExp().getRecordExp())).
						getFieldNameExp();
					String projName =
						leftExp.getRecordProjExp().getFieldName();
					
					System.out.println("[RecordFiller] adding binding of '"
							+baseName+"⁋"+projName+"' to value: "+pv);	
					addBinding(baseName + "⁋" + projName, pv, env);
					retPVal = new PoetsValue.BoolV(true);
				} catch (InferError m) {
					// failed to infer the special case of "r.f == exp"; just
					// give up then
					throw new InferError("BinOP.EQ: r.f == exp special case failed");
				}
			} else if(isFieldProject(rightExp)) {
				System.out.println("[RecordFiller] BFH TODO!");
				// similar to previous case
			} else {
				// different equality check; evaluate both branches to true
				// and bail out if they don't both eval to true
				System.out.println("[RecordFiller] neither left nor right");
				PoetsValue pvLeft = inferEnv(leftExp, env);
				PoetsValue pvRight = inferEnv(rightExp, env);
				retPVal = new PoetsValue.BoolV(pvLeft.equals(pvRight));
			}
		}

		// at this point we know that the binopexp was *not* an assignment
		// to a fieldName but some other binary operator; dispatch on kind
		// of operator:
		System.out.println("[RF] eval left side: " + leftExp);
		PoetsValue pvLeft = inferEnv(leftExp, env);
		System.out.println("[RF] eval right side: " + rightExp);
		PoetsValue pvRight = inferEnv(rightExp, env);
		System.out.println("[RF] both sides of binop eval'ed");
		if(binOp.equals(BinOp.LEQ)) {
			//TODO
		} else if(binOp.equals(BinOp.PLUS)) {
			// check that both PValues are numbers and one is smaller than
			// the other; it's safe to convert ints to double here
			Double n1 = toDouble(pvLeft);
			Double n2 = toDouble(pvRight);
			retPVal = new PoetsValue.BoolV(n1 <= n2);
		} else if(binOp.equals(BinOp.TIMES)) {
			retPVal = times(pvLeft, pvRight);
		} else if(binOp.equals(BinOp.DIV)) {
			retPVal = div(pvLeft, pvRight);
		} else if(binOp.equals(BinOp.AND)) {
			retPVal = and(pvLeft, pvRight);
		} else if(binOp.equals(BinOp.CONS)) {
			// assuming pvRight is really a ListV
			if(pvRight instanceof PoetsValue.ListV) {
				((PoetsValue.ListV)pvRight).val.add(0, pvLeft);
				retPVal = pvRight;
			} else {
				System.out.println("[RF] CONS("+pvLeft+", "+pvRight+")");
				throw new InferError("BinOp.CONS N/A");
			}
		} else if(binOp.equals(BinOp.DPLUS)) {
			throw new InferError("BinOp.DPLUS N/A");
		} else if(binOp.equals(BinOp.DTIMES)) {
			throw new InferError("BinOP.DTIMES N/A");
		} else {
			if(retPVal == null) {
			// unknown binop
				throw new InferError("Unknown binop: " + binOp);
			}
		}

		return retPVal;
	}

	private Integer findIndex(Exp exp) {
		for(Integer i : exprMap.keySet()) {
			if(exprMap.get(i).equals(exp)) {
				return i;
			}
		}
		return null;
	}

	private Integer returnExp(Exp exp) {
		// check whether exp is already bound in exprMap; because then, we
		// simply reuse the index found
		// Maybe it is quicker to simply put in a new exp and disrecard
		// that we might be abusing memory? It seems to depend on whether
		// we will quickly exhaust the capacity of the exprMap. 
		Integer hasIndex = findIndex(exp);
		if(hasIndex != null) {
			return hasIndex;
		} else {
			Integer nId = getFreshId();
			exprMap.put(nId, exp);
			return nId;
		}
	}

	private Integer substituteList(Integer listIndex, String var, Exp arg) {
		List<Integer> exps = exprMap.get(listIndex).getListExps();

		List<Integer> newList = new ArrayList<Integer>();

		for(Integer i : exps) {
			newList.add(substitute(i, var, arg));
		}
		Exp newExp = new Exp();
		newExp.setListExps(newList);
		return returnExp(newExp);
	}

	/*** The substitute method replaces all free occurrences of var with
	 * arg in body.
	 *
	 * Recall that an Exp may contain references to its subexpressions
	 * using indicies in 'exprMap'. In order to correctly reference the
	 * modified subexpressions, we need to make new entries in the given
	 * 'exprMap'.
	 * @param body The Exp in which to perform a substitution.
	 * @param var The variable in 'body' to replace.
	 * @param arg The Exp to insert in place of 'var' in 'body'.
	 * @return The method returns an index in the 'exprMap' pointing to
	 * the newly created exp.
	 */
	private Integer substitute(Integer bodyIndex, String var, Exp arg) {
		Exp body = exprMap.get(bodyIndex);
		if(body.isSetVariableExp()) {
			if(body.getVariableExp().equals(var)) {
				return returnExp(arg);
			} else {
				return bodyIndex;
			}
		} else if(body.isSetBinaryOpExp()) {
			BinaryOp binop = body.getBinaryOpExp();
			Integer newLeftIdx = substitute(binop.getLeftExp(), var, arg);	
			Integer newRightIdx = substitute(binop.getRightExp(), var, arg);

			BinaryOp newBinOp = new BinaryOp();
			newBinOp.setOperator(binop.getOperator());
			newBinOp.setLeftExp(newLeftIdx);
			newBinOp.setRightExp(newRightIdx);
			Exp newExp = new Exp();
			newExp.setBinaryOpExp(newBinOp);

			return returnExp(newExp);
		} else if(body.isSetRecordExp()) {
			Record rec = body.getRecordExp();
			Map<String, Integer> fields = rec.getFields();
			// construct new fieldsMap
			Map<String, Integer> newFields = 
				new HashMap<String, Integer>();
			for(String fieldName : rec.getFields().keySet()) {
				Integer newFieldExpIxd = 
					substitute(fields.get(fieldName), var, arg);
				newFields.put(fieldName, newFieldExpIxd);
			}

			Record newRec = new Record();
			newRec.setRecordName(rec.getRecordName());
			newRec.setFields(newFields);
			Exp newExp = new Exp();
			newExp.setRecordExp(newRec);
			return returnExp(newExp);
		} else if(body.isSetRecordProjExp()) {
			RecordProj rProj = body.getRecordProjExp();

			Integer newExpIdx = substitute(rProj.getRecordExp(), var, arg);

			RecordProj newProj = new RecordProj();
			newProj.setFieldName(rProj.getFieldName());
			newProj.setRecordExp(newExpIdx);
			Exp newExp = new Exp();
			newExp.setRecordProjExp(newProj);
			return returnExp(newExp);
		} else if(body.isSetRecordUpdateExp()) {
			RecordUpdate rUp = body.getRecordUpdateExp();

			Integer newReExp = substitute(rUp.getRecordExp(), var, arg);
			Integer newUpExp = substitute(rUp.getUpdateExp(), var, arg);

			RecordUpdate newRUp = new RecordUpdate();
			newRUp.setFieldName(rUp.getFieldName());
			newRUp.setRecordExp(newReExp);
			newRUp.setUpdateExp(newUpExp);
			Exp newExp = new Exp();
			newExp.setRecordUpdateExp(newRUp);
			return returnExp(newExp);
		} else if(body.isSetLambdaExp()) {
			Lambda λ = body.getLambdaExp();
			Integer λbodyIdx;
			if(boundVars.contains(var)) {
				// var is already bound -> var must have been bound
				// "previously" so don't unbind it after substitution
				 λbodyIdx = substitute(λ.getBodyExp(), var, arg);
			} else {
				boundVars.add(var);
				λbodyIdx = substitute(λ.getBodyExp(), var, arg);
				boundVars.remove(var);
			}

			Lambda newλ = new Lambda();
			newλ.setVariable(λ.getVariable());
			newλ.setBodyExp(λbodyIdx);

			Exp newExp = new Exp();
			newExp.setLambdaExp(newλ);
			return returnExp(newExp);
		} else if(body.isSetApplicationExp()) {
			Application appExp = body.getApplicationExp();

			Integer newFunIdx = 
				substitute(appExp.getFunctionExp(), var, arg);
			Integer newArgIdx =
				substitute(appExp.getArgumentExp(), var, arg);

			Application newApp = new Application();
			newApp.setFunctionExp(newFunIdx);
			newApp.setArgumentExp(newArgIdx);
			Exp newExp = new Exp();
			newExp.setApplicationExp(newApp);
			return returnExp(newExp);
		} else if(body.isSetIfThenElseExp()) {
			IfThenElse ite = body.getIfThenElseExp();

			Integer newCondIdx =
				substitute(ite.getConditionalExp(), var, arg);
			Integer newThenIdx =
				substitute(ite.getThenExp(), var, arg);
			Integer newElseIdx =
				substitute(ite.getElseExp(), var, arg);

			IfThenElse newITE = new IfThenElse();
			newITE.setConditionalExp(newCondIdx);
			newITE.setThenExp(newThenIdx);
			newITE.setElseExp(newElseIdx);
			Exp newExp = new Exp();
			newExp.setIfThenElseExp(newITE);
			return returnExp(newExp);
		} else if(body.isSetCaseExp()) {
			Case caseExp = body.getCaseExp();

			Integer newCaseIdx = 
				substitute(caseExp.getCaseExp(), var, arg);
			List<CaseExp> newCaseExps =
				new ArrayList<CaseExp>();

			for(CaseExp ce : caseExp.getCaseExps()) {
				CaseExp newCE = new CaseExp();
				newCE.setRName(ce.getRName());
				newCE.setVariable(ce.getVariable());

				if(boundVars.contains(ce.getVariable())) {
					newCE.setBody(substitute(ce.getBody(), var, arg));
				} else {
					boundVars.add(ce.getVariable());
					newCE.setBody(substitute(ce.getBody(), var, arg));
					boundVars.remove(ce.getVariable());
				}

				newCaseExps.add(newCE);
			}

			Case newCase = new Case();
			newCase.setCaseExp(newCaseIdx);
			newCase.setCaseExps(newCaseExps);

			Exp newExp = new Exp();
			newExp.setCaseExp(newCase);
			return returnExp(newExp);
		} else if(body.isSetListExps()) { 
			return substituteList(bodyIndex, var, arg);
		} else {
			return bodyIndex;
		}
	}


	private Integer normalizeFold(
			Integer topExp,
			Function fun, 
			Integer lamIdx, 
			Integer leftMostIdx, Integer rightMostIdx) {
		Integer curAccIdx = 0;
		switch(fun) {
			case Foldr:
				// start from last element
				// e2 is acc
				// e1 is list to fold over
				System.out.println("[RF] list to fold: " + exprMap.get(rightMostIdx));
				System.out.println("[RF] acc arg. : " + exprMap.get(leftMostIdx));
				curAccIdx = leftMostIdx;
				if(!exprMap.get(rightMostIdx).isSetListExps()) {
					// no listexp is set, so we can't eval. expr.
					return topExp;
				}
				List<Integer> expInts = exprMap.get(rightMostIdx).getListExps();
				System.out.println("[RF] expInts: " + expInts);
				for(int ix = expInts.size() - 1; ix >= 0; ix--) {
					System.out.println("[RF] handling expInt["+ix+"]");
					// construct new app-exp
					// app(app(lamE, curAcc), exprMap.get(ix))
					Application innerApp = 
						new Application()
						.setFunctionExp(lamIdx)
						.setArgumentExp(expInts.get(ix));
					Exp innerExp = new Exp()
						.setApplicationExp(innerApp);
					System.out.println("[RF] innerExp " + innerExp);
					Integer innerIdx = returnExp(innerExp);
					Application outerApp = 
						new Application()
						.setFunctionExp(innerIdx)
						.setArgumentExp(curAccIdx);
					Exp outerExp = new Exp()
						.setApplicationExp(outerApp);
					System.out.println("[RF] outerExp " + outerExp);
					curAccIdx = returnExp(outerExp);
				}
				System.out.println("[RF] fold-exp before normalization: " + exprMap.get(curAccIdx));
				System.out.println("[RF] in exprMap: " + exprMap);
				try {
					System.out.println("[RF] normalizing exp: " + curAccIdx + " -> " + exprMap.get(curAccIdx));
					Integer nInt = normalizeExp(curAccIdx);
					System.out.println("[RF] normalized exp: " + nInt);
					return nInt;
				} catch (NullPointerException e) {
					System.out.println("[RF] gave NPE?");
					return topExp;
				}
			default:
				break;
		}
		return topExp;
	}

	enum FunctionKind {
		FOLD,
		REPORT,
		UNKNOWN
	}

	private Exp getLeftMostNonApp(Integer expIndex) {
		Exp curExp = exprMap.get(expIndex);
		while(curExp.isSetApplicationExp()) {
			curExp = exprMap.get(curExp.getApplicationExp().getFunctionExp());
		}
		return curExp;		
	}

	private Report extractReport(Integer expIndex) {
		Exp repExp = getLeftMostNonApp(expIndex);	
		if(repExp.isSetRecordProjExp()) {
			String lowerReportName = 
				repExp.getRecordProjExp().getFieldName();
			String reportName = 
				lowerReportName.substring(0,1).toUpperCase() +
				lowerReportName.substring(1);
			// get report info from server
			try {
				PoetsServer.Iface pServer = ServerUtils.getServer();
				Report rep = pServer.getReport(reportName);
				return rep;
			} catch (Exception e) {
				System.out.println("[RF] something went wrong when trying to query report '"+reportName+"'");
				e.printStackTrace();
				return null;
			}


		} 
			
		return null;
	}

	private FunctionKind extractFunctionKind(Integer expIndex) {
		// idea is: traverse down left-most spine of ApplicationExp until
		// reaching a LHS that is NOT an ApplicationExp. At that point we
		// test the kind of expression
		Exp curExp = getLeftMostNonApp(expIndex);
		// At this point we know that curExp is _not_ an AppExp, we can
		// then see whether it is a FunctionExp
		if(curExp.isSetFunctionExp()) {
			switch(curExp.getFunctionExp()) {
				case Foldr:
					// ((fold_@lamb)@e1)@e2 -> FOLD
					return FunctionKind.FOLD;
				default:
					return FunctionKind.UNKNOWN;
			}
		} else {
			System.out.println("[RF] looking for projection in " + curExp);
			// if we have projection and lhs is a functionExp we might have
			// report-call
			if(curExp.isSetRecordProjExp()) {
				try {
				switch(
						exprMap.get(curExp.getRecordProjExp().getRecordExp())
						.getFunctionExp()) {
					case Reports:
						return FunctionKind.REPORT;
					default:
						return FunctionKind.UNKNOWN;
				}
				} catch (Exception e) {
					System.out.println("[RF] null? " + curExp);
					return FunctionKind.UNKNOWN;
				}
			} else {
				return FunctionKind.UNKNOWN;
			}
		}
	}

	private Integer normalizeFunction(Integer expIndex) {
		Exp topApp = exprMap.get(expIndex);
		switch(extractFunctionKind(expIndex)) {
			case FOLD:
				// first detect what function we are seeing
				// we are looking for an instance of the pattern 
				// '(app(app(app(fold_, lamb), e1), e2))'
				if(topApp.isSetApplicationExp()) {
					// we found app(leftExp, e1)
					Exp leftExp = exprMap.get(topApp.getApplicationExp().getFunctionExp());
					if(leftExp.isSetApplicationExp()) {
						// we found app(app(llExp, e2), e1)
						Exp llExp = exprMap.get(leftExp.getApplicationExp().getFunctionExp());
						if(llExp.isSetApplicationExp()) {
							// we found app(app(app(fun, lam)), e2), e1)
							// now check that e1 is a fold and e2 is a lambda
							Exp funE = exprMap.get(llExp.getApplicationExp().getFunctionExp());
							Integer lamIdx = llExp.getApplicationExp().getArgumentExp();
							Exp lamE = exprMap.get(lamIdx);
							if(funE.isSetFunctionExp() && lamE.isSetLambdaExp()) {
								// we found app(app(app(fun,lamb), _,), _)
								System.out.println("[RF] we found a fold; trying to evaluate " + topApp);
								Integer rightMostIdx = normalizeExp(topApp.getApplicationExp().getArgumentExp());
								Integer leftMostIdx = normalizeExp(leftExp.getApplicationExp().getArgumentExp());
								Integer nId = normalizeFold(
										expIndex,
										funE.getFunctionExp(), lamIdx,
										leftMostIdx, rightMostIdx);
								System.out.println("[RF] new id for fold: " + nId + " with value " + exprMap.get(nId));
								System.out.println("[RF] exprMap = " + exprMap);
								return nId;
							}
						}
					}
				}
				break;
			case REPORT:
				System.out.println("[RF] REPORT CASE");
				// figure out what report we are supposed to query; when we
				// have the name of the report, we can fetch it's type and
				// from that we know how many arguments are needed for the
				// query.
				Report rep = extractReport(expIndex);
				System.out.println("[RF] report to query: " + rep);
				Integer numArgs = rep.getType().getArgTypes().size();
				// iterate down the exp-tree converting arguments on the way
				// into a new args-list
				List<Value> argValues = new ArrayList<Value>();
				try {
					while(numArgs > 0 && topApp.isSetApplicationExp()) {
						Application appExp = topApp.getApplicationExp();
						// extract RHS and convert to value and add to argValues
						// converting an arg to pval is not supposed to add
						// bindings nor use them
						PoetsValue argPVal = 
							inferEnv(exprMap.get(appExp.getArgumentExp()),
									new HashMap<String, PoetsValue>());

						argValues.add(0, RecordEncode.encodeValue(argPVal));
						// extract LHS and repeat
						topApp = exprMap.get(appExp.getFunctionExp());
					}
					if(argValues.size() == numArgs) {
						// query the report and add the value to exprMap?
						PoetsServer.Iface pServer = ServerUtils.getServer();
						Value repRes = pServer.queryReport(rep.getName(), new ArrayList<Value>(), argValues);
						PoetsValue.RecV recV = (PoetsValue.RecV) RecordDecode.decodeValue(repRes);
						System.out.println("[RF] query result: " + recV);
						Integer cInt = convertRecV(recV);
						System.out.println("[RF] converted query to : " + cInt);
						System.out.println("[RF] exprMap: " + exprMap);
						return cInt;
					} else {
						System.out.println("[RF] did not find correct number of args for report: " + rep);
						System.out.println("[RF] found just : " + argValues);
						return expIndex;
					}
				} catch (Exception e) {
					e.printStackTrace();
					return expIndex;
				}
			default:
				break;
		}
		return expIndex;
	}

	private Integer convertPVal(PoetsValue pVal) {
		Exp retExp = new Exp();
		if (pVal instanceof PoetsValue.IntV) {
			retExp.setIntExp(((PoetsValue.IntV) pVal).val);
		} else if (pVal instanceof PoetsValue.BoolV) {
			retExp.setBoolExp(((PoetsValue.BoolV)pVal).val);
		} else if (pVal instanceof PoetsValue.StringV) {
			retExp.setStringExp(((PoetsValue.StringV)pVal).val);
		} else if (pVal instanceof PoetsValue.DateTimeV) {
			PoetsValue.DateTimeV dt = (PoetsValue.DateTimeV) pVal;
			DateTime dtv = new DateTime();
			Calendar cal = dt.val;
			dtv.date.setYear(cal.get(Calendar.YEAR));
			dtv.date.setMonth(cal.get(Calendar.MONTH));
			dtv.date.setDay(cal.get(Calendar.DAY_OF_MONTH));
			dtv.time.setHour(cal.get(Calendar.HOUR));
			dtv.time.setMinute(cal.get(Calendar.MINUTE));
			dtv.time.setSecond(cal.get(Calendar.SECOND));
			retExp.setDateTimeExp(dtv);
		} else if (pVal instanceof PoetsValue.DateV) {
			PoetsValue.DateV dt = (PoetsValue.DateV) pVal;
			Date dtv = new Date();
			Calendar cal = dt.val;
			dtv.setYear(cal.get(Calendar.YEAR));
			dtv.setMonth(cal.get(Calendar.MONTH));
			dtv.setDay(cal.get(Calendar.DAY_OF_MONTH));
			retExp.setDateExp(dtv);
		} else if (pVal instanceof PoetsValue.TimeV) {
			PoetsValue.TimeV dt = (PoetsValue.TimeV) pVal;
			Time dtv = new Time();
			Calendar cal = dt.val;
			dtv.setHour(cal.get(Calendar.HOUR));
			dtv.setMinute(cal.get(Calendar.MINUTE));
			dtv.setSecond(cal.get(Calendar.SECOND));
			retExp.setTimeExp(dtv);
		} else if (pVal instanceof PoetsValue.DoubleV) {
			retExp.setRealExp(((PoetsValue.DoubleV)pVal).val);
		} else if(pVal instanceof PoetsValue.RecV) {
			return convertRecV((RecV)pVal);
		} else if (pVal instanceof PoetsValue.RefV) {
			Entity rv = new Entity();
			PoetsValue.RefV pref = (PoetsValue.RefV) pVal;
			rv.setRecordName(pref.getRefName());
			rv.setEntPointer(pref.getRefPointer());
			retExp.setEntityExp(rv);
		} else if (pVal instanceof PoetsValue.ListV) {
			List<PoetsValue> ls = ((PoetsValue.ListV)pVal).val;
			List<Integer> is = new ArrayList<Integer>(ls.size());
			for(PoetsValue pv : ls) {
				is.add(convertPVal(pv));
			}
			retExp.setListExps(is);
		} else {
			System.out.println("[RF] error in convertPVal " + pVal);
			return null;
		}
		
		return returnExp(retExp);
	}

	// converts a record-value to an equivalent exp-record
	// it is a little bit silly to actually do this because later on we
	// will be converting the exp-record back into a PoetsValue.RecV
	// again
	private Integer convertRecV(RecV recV) {
		// convert the value of each field
		if(recV.getIsAbstract()) {
			recV = recV.getInstance();
		}
		Map<String, Integer> fs = new HashMap<String, Integer>();
		for(String fieldName : recV.getKeySet()) {
			Integer idx = convertPVal(recV.getField(fieldName));
			fs.put(fieldName, idx);
		}
		// construct RecExp
		Record rExp = new Record();
		rExp.setFields(fs);
		Exp recExp = new Exp();
		recExp.setRecordExp(rExp);
		// obtain index and 'returnExp' as result
		return returnExp(recExp);
	}

	// reduce an exp as much as possible under the assumption that
	// expression evaluation will eventually terminate; also: in case
	// an actual reduction happens, modify the exprMap to reflect this.
	private Integer normalizeExp(Integer expIndex) {
		Exp exp = exprMap.get(expIndex);
		if(exp.isSetApplicationExp()) {
			Exp lamb = exprMap.get(normalizeExp(exp.getApplicationExp().getFunctionExp()));
			Exp arg  = exprMap.get(normalizeExp(exp.getApplicationExp().getArgumentExp()));
			if(lamb.isSetLambdaExp()) {
				Integer bodyIdx = lamb.getLambdaExp().getBodyExp();
				String var = lamb.getLambdaExp().getVariable();
				Integer newExpIdx = substitute(bodyIdx, var, arg);
				return normalizeExp(newExpIdx);
			} else {
				// lamb did not normalize to λ-exp maybe it could be an
				// inbuild 'function' such as fold or a report query
				return normalizeFunction(expIndex);
			}
		} else if(exp.isSetRecordProjExp()) {
			// normalize exp to record and extract field-exp
			RecordProj rejProj = exp.getRecordProjExp();
			Integer normRecExpIdx = normalizeExp(rejProj.getRecordExp());
			Exp normRecExp =  exprMap.get(normRecExpIdx);
			if(normRecExp.isSetRecordExp()) {
				return normRecExp.getRecordExp().getFields().get(rejProj.getFieldName());
			} else {
				System.out.println("[RF] not eval. exp to record: " + normRecExp);
				return expIndex;
			} 		
		} else if(exp.isSetRecordUpdateExp()) {
			// normalize recordExp and updateExp and insert updateExp in
			// recordExp under fieldName
			RecordUpdate rUpExp = exp.getRecordUpdateExp();
			Integer normRecExpIdx = normalizeExp(rUpExp.getRecordExp());
			Integer updateExpIdx  = normalizeExp(rUpExp.getUpdateExp());
			String fieldName = rUpExp.getFieldName();

			Exp recExp = exprMap.get(normRecExpIdx);
			if(recExp.isSetRecordExp()) {
				Record newRec = new Record(recExp.getRecordExp());
				// set fieldName to updateExpIdx
				newRec.getFields().put(fieldName, updateExpIdx);
				Exp newExp = new Exp();
				newExp.setRecordExp(newRec);
				return returnExp(newExp);
			} else {
				return expIndex;
			}
		} else if(exp.isSetCaseExp()) {
			// try to normalize the caseExp to a Record
			// if possible, find the Record in the list of CaseExp and
			// return the normalized version of that Exp
			Case caseE = exp.getCaseExp();
			Exp caseNorm = exprMap.get(normalizeExp(caseE.getCaseExp()));
			if(caseNorm.isSetRecordExp()) {
				Record rec = caseNorm.getRecordExp();
				String targetRec = rec.getRecordName();
				for(CaseExp ce : caseE.getCaseExps()) {
					if(ce.getRName().equals(targetRec)) {
						//TODO should we mark targetRec as bound?
						return normalizeExp(substitute(ce.getBody(), ce.getVariable(), caseNorm));
					}
				}
			} 

			// reaching this point meant we were unable to reduce the case
			// exp
			return expIndex;
		}	else if(exp.isSetBinaryOpExp()) {
			BinaryOp binOpExp = exp.getBinaryOpExp();

			BinaryOp newBinOpExp = new BinaryOp();
			newBinOpExp.setOperator(binOpExp.getOperator());
			newBinOpExp.setLeftExp(normalizeExp(binOpExp.getLeftExp()));
			newBinOpExp.setRightExp(normalizeExp(binOpExp.getRightExp()));

			Exp newExp = new Exp();
			newExp.setBinaryOpExp(newBinOpExp);
			return returnExp(newExp);
		} else if(exp.isSetIfThenElseExp()) {
			IfThenElse ite = exp.getIfThenElseExp();
			Exp normIteCond = exprMap.get(normalizeExp(ite.getConditionalExp()));
			if(normIteCond.isSetBoolExp()) {
				if(normIteCond.boolExp) {
					return normalizeExp(ite.getThenExp());
				} else {
					return normalizeExp(ite.getElseExp());
				}
			} else {
				return expIndex;
			}
		} else if(exp.isSetListExps()) {
			// TODO: normalize each component
			return expIndex;
		} else {
			return expIndex;
		}
	}

	/*** Try to evaluate a given Exp to 'true' and as a side-effect put
	 * bindings in env that stipulate what bindings must hold for the
	 * free variables in the Exp (only fieldNames currently)
	 */
	private PoetsValue inferEnv (
			Exp origExp,
			HashMap<String, PoetsValue> env)
		throws InferError, InferErrorFieldNotFound
	{
		Integer normExpIdx = normalizeExp(findIndex(origExp));
		Exp exp = exprMap.get(normExpIdx);
		PoetsValue pv = null;
		// dispatch on kind of exp
		if(exp.isSetBinaryOpExp()) {
			pv = binaryInfer(exp.getBinaryOpExp(), env);
		} else if(exp.isSetIntExp()) {
			pv = new PoetsValue.IntV(exp.getIntExp());
		} else if(exp.isSetBoolExp()) {
			pv = new PoetsValue.BoolV(exp.boolExp); // why is there no getter/setter for bools?
		} else if(exp.isSetDateExp()) {
			pv = new PoetsValue.DateV(
					RecordDecode.decodeCalendar(exp.getDateExp())
					);
		} else if(exp.isSetTimeExp()) {
			pv = new PoetsValue.TimeV(
					RecordDecode.decodeCalendar(exp.getTimeExp())
					);
		} else if(exp.isSetDateTimeExp()) {
			pv = new PoetsValue.DateTimeV(
					RecordDecode.decodeCalendar(exp.getDateTimeExp())
					);
		} else if(exp.isSetDurationExp()) {
			pv = new PoetsValue.DurationV(exp.getDurationExp());
		} else if(exp.isSetRealExp()) {
			pv = new PoetsValue.DoubleV(exp.getRealExp());
		} else if(exp.isSetStringExp()) {
			pv = new PoetsValue.StringV(exp.getStringExp());
		} else if(exp.isSetRecordExp()) {
			Record rec = exp.getRecordExp();
			System.out.println("[RF] infer rec: " + rec);

			RecBuilder rb;
			try {
				rb = new RecBuilder(rec.getRecordName());
			} catch (PoetsException e) {
				e.printStackTrace();
				throw new InferError("Server communication error, RecBuilder" + origExp);
			}
			Map<String, Integer> fields = rec.getFields();

			for(String fieldName : fields.keySet()) {
				System.out.println("[RF] converting field: " + fieldName + " from " + exprMap.get(fields.get(fieldName)));
				PoetsValue fieldVal = inferEnv(exprMap.get(fields.get(fieldName)), env);
				System.out.println("[RF] fieldval = " + fieldVal);
				rb.setField(fieldName, fieldVal);
				System.out.println("[RF] setting as " + fieldName);
			}

			pv = rb.create();
			System.out.println("[RF] created record " + pv);
		} else if(exp.isSetEntityExp()) {
			Entity rv = exp.getEntityExp();
			pv = new PoetsValue.RefV(
					rv.getRecordName(), rv.getEntPointer());
		} else if(exp.isSetListExps()) {
			List<Integer> exps = exp.getListExps();
			PoetsValue.ListV valList = new PoetsValue.ListV();
			for(Integer i : exps) {
				// convert to value and add to valList
				System.out.println("[RF] converting " + exprMap.get(i) + " to value");
				PoetsValue v = inferEnv(exprMap.get(i), env);
				System.out.println("[RF] value = " + v);
				valList.addElement(v);
			}
			//TODO: also put in the 'elementType'
			if(valList.val.size() > 0) { 
				valList.elementType = valList.val.get(0);
			}
			pv = valList;
		} else if(exp.isSetFieldNameExp()) {
			if(env.containsKey(exp.getFieldNameExp())) {
				pv = env.get(exp.getFieldNameExp());
			} else {
				// reference to as of yet undefined fieldname; notice that
				// finding a result is then dependent on the order in which we
				// evaluate subexpressions
				System.out.println("[RF] something is wrong in origExp: " + origExp);
				throw new InferErrorFieldNotFound(exp, env);
			}
		} else if(exp.isSetRecordProjExp()) {
			// first test whether we have already found a value for the
			// projected field and return that in case we have
			try {
				String baseName = 
					exprMap.get((exp.getRecordProjExp().getRecordExp())).
					getFieldNameExp();
				String projName =
					exp.getRecordProjExp().getFieldName();
				pv = env.get(baseName + "⁋" + projName);
			} catch (Exception e) {
				PoetsValue.RecV rec = (PoetsValue.RecV) inferEnv(exprMap.get(exp.getRecordProjExp().getRecordExp()), env);
				String projectField = exp.getRecordProjExp().getFieldName();
				pv = rec.getField(projectField);
			}
		} else if(exp.isSetRecordUpdateExp()) {
			PoetsValue.RecV rec = (PoetsValue.RecV) inferEnv(exprMap.get(exp.getRecordUpdateExp().getRecordExp()), env);
			String updateField = exp.getRecordUpdateExp().getFieldName();
			PoetsValue newV = inferEnv(exprMap.get(exp.getRecordUpdateExp().getUpdateExp()), env);
			rec.putField(updateField, newV);
			pv = rec;
		} else if(exp.isSetVariableExp()) {
			// vars are supposed to be replaced when applying LambdaExp
			throw new InferError("Unable to handle Var: " + origExp);
		} else if(exp.isSetLambdaExp()) {
			throw new InferError("Unable to handle Lambda: " + origExp);
		} else if(exp.isSetApplicationExp()
				|| exp.isSetIfThenElseExp() 
				|| exp.isSetCaseExp()) {
			//Exp normExp = normalizeExp(exp);
			//pv = inferEnv(normExp, env);
			//reaching this point means we were unable to normalize the
			//expression to a non-reducible value
			throw new InferError("Non-normalized exp: " + origExp);
		} else if(exp.isSetFunctionExp()) {
			// No corresponding PoetsValue
			throw new InferError("No PoetsValue for FunctionExp" + origExp);
		} else {
			// unknown expression kind
			System.out.println("[RF] unknown expression kind : " + origExp);
			throw new InferError("Unknown expression kind: " + origExp);
		}

		return pv;
	}

	/*** This method tries to infer an environment such that it is
	 * possible to evaluate the given expression to true.
	 */
	public HashMap<String, PoetsValue> inferEnv() 
	throws InferError, InferErrorFieldNotFound, Exception 
	{
		HashMap<String, PoetsValue> env = 
			new HashMap<String, PoetsValue>();
		Exp rootExp = exprMap.get(root);
		try {
			PoetsValue pv = inferEnv(rootExp, env);
			if(pv instanceof PoetsValue.BoolV) {
				if(((PoetsValue.BoolV) pv).val) {
					return env;
				} else {
					throw new InferError("Expression evals to FALSE");				
				}
			} 
			throw new InferError("Expression does not eval to Bool");
		} catch (Exception e) {
			System.out.println("[RF] some error occurred, maybe the returned results are all bad!");
			e.printStackTrace();
			if(env != null && env.size() > 0) {
				return env;
			} else {
				throw e;
			}
		}
	}
}
