package dk.diku.poets.record;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import dk.diku.poets.gen.thrift.value.Record;
import dk.diku.poets.gen.thrift.contracts.Exp;

import dk.diku.poets.gen.thrift.value.Val;
import dk.diku.poets.gen.thrift.value.Value;

@SuppressWarnings("serial")
class ConvertExpException extends Exception {}

public class ExpressionConverter {

	private static void convert(
			Map<Integer, Exp> expMap,
			Map<Integer, Val> valMap,
			Integer index)
		throws ConvertExpException
	{
		Exp exp = expMap.get(index);
		if(exp != null) {
			Val val = new Val();
			// figure out what value is contained and put the value
			// (possibly converted) into the Val
			/** Basic types **/
			if(exp.isSetIntExp()) {
				val.setIntVal(exp.getIntExp());
			} else if(exp.isSetBoolExp()) {
				val.setBoolVal(exp.boolExp); // FIXME: Why is there no setter for boolExp?
			} else if(exp.isSetStringExp()) {
				val.setStringVal(exp.getStringExp());
			} else if(exp.isSetTimeExp()) {
				val.setTimeVal(exp.getTimeExp());
			} else if(exp.isSetDateExp()) {
				val.setDateVal(exp.getDateExp());
			} else if(exp.isSetDateTimeExp()) {
				val.setDateTimeVal(exp.getDateTimeExp());
			} else if(exp.isSetDurationExp()) {
				val.setDurationVal(exp.getDurationExp());
			} else if(exp.isSetRealExp()) {
				val.setRealVal(exp.getRealExp());
			} else /** compond types: list & record **/
			if(exp.isSetRecordExp()) {
				Record record = exp.getRecordExp();
				val.setRecordVal(record);
				Map<String, Integer> fields = record.getFields();
				// also convert field-elements
				for(String fieldName : fields.keySet()) {
					convert(expMap, valMap, fields.get(fieldName));
				}
			} else if(exp.isSetListExps()) {
				List<Integer> listElems = exp.getListExps();
				val.setListVals(listElems);
				// also convert elements of list
				for(Integer listIndex : listElems) {
					convert(expMap, valMap, listIndex);
				}
			} else if(exp.isSetApplicationExp()){
				
			} else {
				// we ran into something which is not (easily) convertible to
				// a Val; it could be a exp-variable or other not 'value'
				// expression.
				System.out.println("[ExpConv] inconvertible Exp: " + exp);
				throw new ConvertExpException();
			}

			valMap.put(index, val);
		} else {
		/*TODO: Consider throwing an Exception here because we encountered
		 * an index that was not mapped in expMap.*/
			System.out.println("[ExpConv] no Exp for index " + index);
			System.out.println("[ExpConv] using expMap: " + expMap);
		}
	}
	
	/**
	 * Convert a thrift <strong>Exp</strong> into a thrift
	 * <strong>Val</strong>. Basically we need to 're-tag' all the 'Exp'
	 * tags into 'Val' tags and produce a new Value with mappings to the
	 * new 'Val'-tagged items.
	 * 
	 * @param expMap Contains a mapping from indices to Exp-objects.
	 * @param expIndex Points to the Exp to convert.  
	 * @return A new Value with root pointing to the converted version
	 * of the Exp pointed to by expIndex and Value.values contains
	 * mappings for all embedded sub-vals of the root-val.
	 */
	public static Value convert(
			Map<Integer, Exp> expMap,
			Integer expIndex)
	{
		Value value = null;
		value = new Value();
		value.values = new HashMap<Integer, Val>();
		value.root = expIndex;
		try { 
			convert(
				expMap, 
				value.values,
				expIndex);
		} catch (ConvertExpException e) {
			System.out.println("[ExpConv] unable to convert Exp["+expIndex+"] using expMap: " + expMap);
			return null;
		}

		return value;
	}

}
