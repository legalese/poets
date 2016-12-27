package dk.diku.poets.exception;

import org.apache.thrift.TException;
import org.apache.thrift.transport.TTransportException;


import dk.diku.poets.gen.thrift.contracts.ContractDefExistsException;
import dk.diku.poets.gen.thrift.contracts.ContractDefNotFoundException;
import dk.diku.poets.gen.thrift.contracts.ContractNotConcludable;
import dk.diku.poets.gen.thrift.contracts.ContractNotFoundException;
import dk.diku.poets.gen.thrift.contracts.GuardednessException;
import dk.diku.poets.gen.thrift.contracts.ParseException;
import dk.diku.poets.gen.thrift.contracts.TemplateNotFoundException;
import dk.diku.poets.gen.thrift.contracts.TemplateTypeMismatchException;
import dk.diku.poets.gen.thrift.contracts.TransactionTooOldException;
import dk.diku.poets.gen.thrift.contracts.UnexpectedTransaction;
import dk.diku.poets.gen.thrift.reporting.ReportInitException;
import dk.diku.poets.gen.thrift.reporting.ReportNotFoundException;
import dk.diku.poets.gen.thrift.value.TypeException;
import dk.diku.poets.record.RecordDecode;
import dk.diku.poets.synchronizedfactory.SynchronizedFactory;
/**
 * Handler class for exception thrown in the communication
 * with the Poets server. This is needed because the thrift communication
 * is wrapped in {@link SynchronizedFactory} class.
 * @author jonsson
 *
 */
public class ExceptionHandler {
	@SuppressWarnings("unchecked")
	public static PoetsException exceptionHandler(Throwable caught){
		System.err.println("[Exception] *** ");
		caught.printStackTrace();
		if(caught == null || caught.getCause() == null){
			return new PoetsException(caught.toString());
		}
		Throwable e = caught.getCause().getCause(); 
		if(e != null){
			Class exceptionClass = e.getClass();
			System.out.println("[POETS] *** "+e.toString());
			// Contract exceptions
			if(exceptionClass.equals(ContractNotConcludable.class)){
				ContractNotConcludable c = (ContractNotConcludable) e;
				return new PoetsException("Contract "+c.contractId +" not concludable");
			}else if(exceptionClass.equals(ContractNotFoundException.class)){
				ContractNotFoundException c = (ContractNotFoundException) e;
				return new PoetsException("Contract "+c.contractId + " not found");
			}else if(exceptionClass.equals(TransactionTooOldException.class)){
				TransactionTooOldException c = (TransactionTooOldException) e;
				return new PoetsException("Transaction to old for "+c.contractId);
			}else if(exceptionClass.equals(UnexpectedTransaction.class)){
				UnexpectedTransaction c = (UnexpectedTransaction) e;
				return new PoetsException("Unexpected transaction: "+
						RecordDecode.decodeValue(c.transaction));
			}else if(exceptionClass.equals(TemplateNotFoundException.class)){
				TemplateNotFoundException c = (TemplateNotFoundException) e;
				return new PoetsException("Template" +c.templateName + " not found");
			}else if(exceptionClass.equals(TemplateTypeMismatchException.class)){
				TemplateTypeMismatchException c = (TemplateTypeMismatchException) e;
				return new PoetsException(c.templateName+ "not of type "+c.templateType);
			}else if(exceptionClass.equals(ParseException.class)){
				ParseException c = (ParseException) e;
				return new PoetsException(c.err);
			}else if(exceptionClass.equals(ContractDefExistsException.class)){
				ContractDefExistsException c = (ContractDefExistsException) e;
				return new PoetsException("Contract "+c.name+" already exists");
			}else if(exceptionClass.equals(ContractDefNotFoundException.class)){
				ContractDefNotFoundException c = (ContractDefNotFoundException) e;
				return new PoetsException("Contract definition "+c.name+" does not exists");
			}else if(exceptionClass.equals(GuardednessException.class)){
				GuardednessException c = (GuardednessException) e;
				return new PoetsException("The clause definition "+c.err+" is not guarded");
			}
			// Reporting exceptions
			else if(exceptionClass.equals(ReportInitException.class)){
				ReportInitException c = (ReportInitException) e;
				return new PoetsException(c.errorMsg);
			}else if(exceptionClass.equals(ReportNotFoundException.class)){
				ReportNotFoundException c = (ReportNotFoundException) e;
				return new PoetsException("Report not found: "+c.getMessage());
			// General Poets exceptions
			}else if(exceptionClass.equals(TException.class)){
				TException c = (TException) e;
				return new PoetsException("Connection to the server failed: "+c.getMessage());
			}else if(exceptionClass.equals(TTransportException.class)){
				TTransportException c = (TTransportException) e;
				return new PoetsException("Connection to the server failed. "+c.getMessage());
			}else if(exceptionClass.equals(TypeException.class)){
				TypeException c = (TypeException) e;
				return new PoetsException(c.errorMsg);
			}
			//Uncaught wrapped exceptions
			caught.getCause().getCause().printStackTrace();
			return new PoetsException("Uncaught exception "+
					caught.getCause().getCause().toString());
		}
		// Uncaught exceptions. This should not happen.
		caught.printStackTrace();
		return new PoetsException("Uncaught exception ");
	}

}
