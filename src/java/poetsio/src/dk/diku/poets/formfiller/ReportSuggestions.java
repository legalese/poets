package dk.diku.poets.formfiller;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import dk.diku.poets.gen.thrift.PoetsServer;
import dk.diku.poets.gen.thrift.reporting.Report;
import dk.diku.poets.gen.thrift.type.Typ;
import dk.diku.poets.gen.thrift.type.Type;
import dk.diku.poets.gen.thrift.value.Value;
import dk.diku.poets.record.PoetsType;
import dk.diku.poets.record.PoetsValue;
import dk.diku.poets.record.RecordDecode;
import dk.diku.poets.record.RecordDefinitionCache;
import dk.diku.poets.record.RecordEncode;

/**
 * This class provides methods for getting report-functions based on
 * a given result-type.
 * 
 * @author jonsson, jespera
 */
public class ReportSuggestions {

	private static String embedsType(String targetRecord, Type type, 
			PoetsServer.Iface pServer) {
		PoetsType reportType = getRecordType(type, pServer);
		if(reportType == null) {
			return null;
		}

		if(reportType.isRec()){
			PoetsType.RecT rt = (PoetsType.RecT) reportType;
			for(String key : rt.fields.keySet()){
				if(rt.fields.get(key).isList()){
					PoetsType.ListT list = (PoetsType.ListT) rt.fields.get(key);
					if(list.elementType.isRec()){						
						PoetsType.RecT rec = (PoetsType.RecT) list.elementType;
						Set<String> subTypes = RecordDefinitionCache.getSubTypes(targetRecord);
						if(subTypes.contains(rec.name) || 
								rec.name.equals(targetRecord)){
							System.out.println("[ReportSuggestions] report returns a list of record: "+key);
							return key;
						}
					}
				}
			}
		}
		return null;
	}
	/**
	 * Tests whether the type contains a list of records with two 
	 * fields, where:
	 * field 1 is a reference pointer (int)
	 * field 2 is (a sub type of) targetRecord 
	 * 
	 * @param targetRecord The target record
	 * @param type The type of the report 
	 * @param pServer A POETS server
	 * @return the field name where the embedded type is located.
	 */
	public static String embedsRefType(
			String targetRecord,
			Type type,
			PoetsServer.Iface pServer){
		PoetsType reportType = getRecordType(type, pServer);
		if(reportType.isRec()){
			PoetsType.RecT recordType = (PoetsType.RecT) reportType;
			// There is only one field, and that is a list of records
			if(recordType.fields.size() == 1 &&
					recordType.fields.values().iterator().next().isList()){
				String fieldName = recordType.fields.keySet().iterator().next();
				PoetsType.ListT entityList = (PoetsType.ListT) recordType.fields.values().iterator().next();
				if(entityList.elementType.isRec()){
					PoetsType.RecT entity = (PoetsType.RecT) entityList.elementType;	
					//The record has exactly 2 fields.
					if(entity.fields.size() == 2){
						for(String subFieldName : entity.fields.keySet()){
							PoetsType entitySub = entity.fields.get(subFieldName);
							//One of the fields is (a sub type of) the target record.
							if(entitySub.isRec()){
								Set<String> subTypes = RecordDefinitionCache.getSubTypes(targetRecord);
								if(((PoetsType.RecT)entitySub).name.equals(targetRecord) || 
										subTypes.contains(((PoetsType.RecT)entitySub).name)){
									System.out.println("[ReportSuggestions] report returns a list of record references: "+fieldName);
									return fieldName;
								}
							}
						}
					}
				}
			}
		}
		return null;
	}

	private static boolean returnsDirect(
			String targetRecord,
			Type reportType,
			PoetsServer.Iface pServer) {

		Typ typ = reportType.types.get(reportType.root);
		if(typ.isSetTypeRecord()) {
			// the report returns a record value
			String reportRecordName = typ.getTypeRecord();
			Set<String> subTypes = RecordDefinitionCache.getSubTypes(reportRecordName);
			// is it either directly of the targetType or does it contain
			if(subTypes.contains(targetRecord) || 
					reportRecordName.equals(targetRecord)) {
				System.out.println("[ReportSuggestions] report returns the value directly");
				return true;
			}
		}

		return false;
	}

	private static PoetsType getRecordType(Type recordType, PoetsServer.Iface pServer) {
		Typ typ = recordType.types.get(recordType.root);
		if(typ.isSetTypeRecord()) {
			String reportRecordName = typ.getTypeRecord();
			PoetsType ret = 
				RecordDecode.decodeType(RecordDefinitionCache.getRecordDefinition(reportRecordName));
//			System.out.println(ret.toString());
			return ret;
		}
		return null;
	}

	private static boolean includeType(String targetRecord, Type reportType, 
			PoetsServer.Iface pServer) {
		return 
		returnsDirect(targetRecord, reportType, pServer) 
		|| embedsType(targetRecord, reportType, pServer) != null
		|| embedsRefType(targetRecord, reportType, pServer) != null;
	}
	/**
	 * Method to find a list of reports that can return a value
	 * (potentially embedded) of a given type.
	 * 
	 * @param targetRecord The record-type that one wishes to get reports
	 * for.
	 *
	 * @return A list of 'Report' thrift structures from which one can see
	 * all information about the reports that can return a value of the
	 * expected type given by @recordName. 
	 */
	public static List<Report> getReportSuggestions(String targetRecord, PoetsServer.Iface pServer) {
		List<Report> returnedReports = new ArrayList<Report>();
		try {
			// get list of all reports 
			//Set<String> reportNames = pServer.getReportNames();
			final PoetsValue.ListV emptyList =
				new PoetsValue.ListV(new PoetsValue.StringV(""));
			List<PoetsValue> reportNames =
				((PoetsValue.ListV)
				RecordDecode.decodeValue(
				pServer.queryReport("ReportNamesByTags", 
						new ArrayList<Value>(),
						new ArrayList<Value>() {{
							add(RecordEncode.encodeValue(emptyList));
							add(RecordEncode.encodeValue(emptyList));
						}}
				))).val;
			for(PoetsValue reportName : reportNames) {
				if(reportName instanceof PoetsValue.StringV) {
					// for each:
					//   get the meta-data (Report)
					Report repMeta = pServer.getReport(((PoetsValue.StringV)reportName).val);
					//   does the Report have the correct return type?
					Type type = repMeta.type.returnType;
					if(type != null) {
	//					System.out.println("[ReportSuggestions] Testing "+reportName);

						if(includeType(targetRecord, type, pServer)){
							returnedReports.add(repMeta);
						}
					}
				}
			}
		} catch (Exception e) {
			System.out.println("[ReportSuggestions] ** runtime error");
			e.printStackTrace();
			throw new RuntimeException(e);
		}
		return returnedReports;
	}
	private static ArrayList<PoetsValue> extractEmbeddedVals(
			String targetRecord,
			String fieldName,
			PoetsValue reportResult) {

		ArrayList<PoetsValue> returnedValues = null;

		// find the only field of the reportResult and return the list it
		// contains
		PoetsValue.RecV rec = (PoetsValue.RecV) reportResult;
		if(rec.getField(fieldName).isList()){
			PoetsValue.ListV recordList = (PoetsValue.ListV) rec.getField(fieldName);

			if(recordList.val.size() >= 0) {
				returnedValues = (ArrayList<PoetsValue>) recordList.val;
			} else {
				returnedValues = new ArrayList<PoetsValue>();
			}
			return returnedValues;
		}
		return null;
	}

	/**
	 * Query the given server for the report given as argument and
	 * extract all the values (according to how the report was
	 * determined by 'getReportSuggestions') into a list.
	 * 
	 * @param report
	 * @param targetRecord
	 * @param pServer
	 * @return
	 */
	public static List<PoetsValue> getReportValues(
			Report report, 
			List<Value> args,
			String targetRecord, 
			PoetsServer.Iface pServer) {

		try {
			Value reportValue = pServer.queryReport(
					report.getName(),
					new ArrayList<Value>(),
					args);
			System.out.println("[ReportSuggestions] received report value: " + reportValue);
			// test: should we return the decoded value directly or should
			// we extract a list?
			if(returnsDirect(targetRecord, report.getType().getReturnType(), pServer)) {
				ArrayList<PoetsValue> reportResults = new ArrayList<PoetsValue>();
//				System.out.println("[ReportSuggestions] returnsDirect");
				PoetsValue rec = RecordDecode.decodeValue(reportValue);
				reportResults.add(rec);
				return reportResults; 
			}
			//The field name for storing the found field containing the right type
			String fieldName = embedsType(targetRecord, report.getType().getReturnType(), pServer);
			if(fieldName != null) {
//				System.out.println("[ReportSuggestions] embedsType. Field "+fieldName);
				return extractEmbeddedVals(
						targetRecord, 
						fieldName,
						RecordDecode.decodeValue(reportValue));
			}
			String fieldRefName = embedsRefType(targetRecord, report.getType().getReturnType(), pServer); 
			if(fieldRefName != null){
//				System.out.println("[ReportSuggestions] embedsRefType. Field "+fieldRefName);
				return extractEmbeddedVals(
						targetRecord, 
						fieldRefName, 
						RecordDecode.decodeValue(reportValue));
			}
			System.out.println("[ReportSuggestions] ???");
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
}

