/**
 * 
 */
package dk.diku.poets.record;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import dk.diku.poets.gen.thrift.data.RecordDefinition;
import dk.diku.poets.poetsserver.ServerUtils;

/**
 * Wrapper for the getRecordDefinition Thrift method. This 
 * class caches record definitions that have already been
 * fetched from the Poets server.
 * 
 * @author jonsson
 *
 */
public class RecordDefinitionCache {
	private static HashMap<String, RecordDefinition> recDefinitions;
	private static HashMap<String, Set<String>> subTypes;

	/**
	 * 
	 * @param recordName 
	 * @return The {@link RecordDefinition} for recordName
	 */
	public static RecordDefinition getRecordDefinition(String recordName){
		if(recDefinitions == null){
			recDefinitions = new HashMap<String, RecordDefinition>();
		}
		if(recDefinitions.containsKey(recordName) &&
			 recDefinitions.get(recordName) != null){
			return recDefinitions.get(recordName);
		}else{
			try {
				RecordDefinition r = ServerUtils.getServer().getRecordDefinition(recordName);
				recDefinitions.put(recordName, r);
				return r;
			} catch (Exception e) {
				System.err.println("[POETS] *** getRecordDefinition failed for "+recordName);
				e.printStackTrace();
			}
		}
		return null;
	}
	/**
	 * 
	 * @param recordName
	 * @return The set of sub type names for recordName  or the empty set of 
	 * none exists
	 */
	public static Set<String> getSubTypes(String recordName){
		if(subTypes == null){
			subTypes = new HashMap<String, Set<String>>();
		}
		if(subTypes.containsKey(recordName)){
			return subTypes.get(recordName);
		}else{
			try{
				Set<String> types = ServerUtils.getServer().getSubTypes(recordName);
				subTypes.put(recordName, types);
				return types;
			}catch (Exception e) {
				System.err.println("[POETS] *** getSubTypes failed for "+recordName);
				e.printStackTrace();
			}
		}
		return new HashSet<String>();
	}
}
