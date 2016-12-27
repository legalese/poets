package dk.diku.poets.pll;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import dk.diku.poets.gen.thrift.data.RecordDefinition;
import dk.diku.poets.pll.Layout;
import dk.diku.poets.pll.Layout.Field;
import dk.diku.poets.poetsserver.ServerUtils;
import dk.diku.poets.record.RecordDefinitionCache;

public class LayoutEnv {
	private Map<String, Layout> map;

	public LayoutEnv(String jsonStr) {
		map = parse(jsonStr);
	}

	/***
	 * This methods provides a way to parse a layout-spec from a string.
	 * 
	 * The result (if parsing goods good) is a map from recordnames to
	 * layouts.
	 *
	 * A layout-spec is a json encoded string which contains all the
	 * layout definitions for each ontology entity
	 *
	 * <pre>
	 * layout-spec ::= { layout-entry+ }
	 * layout-entry::= 
	 * RecordName : { 
	 *   title : "string", 
	 *   header : [field-spec*],
	 *   body   : [field-spec*],
	 *   footer : [field-spec*]
	 *}
	 * field-spec ::= { "fieldName" : "stringlabel" }
	 * </pre>
	 */
	private static Map<String, Layout> parse(String layoutSpec) {
		HashMap<String, Layout> map = 
			new HashMap<String, Layout>();

		try {
			JSONObject jsonObj = new JSONObject(layoutSpec);

			@SuppressWarnings("unchecked")
			Iterator<String> it = (Iterator<String>) jsonObj.keys();
			while(it.hasNext()) {
				String recordName = (String) it.next();
				JSONObject jsonRecObj = jsonObj.getJSONObject(recordName);
				String title = jsonRecObj.getString("title");

				// alsp check that all referenced fields are actually defined
				// in the corresponding ontology entity
				List<Field> header = getFieldList(jsonRecObj.getJSONArray("header"));
				if(!checkFields(header, recordName)) {
					throw new RuntimeException("Some header fields not found in ontology");
				}
				List<Field> body = getFieldList(jsonRecObj.getJSONArray("body"));
				if(!checkFields(body, recordName)) {
					throw new RuntimeException("Some body fields not found in ontology");
				}
				List<Field> footer = getFieldList(jsonRecObj.getJSONArray("footer"));
				if(!checkFields(footer, recordName)) {
					throw new RuntimeException("Some footer fields not found in ontology");
				}

				Layout mLayout = new Layout();
				mLayout.setRecordName(recordName);
				mLayout.setTitle(title);
				mLayout.setHeader(header);
				mLayout.setBody(body);
				mLayout.setFooter(footer);

				map.put(recordName, mLayout);

			}
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}

		return map;
	}

	//	private static boolean checkFields(List<Field> fields, String recordName) {
	//		try {
	//			Class<?> recClass = Class.forName("dk.diku.poets.record.gen." + recordName);
	//			for(Field field : fields) {
	//				try {
	//					java.lang.reflect.Field javaField = recClass.getField(field.getName());
	//					continue;
	//				} catch (NoSuchFieldException e) {
	//					throw new RuntimeException("Field from layout not found in ontology: " + field.getName());
	//				}
	//			}
	//		}	catch (ClassNotFoundException e) {
	//			throw new RuntimeException("Record from layout not defined in ontology: " + recordName);
	//		}
	//		return true;
	//	}
	private static boolean checkFields(List<Field> fields, String recordName){
		try{
			RecordDefinition recordDefinition = RecordDefinitionCache.getRecordDefinition(recordName);
			for(Field field : fields){
				if(!recordDefinition.fieldsDefinitions.containsKey(field.getName())){
					throw new RuntimeException("Field from layout not found in ontology: " + field.getName());
				}
			}
		}catch (Exception e) {
			throw new RuntimeException("Record from layout not defined in ontology: " + recordName);
		}
		return true;
	}

	private static Field getFieldFromJSON(JSONObject jsonObj) {
		Field retField = new Field();

		Iterator<String> it = (Iterator<String>) jsonObj.keys();
		while(it.hasNext()) {
			String fieldName = (String) it.next();
			retField.setName(fieldName);
			try {
				retField.setLabel(jsonObj.getString(fieldName));
			} catch (JSONException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		return retField;
	}
	private static List<Field> getFieldList(JSONArray arr) {
		List<Field> retList = new ArrayList<Field>();

		for(int i = 0; i < arr.length(); i++) {
			try {
				retList.add(getFieldFromJSON(arr.getJSONObject(i)));
			} catch (JSONException e) {
				// TODO Auto-generated catch block
				//e.printStackTrace();
				System.out.println("[LayoutEnv] failed to get fieldlist from " + arr.toString());
			}
		}
		return retList;
	}

	private Layout getLayout(String recordName) {
		return map.get(recordName);
	}

	/***
	 * This method takes a recorddefinition and returns a Layout
	 * instance.
	 *
	 * The method first tries to see if there is an entry for the
	 * record in the map. Then it traverses the class-hierarchy of the
	 * record and fills in missing fields from the super-classes.
	 * Fieldnames that have already been mapped in sub-classes are
	 * <strong>not</strong> overwritten.
	 *
	 * Fieldnames from superclasses are added <strong>after</strong>
	 * fieldnames that are already mapped in the resulting Layout.
	 */
	public Layout getLayout(RecordDefinition recordDefinition){
		String recordName = recordDefinition.getRecordName();
		Layout baseLayout = getLayout(recordName);
		if (baseLayout == null) {
                    return null;
		}
		try{
                    Set<String> sups = recordDefinition.getSuperClasses();
                    for(String sup: sups) {
                        Layout superLayout = getLayout(RecordDefinitionCache.getRecordDefinition(sup));
                        if(superLayout != null){
                            System.out.println("[LayoutEnv] merging into: " + recordName + " from: " + sup);
                            baseLayout.mergeLayout(superLayout);
                        }
                    }
		}catch (Exception e) {
			System.out.println("[LayoutEnv] *** Could not retrieve layout for "+recordName);
			e.printStackTrace();
		}
		return baseLayout;
	}
}
