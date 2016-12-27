package dk.diku.poets.record;

import java.util.Set;

import dk.diku.poets.exception.ExceptionHandler;
import dk.diku.poets.exception.PoetsException;
import dk.diku.poets.gen.thrift.PoetsServer;
import dk.diku.poets.gen.thrift.data.RecordDefinition;
import dk.diku.poets.poetsserver.ServerUtils;
import dk.diku.poets.record.PoetsType;
import dk.diku.poets.record.PoetsValue;
/**
 * Class for safely constructing {@link PoetsValue} records.
 * Use setField(fieldName, value) to set values in the record.
 * 
 * This class can also construct an empty record with a complete structure, 
 * but empty values in all leafs
 * 
 * @author jonsson,jespera
 *
 */
public class RecBuilder{
	public RecBuilder(){
		pServer = ServerUtils.getServer();
	}
	public RecBuilder(String name) throws PoetsException{
		this();
		try {
			RecordDefinition ret = RecordDefinitionCache.getRecordDefinition(name);
			this.type = (PoetsType.RecT) RecordDecode.decodeType(ret);
			this.rec = new PoetsValue.RecV(name, type.isAbstract, type.superClasses);
		} catch (Exception e) {
			e.printStackTrace();
			throw ExceptionHandler.exceptionHandler(e);
		}
	}
	private PoetsServer.Iface pServer;
	private PoetsValue.RecV rec;
	private PoetsType.RecT type;
	public RecBuilder setField(String key, PoetsValue value){
		if(type.fields.containsKey(key)){
			if(checkType(type.fields.get(key), value)){
				if(type.fields.get(key).getClass().equals(PoetsType.RecT.class)){
					PoetsType.RecT pr = (PoetsType.RecT) type.fields.get(key);
					if(pr.isAbstract && !pr.rootType()){
						PoetsValue.RecV abstractValue = (PoetsValue.RecV) value.clone();
						abstractValue.name = pr.name;
						abstractValue.isAbstract = true;
						abstractValue.setInstance((PoetsValue.RecV) value);
						rec.val.put(key, abstractValue);
//						System.out.println("[RecBuilder] Abstract record "+abstractValue);
						return this;
					}
				}
				rec.val.put(key, value);
				return this;
			}else{
				System.out.println("[RecBuilder] *** "+value +
						" does not match type of "+key + " in "+type);
			}
		}else{
			System.out.println("[RecBuilder] *** " + type + 
					" does not contain the field "+key);
		}
		return null;
	}
	public RecBuilder setField(String key, String value){
		return setField(key, new PoetsValue.StringV(value));
	}
	public RecBuilder setField(String key, Double value){
		return setField(key, new PoetsValue.DoubleV(value));
	}
	public RecBuilder setField(String key, Integer value){
		return setField(key, new PoetsValue.IntV(value));
	}
	public RecBuilder setField(String key, Calendar value){
		return setField(key, new PoetsValue.DateTimeV(value));
	}
	public RecBuilder setField(String key, Boolean value){
		return setField(key, new PoetsValue.BoolV(value));
	}
	/**
	 * Instantiates the {@link PoetsValue.RecV} defined by the {@link Constructor}
	 *  with the values given in the setFields() method.
	 * 
	 * The returned {@link PoetsValue.RecV} is complete with all fields
	 * filled.
	 */
	public PoetsValue.RecV create(){
		try{
			validate(rec);
			return rec;
		}catch (PoetsException e) {
			System.err.println(e.getDescription());
			e.printStackTrace();
		}
		return null;
	}
	/**
	 * Auto fills the empty fields in the record.
	 * @return A complete instance of the record
	 * @throws PoetsException
	 */
	public PoetsValue getEmptyInstance() throws PoetsException{
		for(String mapKey : type.fields.keySet()){
			if(rec.val.get(mapKey) == null){
				PoetsType key = type.fields.get(mapKey);
				if(key == null){
					System.err.println(mapKey + type.fields);
					return null;
				}
				if(key.isBool()){
					this.setField(mapKey, new PoetsValue.BoolV());
				}
				if(key.isInt()){
					this.setField(mapKey, new PoetsValue.IntV());
				}
				if(key.isDouble()){
					this.setField(mapKey, new PoetsValue.DoubleV());
				}
				if(key.isString()){
					this.setField(mapKey, new PoetsValue.StringV());
				}
				if(key.isDateTime()){
					this.setField(mapKey, new PoetsValue.DateTimeV());
				}
				if(key.isRec()){
					PoetsType.RecT recType = (PoetsType.RecT) key;
					RecBuilder subTypeBuilder = new RecBuilder(recType.name);
					this.setField(mapKey, subTypeBuilder.getEmptyInstance());
				}
				if(key.isRef()){
					PoetsType.RefT subType = (PoetsType.RefT) key;
					//A null value of a pointer is -1
					PoetsValue.RefV newRec = new PoetsValue.RefV(subType.getRefName(), -1);
					this.setField(mapKey, newRec);
				}
				if(key.isList()){
					PoetsType.ListT subType = (PoetsType.ListT) key;
					//TODO Build the list with the right elementType
					PoetsValue elementType = null;
					if(subType.elementType.isBool()){
						elementType = new PoetsValue.BoolV();	
					}else if(subType.elementType.isInt()){
						elementType = new PoetsValue.IntV();
					}else if(subType.elementType.isString()){
						elementType = new PoetsValue.StringV();
					}else if(subType.elementType.isDateTime()){
						elementType = new PoetsValue.DateTimeV();
					}else if(subType.elementType.isDouble()){
						elementType = new PoetsValue.DoubleV();
					}else if(subType.elementType.isRec()){
						PoetsType.RecT subClass= (PoetsType.RecT) subType.elementType;
						RecBuilder recBuilder = new RecBuilder(subClass.name);
						elementType = recBuilder.getEmptyInstance();
					}else if(subType.elementType.isRef()){
						PoetsType.RefT subClass= (PoetsType.RefT) subType.elementType;
						elementType = new PoetsValue.RefV(subClass.getRefName(), -1);
					}else{
						System.err.println("[RecBuilder] ** ElementType of list did not match a type");
					}				
					PoetsValue rec = new PoetsValue.ListV(elementType);
					this.setField(mapKey, rec);
				}
			}
		}
		return rec;
	}
	public void validate(PoetsValue.RecV rec) throws PoetsException{
		for(String key : type.fields.keySet()){
			if(!rec.val.containsKey(key)){
				throw new PoetsException(rec + " SHOULD contain the field "+key+" but does not; type = " + type);
			}
			if(rec.val.get(key).isRef()){
				PoetsValue.RefV ref = (PoetsValue.RefV) rec.val.get(key);
				if(ref.getRefPointer() < 0){
					throw new PoetsException(ref + " has a non-filled pointer to "+key );
				}
			}
		}
	}
	private boolean checkType(PoetsType key, PoetsValue value){
		if(key.getClass().equals(PoetsType.DurationT.class) &&
				value.getClass().equals(PoetsValue.DurationV.class)){
			return true;
		}
		if(key.getClass().equals(PoetsType.BoolT.class) &&
				value.getClass().equals(PoetsValue.BoolV.class)){
			return true;
		}
		if(key.getClass().equals(PoetsType.IntT.class) &&
				value.getClass().equals(PoetsValue.IntV.class)){
			return true;
		}
		if(key.getClass().equals(PoetsType.DoubleT.class) &&
				(value.getClass().equals(PoetsValue.DoubleV.class) ||
				 value.getClass().equals(PoetsValue.IntV.class))){
			return true;
		}
		if(key.getClass().equals(PoetsType.StringT.class) &&
				value.getClass().equals(PoetsValue.StringV.class)){
			return true;
		}
		if(key.getClass().equals(PoetsType.TimeT.class) &&
				value.getClass().equals(PoetsValue.TimeV.class)){
			return true;
		}
		if(key.getClass().equals(PoetsType.DateT.class) &&
				value.getClass().equals(PoetsValue.DateV.class)){
			return true;
		}
		if(key.getClass().equals(PoetsType.DateTimeT.class) &&
				value.getClass().equals(PoetsValue.DateTimeV.class)){
			return true;
		}
		if(key.getClass().equals(PoetsType.RecT.class) &&
				value.getClass().equals(PoetsValue.RecV.class)){
			PoetsType.RecT subType = (PoetsType.RecT) key;
			PoetsValue.RecV subClass = (PoetsValue.RecV) value;
			try{
				Set<String> subTypes = RecordDefinitionCache.getSubTypes(subType.name);
				if(subTypes.contains(subClass.getName()) || 
						subType.name.equals(subClass.getName())){
					return true;
				}
			}catch (Exception e) {
				e.printStackTrace();
			}
		}
		if(key.getClass().equals(PoetsType.RefT.class) &&
				value.getClass().equals(PoetsValue.RefV.class)){
			PoetsType.RefT subType = (PoetsType.RefT) key;
			PoetsValue.RefV subClass = (PoetsValue.RefV) value;
			try{
				Set<String> subTypes = pServer.getSubTypes(subType.getRefName());
				if(subTypes.contains(subClass.getRefName()) ||
						subType.getRefName().equals(subClass.getRefName())){
					return true;
				}
			}catch (Exception e) {
				e.printStackTrace();
			}
		}
		if(key.getClass().equals(PoetsType.ListT.class) &&
				value.getClass().equals(PoetsValue.ListV.class)){
			PoetsType.ListT subType = (PoetsType.ListT) key;
			PoetsValue.ListV subClass = (PoetsValue.ListV) value;
			if(subClass.getElementType() != null){
				return checkType(subType.elementType, subClass.getElementType());
			}else{
				//TODO: include type information.
				return true;
			}
		}
		return false;
	}
}
