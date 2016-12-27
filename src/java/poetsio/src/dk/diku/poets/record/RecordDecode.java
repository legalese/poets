package dk.diku.poets.record;

import dk.diku.poets.gen.thrift.data.BasicRecordAttribute;
import dk.diku.poets.gen.thrift.data.RecordAttribute;
import dk.diku.poets.gen.thrift.data.RecordDefinition;

import dk.diku.poets.gen.thrift.type.Typ;
import dk.diku.poets.gen.thrift.type.Type;
import dk.diku.poets.gen.thrift.type.TypeConstant;

import dk.diku.poets.gen.thrift.value.Date;
import dk.diku.poets.gen.thrift.value.DateTime;
import dk.diku.poets.gen.thrift.value.Time;
import dk.diku.poets.gen.thrift.value.Val;
import dk.diku.poets.gen.thrift.value.Value;

import dk.diku.poets.record.Calendar;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.ListV;

/**
 * Decodes Thrift values and types into {@link PoetsValue} and {@link PoetsType} 
 * 
 * @author jonsson,jespera
 *
 */
public class RecordDecode {
	public static PoetsValue decodeValue(Value value){
		int rootIndex = value.getRoot();
		return _decodeValue(value, rootIndex);
	}
	private static PoetsValue _decodeValue(Value value, int rootIndex){
		if(value.values == null) {
			System.out.println("[Decode] ** null-values");
			throw new RuntimeException();
		}
		Val rootVal = value.values.get(rootIndex);
		if(rootVal == null) {
			System.out.println("[Decode] ** null-root value");
			throw new RuntimeException();
		}

		if(rootVal.isSetIntVal()) {
			return new PoetsValue.IntV(rootVal.intVal); 
		} else if(rootVal.isSetBoolVal()) {
			return new PoetsValue.BoolV(rootVal.boolVal);
		} else if(rootVal.isSetStringVal()) {
			return new PoetsValue.StringV(rootVal.stringVal);
		} else if(rootVal.isSetTimeVal()) {
			return new PoetsValue.TimeV(decodeCalendar(rootVal.getTimeVal()));
		} else if(rootVal.isSetDateVal()) {
			return new PoetsValue.DateV(decodeCalendar(rootVal.getDateVal()));
		} else if(rootVal.isSetDateTimeVal()) {
			return new PoetsValue.DateTimeV(decodeCalendar(rootVal.getDateTimeVal()));
		} else if(rootVal.isSetDurationVal()) {
			System.out.println("[Decode] decoding duration: " + rootVal.getDurationVal());
			return new PoetsValue.DurationV(rootVal.getDurationVal());
		} else if(rootVal.isSetRealVal()) {
			return new PoetsValue.DoubleV(rootVal.getRealVal());
		} else if(rootVal.isSetRecordVal()) {
//			System.out.println("[Decode] isRec >> " + rootVal);
			dk.diku.poets.gen.thrift.value.Record recVal = rootVal.recordVal;
			try{
				RecBuilder builder = new RecBuilder(recVal.recordName);
				RecordDefinition def = RecordDefinitionCache.getRecordDefinition(recVal.recordName);
				for(String key : recVal.fields.keySet()){
					int idx = recVal.fields.get(key);
					int rootIdx = def.fieldsDefinitions.get(key).getFieldType().getRoot();
					Typ typ = def.fieldsDefinitions.get(key).getFieldType().getTypes().get(rootIdx); 
					PoetsValue decodedValue;
					if(typ.isSetTypeList()){
						Typ t = def.fieldsDefinitions.get(key).getFieldType().getTypes().get(typ.typeList);
						decodedValue = decodeValueList(value, idx, decodeType(t,def.fieldsDefinitions.get(key).getFieldType()));
					}else{
						decodedValue = _decodeValue(value, idx);
					}
					builder.setField(key, decodedValue);
				}
				return builder.create();
			}catch (Exception e) {
				e.printStackTrace();
				return null;
			}
		} else if(rootVal.isSetEntVal()){
//			System.out.println("[Decode] isRef >> " + rootVal);
			if(!rootVal.getEntVal().isSetEntPointer()){
				System.err.println("[RecordDecode] *** Pointer not set");
			}
			return new PoetsValue.RefV(rootVal.entVal.recordName, rootVal.entVal.entPointer);

		} else if(rootVal.isSetListVals()) {
			// decode top-level list; we need to figure out the type of the
			// elements in the list; if the list is empty, we've lost
			// because the type is not included in the list;
			// as a HACK until a way to obtain the type of the elements
			// becomes available, we look for the first element and extract
			// the type based on that instead
			if(rootVal.getListVals().size() > 0) {
				int firstElemIndex = rootVal.getListVals().get(0);
				return decodeValueList(value, rootIndex, _decodeValue(value, firstElemIndex).toType());
			} else {
				return new ListV();
			}
		}else{
			System.out.println("[Decode] *** ERROR >> " + rootVal);
			return null;
		}
	}
	private static PoetsValue decodeValueList(Value value, int idx, PoetsType type) {
		Val rootVal = value.values.get(idx);
		try{
			if(rootVal.isSetListVals()){
//				System.out.println("[Decode] isList >> " + rootVal + " of type "+type);
				PoetsValue.ListV newRec = new PoetsValue.ListV();
				if(type.getClass().equals(PoetsType.RecT.class)){
					newRec.elementType = (new RecBuilder(((PoetsType.RecT)type).name)).getEmptyInstance();
				}else if(type.getClass().equals(PoetsType.RefT.class)){
					// We rely on the 'elementType' field only being used as a
					// type, so it should not matter that we put in id = -1
					// TODO: elementType should be a PoetsType and not
					// PoetsValue!
					newRec.elementType = new PoetsValue.RefV(((PoetsType.RefT)type).getRefName(), -1);
				}else if(type.getClass().equals(PoetsType.ListT.class)){
					System.err.println("[RecordDecode] *** Cannot decode lists of lists.");
				}else if(type.getClass().equals(PoetsType.BoolT.class)){
					newRec.elementType = new PoetsValue.BoolV();
				}else if(type.getClass().equals(PoetsType.IntT.class)){
					newRec.elementType = new PoetsValue.IntV();
				}else if(type.getClass().equals(PoetsType.DoubleT.class)){
					newRec.elementType = new PoetsValue.DoubleV();
				}else if(type.getClass().equals(PoetsType.DateTimeT.class)){
					newRec.elementType = new PoetsValue.DateTimeV();
				}else if(type.getClass().equals(PoetsType.TimeT.class)){
					newRec.elementType = new PoetsValue.TimeV();
				}else if(type.getClass().equals(PoetsType.DateT.class)){
					newRec.elementType = new PoetsValue.DateV();
				}else if(type.getClass().equals(PoetsType.StringT.class)){
					newRec.elementType = new PoetsValue.StringV();
				}
				for(int v : rootVal.listVals){
					PoetsValue decodedValue = _decodeValue(value, v);
					if(decodedValue.isRec() &&
							!((PoetsValue.RecV)decodedValue).getName().equals(((PoetsType.RecT)type).name) &&
							((PoetsType.RecT)type).isAbstract && 
							!((PoetsType.RecT)type).rootType()){
						//Special case for lists of abstract records
						PoetsValue.RecV abstractRec = 
							(PoetsValue.RecV) (new RecBuilder(((PoetsType.RecT)type).name)).getEmptyInstance();
						abstractRec.setInstance((PoetsValue.RecV) decodedValue);
						abstractRec.isAbstract = true;

						newRec.addElement(abstractRec);
					}else{
						newRec.addElement(decodedValue);
					}
				}
				return newRec;
			}
		}catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	public static boolean isAbstract(RecordDefinition rdef) {
		for(RecordAttribute rattr : rdef.getRecordAttributes()) {
			if(rattr.getBasicAttr() == BasicRecordAttribute.RecordIsAbstract) {
				return true;
			}
		}
		return false;
	}

	public static Calendar decodeCalendar(Time timeVal) {
		//convert to calendar
		Calendar cal = Calendar.getInstance();
		cal.clear();
		//account for dates sent being UTC+1 (Denmark, no DST/wintertime)
		cal.set(Calendar.HOUR, timeVal.getHour() + 1);
		cal.set(Calendar.MINUTE, timeVal.getMinute());
		cal.set(Calendar.SECOND, timeVal.getSecond());
		return cal;
	}
	public static Calendar decodeCalendar(Date dateVal) {
		//convert to calendar
		Calendar cal = Calendar.getInstance();
		cal.clear();
		cal.set(Calendar.YEAR, dateVal.getYear());
		cal.set(Calendar.MONTH, dateVal.getMonth()); 
		cal.set(Calendar.DAY_OF_MONTH, dateVal.getDay());
		return cal;
	}
	public static Calendar decodeCalendar(DateTime dateTimeVal) {
		//convert to calendar
		Calendar cal = Calendar.getInstance();
		cal.clear();
		cal.set(Calendar.YEAR, dateTimeVal.getDate().getYear());
		cal.set(Calendar.MONTH, dateTimeVal.getDate().getMonth()); 
		cal.set(Calendar.DAY_OF_MONTH, dateTimeVal.getDate().getDay());
		//account for dates sent being UTC+1 (Denmark, no DST/wintertime)
		cal.set(Calendar.HOUR, dateTimeVal.getTime().getHour() + 1);
		cal.set(Calendar.MINUTE, dateTimeVal.getTime().getMinute());
		cal.set(Calendar.SECOND, dateTimeVal.getTime().getSecond());
		return cal;
	}
	public static PoetsType decodeType(RecordDefinition recordDefinition) {
		PoetsType.RecT newRecType = new PoetsType.RecT();
		newRecType.name = recordDefinition.recordName;
		newRecType.isAbstract = isAbstract(recordDefinition);
		newRecType.superClasses = recordDefinition.superClasses;
		for(String key : recordDefinition.fieldsDefinitions.keySet()){
			Type recordT = recordDefinition.fieldsDefinitions.get(key).getFieldType();
			newRecType.fields.put(key, decodeType(recordT));
		}
		return newRecType;
	}
	public static PoetsType decodeType(Type recordT){
		Typ typ = recordT.types.get(recordT.root);
		return decodeType(typ, recordT);
	}
	private static PoetsType decodeType(Typ typ, Type recordT){
		if(typ.isSetTypeConstant()){
			TypeConstant typeC = TypeConstant.findByValue(typ.getTypeConstant().getValue());
//			System.out.print("[RecordDecode] Constant: ");
			if(typeC.equals(TypeConstant.Bool)){
//				System.out.println("Bool");
				return new PoetsType.BoolT();
			}else if(typeC.equals(TypeConstant.Int)){
//				System.out.println("Int");
				return new PoetsType.IntT();
			}else if(typeC.equals(TypeConstant.Real)){
//				System.out.println("Double");
				return new PoetsType.DoubleT();
			}else if(typeC.equals(TypeConstant.String)){
//				System.out.println("String");
				return new PoetsType.StringT();
			}else if(typeC.equals(TypeConstant.Time)){
//				System.out.println("DateTime");
				return new PoetsType.TimeT();
			}else if(typeC.equals(TypeConstant.Date)){
//				System.out.println("DateTime");
				return new PoetsType.DateT();
			}else if(typeC.equals(TypeConstant.DateTime)){
//				System.out.println("DateTime");
				return new PoetsType.DateTimeT();
			}else if(typeC.equals(TypeConstant.Duration)){
//				System.out.println("Duration");
				return new PoetsType.DurationT();
			}
		}else if(typ.isSetTypeRecord()){
//			System.out.println("[RecordDecode] Record "+typ.getTypeRecord());
			try{
				RecordDefinition def = RecordDefinitionCache.getRecordDefinition(typ.getTypeRecord());
				PoetsType.RecT newT = new PoetsType.RecT(typ.getTypeRecord());
				newT.isAbstract = isAbstract(def);
				newT.superClasses = def.superClasses;
				for(String fieldName : def.fieldsDefinitions.keySet()){
					newT.fields.put(fieldName, decodeType(def.fieldsDefinitions.get(fieldName).getFieldType()));
				}
				return newT;
			}catch (Exception e) {
				e.printStackTrace();
				return null;
			}
		}else if(typ.isSetTypeEntity()){
//			System.out.println("[RecordDecode] Reference "+typ.getTypeRef());
			return new PoetsType.RefT(typ.getTypeEntity());
		}else if(typ.isSetTypeList()){
			PoetsType.ListT newRecType = new PoetsType.ListT();
			newRecType.elementType = decodeType(recordT.types.get(typ.getTypeList()), recordT);
//			System.out.println("[RecordDecode] List "+newRecType.elementType);

			return newRecType;
		}
		return null;
	}
}
