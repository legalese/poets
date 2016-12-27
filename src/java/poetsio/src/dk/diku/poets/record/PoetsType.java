package dk.diku.poets.record;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Set;
/**
 * Java representation of the Thrift Types.
 * 
 * @author jonsson
 *
 */
@SuppressWarnings("serial")
public abstract class PoetsType  implements Serializable{
	public static class DurationT extends PoetsType implements Serializable{
		public DurationT(){}
	}
	public static class StringT extends PoetsType implements Serializable{
		public StringT(){}
	};
	public static class IntT extends PoetsType implements Serializable{
		public IntT(){}
	};
	public static class BoolT extends PoetsType implements Serializable{
		public BoolT(){}
	};
	public static class DoubleT extends PoetsType implements Serializable{
		public DoubleT(){}
	};
	public static class DateTimeT extends PoetsType implements Serializable{
		public DateTimeT(){}
	};
	public static class DateT extends PoetsType implements Serializable {
		public DateT(){}
	};
	public static class TimeT extends PoetsType implements Serializable {
		public TimeT(){}
	};
	public static class RecT extends PoetsType implements Serializable{
		private static final String[] RootTypes = new String[]{
			"Contract",
			"Event",
			"Data",
			"Transaction",
			"Report",
			"PutContract"
		};
		public RecT(){
			fields = new HashMap<String, PoetsType>();
			isAbstract = false;
		}
		public RecT(String name){
			this();
			this.name = name;
		}
		public String name;
		public HashMap<String, PoetsType> fields;
		public boolean isAbstract;
		public Set<String> superClasses;
		public boolean rootType() {
			for(int i = 0; i < RootTypes.length; i++){
				if(RootTypes[i].equals(this.name)){
					return true;
				}
			}
			return false;
		}
	};
	public static class RefT extends PoetsType implements Serializable{
		public RefT(){}
		public RefT(String name){
			this.name = name;
		}
		private String name;
		public String getRefName(){
			return name;
		}
	};
	public static class ListT extends PoetsType implements Serializable{
		public ListT(){}
		public PoetsType elementType;
	};
	@Override
	public String toString(){
		Class<? extends PoetsType> cName = this.getClass();
		if(cName == RecT.class){
			RecT r = (RecT) this;
			StringBuffer ret = new StringBuffer(r.name + (r.isAbstract?"A":"").toString() + "{");
			for(String key : r.fields.keySet()){
				ret.append(key + ": " + r.fields.get(key).toString() +", ");
			}
			ret.append("}");
			return ret.toString();
		}else if(cName == IntT.class){
			return "Int";
		}else if(cName == StringT.class){
			return "String";
		}else if(cName == DoubleT.class){
			return "Double";
		}else if(cName == DateTimeT.class){
			return "DateTime";
		}else if(cName == DateT.class){
			return "Date";
		}else if(cName == TimeT.class) {
			return "Time";
		}else if(cName == BoolT.class){
			return "Bool";
		}else if(cName == RefT.class){
			RefT r = (RefT) this;
			return "(* "+r.getRefName()+" *)";
		}else if(cName == ListT.class){
			ListT r = (ListT) this;
			String ret = "[";
			ret += r.elementType.toString();
			ret += "]";
			return ret;
		}else if(cName == DurationT.class){
			return "Duration";
		}else{
			System.err.println("Unrecognized PoetsValue ");
			return null;
		}
	}
	public boolean isList(){
		return this.getClass().equals(PoetsType.ListT.class);
	}
	public boolean isInt(){
		return this.getClass().equals(PoetsType.IntT.class);
	}
	public boolean isDuration() {
		return this.getClass().equals(PoetsType.DurationT.class);
	}
	public boolean isBool(){
		return this.getClass().equals(PoetsType.BoolT.class);
	}
	public boolean isRec(){
		return this.getClass().equals(PoetsType.RecT.class);
	}
	public boolean isDouble(){
		return this.getClass().equals(PoetsType.DoubleT.class);
	}
	public boolean isRef(){
		return this.getClass().equals(PoetsType.RefT.class);
	}
	public boolean isDateTime(){
		return this.getClass().equals(PoetsType.DateTimeT.class);
	}
	public boolean isDate(){
		return this.getClass().equals(PoetsType.DateT.class);
	}
	public boolean isTime(){
		return this.getClass().equals(PoetsType.TimeT.class);
	}
	public boolean isString(){
		return this.getClass().equals(PoetsType.StringT.class);
	}
}
