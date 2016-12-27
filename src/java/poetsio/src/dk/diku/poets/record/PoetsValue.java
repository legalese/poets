package dk.diku.poets.record;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import dk.diku.poets.gen.thrift.value.Duration;

/**
 * Java representation of the Thrift values without the "nasty"
 * stuff required by thrift.
 * Use the RecBuilder class to construct records safely.
 * 
 * @author jonsson, jespera
 *
 */
@SuppressWarnings("serial")
public abstract class PoetsValue implements Serializable{
	public static class DurationV extends PoetsValue implements Serializable {
		
		private int durationSeconds;
		private int durationMinutes;
		private int durationHours;
		private int durationDays;
		private int durationWeeks;
		private int durationMonths;
		private int durationYears;

		@Override
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof DurationV)) return false;
			DurationV other = (DurationV) otherObj;
			return 
				other.durationSeconds == durationSeconds &&
				other.durationMinutes == durationMinutes &&
				other.durationHours   == durationHours &&
				other.durationDays    == durationDays &&
				other.durationWeeks   == durationWeeks &&
				other.durationMonths  == durationMonths &&
				other.durationYears   == durationYears;
		}

		@Override
		public PoetsType toType() {
			return new PoetsType.DurationT();
		}

		public DurationV() {
			durationSeconds = 0;
			durationMinutes = 0;
			durationHours   = 0;
			durationDays    = 0;
			durationWeeks   = 0;
			durationMonths  = 0;
			durationYears   = 0;
		}

		public DurationV(Duration d) {
			durationSeconds = d.getDurationSeconds();
			durationMinutes = d.getDurationMinutes();
			durationHours   = d.getDurationHours();
			durationDays    = d.getDurationDays();
			durationWeeks   = d.getDurationWeeks();
			durationMonths  =	d.getDurationMonths();
			durationYears   = d.getDurationYears();
		}

		public Duration getDuration() {
			Duration d = new Duration();
			d.setDurationSeconds(durationSeconds);
			d.setDurationMinutes(durationMinutes);
			d.setDurationHours(durationHours);
			d.setDurationDays(durationDays);
			d.setDurationWeeks(durationWeeks);
			d.setDurationMonths(durationMonths);
			d.setDurationYears(durationYears);
			return d;
		}
	}
	public static class StringV extends PoetsValue implements Serializable{
		public PoetsType.StringT toType() {
			return new PoetsType.StringT();
		}
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof StringV)) return false;
			StringV otherS = (StringV) otherObj;
			return this.val.equals(otherS.val);
		}
		public StringV(){
			this.val = "";
		}
		public StringV(String val){
			this.val = val;
		}
		public String val;
	};
	public static class BoolV extends PoetsValue implements Serializable{
		public PoetsType.BoolT toType() {
			return new PoetsType.BoolT();
		}
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof BoolV)) return false;
			BoolV otherS = (BoolV) otherObj;
			return this.val == otherS.val;
		}
		public BoolV(){
			this.val = false;
		}
		public BoolV(boolean val){
			this.val = val;
		}
		public boolean val;
	};
	public static class DoubleV extends PoetsValue implements Serializable{
		public PoetsType.DoubleT toType() {
			return new PoetsType.DoubleT();
		}
		public DoubleV(){}
		public DoubleV(double val){
			this.val = val;
		}
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof DoubleV)) return false;
			DoubleV otherS = (DoubleV) otherObj;
			return this.val == otherS.val;
		}
		public double val;
	};
	public static class TimeV extends PoetsValue {
		public PoetsType.TimeT toType() {
			return new PoetsType.TimeT();
		}
		
		public TimeV(){
			this.val = new Calendar();
		}
		public TimeV(Calendar val){
			this.val = val;
		}
		public Calendar val;
		@Override
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof TimeV)) return false;
			TimeV otherS = (TimeV) otherObj;
			return this.val.equals(otherS.val);
		}
	};
	public static class DateV extends PoetsValue {
		public PoetsType.DateT toType() {
			return new PoetsType.DateT();
		}
		
		public DateV(){
			this.val = new Calendar();
		}
		public DateV(Calendar val){
			this.val = val;
		}
		public Calendar val;
		@Override
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof DateV)) return false;
			DateV otherS = (DateV) otherObj;
			return this.val.equals(otherS.val);
		}
	};
	public static class DateTimeV extends PoetsValue implements Serializable{
		public PoetsType.DateTimeT toType() {
			return new PoetsType.DateTimeT();
		}
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof DateTimeV)) return false;
			DateTimeV otherS = (DateTimeV) otherObj;
			return this.val.equals(otherS.val);
		}
		public DateTimeV(){
			this.val = new Calendar();
		}
		public DateTimeV(Calendar val){
			this.val = val;
		}
		public Calendar val;
	};
	public static class IntV extends PoetsValue implements Serializable{
		public PoetsType.IntT toType() {
			return new PoetsType.IntT();
		}
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof IntV)) return false;
			IntV otherS = (IntV) otherObj;
			return this.val == otherS.val;
		}
		public IntV(){}
		public IntV(int val){
			this.val = val; 
		}
		public int val;
	};
	public static class RecV extends PoetsValue implements Serializable{
		public PoetsType.RecT toType() {
			if(isAbstract && instance != null) {
				return instance.toType();
			} else {
				return new PoetsType.RecT(name);
			}
		}
		// FIXME? This doesn't compare abstract records
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof RecV)) return false;
			RecV otherS = (RecV) otherObj;
			return 
				this.name.equals(otherS.name) &&
				this.val.equals(otherS.val);
		}
		protected RecV(){}
		protected RecV(String name, boolean isAbstract, Set<String> superClasses){ 
			this.val          = new HashMap<String, PoetsValue>();
			this.name         = name;
			this.isAbstract   = isAbstract;
			this.superClasses = superClasses;
		}
		protected Map<String, PoetsValue> val;
		protected String name;
		protected boolean isAbstract;
		private Set<String> superClasses;
		private PoetsValue.RecV instance;
		public String getName(){
			return name;
		}
		public Set<String> getKeySet(){
			return this.val.keySet();
		}
		public PoetsValue getField(String name){
			return this.val.get(name);
		}
		public void putField(String name, PoetsValue value){
			this.val.put(name, value);
		}
		/**
		 * 
		 * @return True if the record is abstract
		 */
		public boolean getIsAbstract(){
			return this.isAbstract;
		}
		/**
		 * 
		 * @return The concrete instance of the abstract type if one is present.
		 */
		public PoetsValue.RecV getInstance(){
			return this.instance;
		}
		public Set<String> getSuperClasses(){
			return this.superClasses;
		}
		/**
		 * Set the instance of an abstract record.
		 * This will instantiate the record to the concrete
		 * sub type
		 * @param instance the instance of the record 
		 */
		public void setInstance(PoetsValue.RecV instance){
			if(!instance.getIsAbstract() && this.isAbstract){
				this.instance = instance;
			}
		}
	};
	public static class RefV extends PoetsValue implements Serializable{
		public PoetsType.RefT toType() {
			return new PoetsType.RefT(name);
		}
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof RefV)) return false;
			RefV otherS = (RefV) otherObj;
			return 
				this.refPointer == otherS.getRefPointer() &&
				this.name.equals(otherS.name);
		}
		public RefV(){}
		public RefV(String recordName, int refPointer){
			this.name = recordName;
			this.refPointer = refPointer;
		}
		public String getRefName(){
			return name;
		}
		public int getRefPointer(){
			return refPointer;
		}
		private String name;
		private int refPointer;
		public void setRefPointer(int refPointer) {
			this.refPointer = refPointer;
		}
	};
	public static class ListV extends PoetsValue implements Serializable{
		public PoetsType.ListT toType() {
			PoetsType.ListT lt = new PoetsType.ListT();
			lt.elementType = elementType.toType();
			return lt;
		}
		public boolean equals(Object otherObj) {
			if(this == otherObj) return true;
			if(!(otherObj instanceof ListV)) return false;
			ListV otherS = (ListV) otherObj;
			return this.val.equals(otherS.val);
		}
		public ListV(){
			val = new ArrayList<PoetsValue>();
		}
		public ListV(PoetsValue elementType){
			this();
			this.elementType = elementType;
		}
		public List<PoetsValue> val;
		public PoetsValue elementType;
		public PoetsValue getElementType(){
			return elementType;
		}
		public void addElement(PoetsValue elem){
			val.add(elem);
		}
	};
	@Override
	public String toString(){
		Class<? extends PoetsValue> cName = this.getClass();
		if(cName == RecV.class){
			RecV r = (RecV) this;
			StringBuffer ret = new StringBuffer();
			if(r.getIsAbstract()){
				if(r.getInstance() != null){
					ret.append("("+r.getInstance().getName()+")");
				}else{
					ret.append("%"+r.getName());
				}
			}else{
				ret.append(r.getName());				
			}
			if(r.getInstance() != null){
				if(r.getInstance().getKeySet().size() > 0){
					ret.append("{");
				}
				for(String key : r.getInstance().getKeySet()){
					ret.append(key + ": " + r.getInstance().getField(key)+", ");
				}				
				if(r.getInstance().getKeySet().size() > 0){
					ret.append("}");
				}
			}else{
				if(r.getKeySet().size() > 0){
					ret.append("{");
				}
				for(String key : r.getKeySet()){
					ret.append(key + ": " + r.getField(key) +", ");
				}
				if(r.getKeySet().size() > 0){
					ret.append("}");
				}
			}
			//ret.delete(ret.length()-3, ret.length()-1);
			return ret.toString();
		}else if(cName == DurationV.class) {
			DurationV dv = (DurationV) this;
			return dv.durationMinutes+"min";
		}else if(cName == IntV.class){
			IntV r = (IntV) this;
			return ""+r.val;
		}else if(cName == StringV.class){
			StringV r = (StringV) this;
			return ""+r.val;
		}else if(cName == DoubleV.class){
			DoubleV r = (DoubleV) this;
			return ""+r.val;
		}else if(cName == TimeV.class){
			TimeV r = (TimeV) this;
			return r.val.getTime();
		}else if(cName == DateV.class){
			DateV r = (DateV) this;
			return r.val.getDate();
		}else if(cName == DateTimeV.class){
			DateTimeV r = (DateTimeV) this;
			return ""+r.val;
		}else if(cName == BoolV.class){
			BoolV r = (BoolV) this;
			return ""+r.val;
		}else if(cName == RefV.class){
			RefV r = (RefV) this;
			return "(* "+r.getRefName()+", "+r.getRefPointer()+" *)";
		}else if(cName == ListV.class){
			ListV r = (ListV) this;
			StringBuffer ret = new StringBuffer("[");
			for(PoetsValue v : r.val){
				ret.append(v +", ");
			}
			//ret.delete(ret.length()-3, ret.length()-1);
			ret.append("]");
			return ret.toString();
		}else{
			System.err.println("Unrecognized PoetsValue ");
			return null;
		}
	}
	public boolean isList(){
		return this.getClass().equals(PoetsValue.ListV.class);
	}
	public boolean isInt(){
		return this.getClass().equals(PoetsValue.IntV.class);
	}
	public boolean isBool(){
		return this.getClass().equals(PoetsValue.BoolV.class);
	}
	public boolean isDuration(){
		return this.getClass().equals(PoetsValue.DurationV.class);
	}
	public boolean isRec(){
		return this.getClass().equals(PoetsValue.RecV.class);
	}
	public boolean isDouble(){
		return this.getClass().equals(PoetsValue.DoubleV.class);
	}
	public boolean isRef(){
		return this.getClass().equals(PoetsValue.RefV.class);
	}
	public boolean isTime(){
		return this.getClass().equals(PoetsValue.TimeV.class);
	}
	public boolean isDate(){
		return this.getClass().equals(PoetsValue.DateV.class);
	}
	public boolean isDateTime(){
		return this.getClass().equals(PoetsValue.DateTimeV.class);
	}
	public boolean isString(){
		return this.getClass().equals(PoetsValue.StringV.class);
	}
	public PoetsValue clone(){
		if(isRec()){
			RecV r = (RecV) this;
			PoetsValue.RecV newVal = new PoetsValue.RecV(r.getName(), r.getIsAbstract(), r.getSuperClasses());
			if(r.instance != null) {
				newVal.instance = (RecV)r.instance.clone();
			}
			for(String key : r.getKeySet()){
				newVal.putField(key, r.getField(key).clone());
			}
			return newVal;
		}else if(isList()){
			ListV r = (ListV) this;
			ListV newVal = new PoetsValue.ListV();
			newVal.elementType = r.elementType;
			for(PoetsValue v : r.val){
				newVal.val.add(v.clone());
			}
			return newVal;
		}else if(isRef()){
			RefV r = (RefV) this;
			return new RefV(r.getRefName(), r.getRefPointer());
		}else if(isBool()){
			return new BoolV(((BoolV)this).val);
		}else if(isString()){
			return new StringV(((StringV)this).val);
		}else if(isDouble()){
			return new DoubleV(((DoubleV)this).val);
		}else if(isTime()){
			return new TimeV(new Calendar((Date)((TimeV)this).val.date().clone()));
		}else if(isDate()){
			return new DateV(new Calendar((Date)((DateV)this).val.date().clone()));
		}else if(isDateTime()){
			return new DateTimeV(new Calendar((Date)((DateTimeV)this).val.date().clone()));
		}else if(isInt()){
			return new IntV(((IntV)this).val);
		}else if(isDuration()){
			return new DurationV(((DurationV)this).getDuration());
		}
		return null;
	}
	@Override
	public abstract boolean equals(Object otherObj);

	public abstract PoetsType toType();
}
