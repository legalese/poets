package dk.diku.poets.record;

import java.io.Serializable;
import java.util.Date;

@SuppressWarnings("serial")
public class Calendar implements Serializable{

	public static final int YEAR         = 0;
	public static final int MONTH        = 1;
	public static final int DAY_OF_MONTH = 2;
	public static final int HOUR         = 3;
	public static final int MINUTE       = 4;
	public static final int SECOND       = 5;

	private Date date;

	@SuppressWarnings("deprecation")
	public int get(int field) {
		switch (field) {
		case YEAR:
			return date.getYear() + 1900;
		case MONTH:
			return date.getMonth() + 1;
		case DAY_OF_MONTH:
			return date.getDate();
		case HOUR:
			return date.getHours();
		case MINUTE:
			return date.getMinutes();
		case SECOND:
			return date.getSeconds();
		default:
			return -1;
		}
	}

	public static Calendar getInstance() {
		return new Calendar();
	}

	public void clear() {
		// TODO Auto-generated method stub

	}
	public Date date(){
		return date;
	}


	@Override
	public int hashCode() {
		return this.date.hashCode();
	}

	@Override
	public boolean equals(Object other) {
		if(this == other) return true;
		if(this.getClass() != other.getClass()) return false;
		
		Calendar otherCal = (Calendar) other;
		return this.date.equals(otherCal.date);
	}

	@SuppressWarnings("deprecation")
	public void set(int field, int value) {
		switch (field) {
		case YEAR:
			date.setYear(value - 1900);	
			break;
		case MONTH:
			date.setMonth(value - 1);
			break;
		case DAY_OF_MONTH:
			date.setDate(value);
			break;
		case HOUR:
			date.setHours(value);
			break;
		case MINUTE:
			date.setMinutes(value);
			break;
		case SECOND:
			date.setSeconds(value);
			break;
		default:
			break;
		}
	}
	public Calendar(){
		this(new Date());
	}
	public Calendar(Date date){
		this.date = date;
		//this.date.setSeconds(0);
	}
	/**
	 * Convert a string representation of a Calendar to
	 * a calendar object.
	 * Assuming the format as: YYYY-MM-DD HH:MM:SS (same as output from toString())
	 * @param calString
	 */
	public Calendar(String calString) {
		String[] l = calString.split("[- :]");
		int year =  Integer.parseInt(l[0]);
		int month = Integer.parseInt(l[1]);
		int day =   Integer.parseInt(l[2]);
		int hour =  Integer.parseInt(l[3]);
		int min =   Integer.parseInt(l[4]);
		date = new Date();
		set(YEAR, year);
		set(MONTH, month);
		set(DAY_OF_MONTH, day);
		set(HOUR, hour);
		set(MINUTE, min);
		if(l.length > 5){
			int sec =   Integer.parseInt(l[5]);
			set(SECOND, sec);
		}
		
	}

	public void add(int field, int value) {
		java.util.Calendar oldCal = 
			java.util.Calendar.getInstance();
		oldCal.setTime(date);
		switch (field) {
		case YEAR:
			oldCal.add(java.util.Calendar.YEAR, value);
		case MONTH:
			oldCal.add(java.util.Calendar.MONTH, value);
		case DAY_OF_MONTH:
			oldCal.add(java.util.Calendar.DAY_OF_MONTH, value);
		case HOUR:
			oldCal.add(java.util.Calendar.HOUR, value);
		case MINUTE:
			oldCal.add(java.util.Calendar.MINUTE, value);
		case SECOND:
			oldCal.add(java.util.Calendar.SECOND, value);
		default:
			break;
		}
		date = oldCal.getTime();
	}

	public String toString(){
		return getDate() + " " + getTime();
	}
	
	public String getDate() {
		StringBuffer sb = zeroPrepender(4, Integer.toString(get(YEAR)));
		sb.append('-');
		sb.append(zeroPrepender(2, Integer.toString(get(MONTH))));
		sb.append('-');
		sb.append(zeroPrepender(2, Integer.toString(get(DAY_OF_MONTH))));
		return sb.toString();
	}
	public String getTime(){
		StringBuffer sb = zeroPrepender(2, Integer.toString(get(HOUR)));
		sb.append(':');
		sb.append(zeroPrepender(2, Integer.toString(get(MINUTE))));
//		sb.append(':');
//		sb.append(zeroPrepender(2, Integer.toString(get(SECOND))));
		return sb.toString();
	}
	private StringBuffer zeroPrepender(int lenght, String s){
		int zeroCount = Math.max(lenght - s.length(), 0);
		StringBuffer sb = new StringBuffer();
		for(int i = zeroCount; i > 0; i--){
			sb.append(0);
		}
		sb.append(s);
		return sb;
	}
}
