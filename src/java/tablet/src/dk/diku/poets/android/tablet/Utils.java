package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import android.os.AsyncTask;

import android.view.View;
import android.view.ViewGroup;

import dk.diku.poets.gen.thrift.PoetsServer;


import dk.diku.poets.gen.thrift.value.Value;

import dk.diku.poets.poetsserver.ServerUtils;

import dk.diku.poets.record.Calendar;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.ListV;
import dk.diku.poets.record.PoetsValue.RecV;
import dk.diku.poets.record.PoetsValue.RefV;
import dk.diku.poets.record.PoetsValue.StringV;

import dk.diku.poets.record.RecordDecode;
import dk.diku.poets.record.RecordEncode;

public class Utils {
	public static final int NOVIEW = -42;

	public static abstract class Option <T> {
	}

	public static class None<T> extends Option<T> {
		public static <T> None<T> mk() {
			return new None<T>();
		}
	}

	public static class Some<T> extends Option<T> {
		public T value;
		public static <T> Some<T> mk(T v) {
			Some<T> s = new Some<T>(v);
			return s;
		} 

		public Some(T v) {
			value = v;
		}
	}

	public static RecV get(RecV rec) {
		return rec.getIsAbstract()?rec.getInstance():rec;
	}


	static public int getChildIndex(ViewGroup vg, View v) {
		for(int i = 0; i < vg.getChildCount(); i++) {
			if(vg.getChildAt(i).equals(v)) {
				return i;
			}
		}
		return NOVIEW;
	}

	public interface RunWithArg <T> {
		public void run(T t);
	}

	public static void withQuery(final String report, final List<PoetsValue> args,
			                   final Utils.RunWithArg<PoetsValue> rwa) {
				(new AsyncTask<Void, Void, PoetsValue> (){
					@Override
					protected PoetsValue doInBackground(Void... v) {
						System.out.println("Utils: about to run query: " + report);
						try {
							List<Value> valueArgs = new ArrayList<Value>(args.size());
							for(PoetsValue pv : args) {
								valueArgs.add(RecordEncode.encodeValue(pv));
							}
							PoetsServer.Iface pServer = ServerUtils.getServer();
							Value res = pServer.queryReport(report, new ArrayList<Value>(), valueArgs);
							return RecordDecode.decodeValue(res);
						} catch (Exception e) {
							return null;
						}
					}
					@Override
					protected void onPostExecute(PoetsValue pv) {
						rwa.run(pv);
					}
				}).execute();

	}

	public static PoetsValue withQuery(String reportName, List<PoetsValue> args) {
		try {
			List<Value> valueArgs = new ArrayList<Value>(args.size());
			for(PoetsValue pv : args) {
				valueArgs.add(RecordEncode.encodeValue(pv));
			}
			PoetsServer.Iface pServer = ServerUtils.getServer();
			Value res = pServer.queryReport(reportName, new ArrayList<Value>(), valueArgs);
			return RecordDecode.decodeValue(res);
		} catch (Exception e) {
			return null;
		}
	}

	static void getEntities(final String startType, final RunWithArg<Set<PoetsValue>> rwa) {
		(new AsyncTask<Void,Void,Set<PoetsValue>>() {
			@Override
			protected Set<PoetsValue> doInBackground(Void... arg0) {
				Set<PoetsValue> entities = new HashSet<PoetsValue>();
				try {
					PoetsServer.Iface pServer = ServerUtils.getServer();
					Set<String> subtypes = pServer.getSubTypes(startType);
					subtypes.add(startType);
					for(final String subtype : subtypes) {
						@SuppressWarnings("serial")
						Value vList = pServer.queryReport("EntitiesByType",
							new ArrayList<Value>(),
							new ArrayList<Value>() {{
								add(RecordEncode.encodeValue(new StringV(subtype)));
							}});
						List<PoetsValue> foundEntities = ((ListV) RecordDecode.decodeValue(vList)).val;
						entities.addAll(foundEntities);
					}
				} catch (Exception e) {
					System.out.println("CON10: error in getEntities " + startType);
					e.printStackTrace();
				}
				return entities;
			}
			@Override
			protected void onPostExecute(Set<PoetsValue> entities) {
				if(entities != null) {
					rwa.run(entities);
				}
			}
		}).execute();
	}	

	// returns true iff c1 < c2
	static public boolean before(Calendar c1, Calendar c2) {
		int[] c1s = {
			c1.get(Calendar.YEAR),
			c1.get(Calendar.MONTH),
			c1.get(Calendar.DAY_OF_MONTH),
			c1.get(Calendar.HOUR),
			c1.get(Calendar.MINUTE),
			c1.get(Calendar.SECOND)
		};
		
		int[] c2s = {
			c2.get(Calendar.YEAR),
			c2.get(Calendar.MONTH),
			c2.get(Calendar.DAY_OF_MONTH),
			c2.get(Calendar.HOUR),
			c2.get(Calendar.MINUTE),
			c2.get(Calendar.SECOND)
		};

		for(int i = 0; i < c1s.length; i++) {
			if(c1s[i] < c2s[i]) {
				return true;
			} else {
				if(c1s[i] == c2s[i]) {
					continue;
				} else {
					break;
				}
			}
		}

		return false;
	}

	static public PoetsValue getValueFromPath(RecV root, String... path) {
		if(path == null || root == null || path.length == 0) {
			System.out.println("Utils: error path = " + path + "; root = " + root);
			return null;
		}

		try {
			PoetsValue retVal = root;
			for(int i = 0; i < path.length; i++) {
				retVal = ((RecV)retVal).getField(path[i]);
			}

			return retVal;
		} catch (Exception e) {
			System.out.println("Utils: (exn) error path = " + path + "; root = " + root);
			return null;
		}
	}

	static public boolean isAtomic(PoetsValue pVal) {
		return !(pVal instanceof RecV || pVal instanceof ListV || pVal instanceof RefV);
	}

	static public Map<String, PoetsValue> getAtomicFields(RecV recV) {
		Map<String, PoetsValue> vals = 
			new HashMap<String, PoetsValue>();
		RecV cRecV = recV;

		if(recV.getIsAbstract()) {
			cRecV = recV.getInstance();
		}
		for(String fieldName : cRecV.getKeySet()) {
			PoetsValue fVal = cRecV.getField(fieldName);
			if(isAtomic(fVal)) {
				vals.put(fieldName, fVal);
			}
		}

		return vals;
	}

}
