package dk.diku.poets.android.tablet;

import java.lang.String;

import java.util.AbstractMap.SimpleEntry;

import java.util.ArrayList;

import java.util.concurrent.Semaphore;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.Map.Entry;

import java.util.Set;

import android.content.Context;

import android.os.AsyncTask;

import android.view.View;
import android.view.ViewGroup;

import android.widget.ProgressBar;
import android.widget.RelativeLayout;

import dk.diku.poets.gen.thrift.data.FieldAttribute;

import dk.diku.poets.gen.thrift.type.Typ;
import dk.diku.poets.gen.thrift.type.Type;
import dk.diku.poets.gen.thrift.type.TypeConstant;
import dk.diku.poets.gen.thrift.data.FieldDefinition;

import dk.diku.poets.record.PoetsType;

import dk.diku.poets.record.PoetsType.RecT;
import dk.diku.poets.record.PoetsValue;

import static dk.diku.poets.android.tablet.Utils.None;
import static dk.diku.poets.android.tablet.Utils.Some;
import static dk.diku.poets.android.tablet.Utils.Option;

import dk.diku.poets.record.PoetsValue.ListV;

public class InputSpec extends RelativeLayout implements Input {

	private Context mContext;
	private InputWidget iWidget;
	private ProgressBar pb;
	private Semaphore sem;

	private InputSpec(Context context) {
		super(context);
		sem = new Semaphore(0, true);
		mContext = context;
		pb = new ProgressBar(mContext);
		pb.setIndeterminate(true);
		pb.setLayoutParams(
				new ViewGroup.LayoutParams(
					ViewGroup.LayoutParams.WRAP_CONTENT,
					ViewGroup.LayoutParams.WRAP_CONTENT));
		addView(pb);
	}

	private void addInputWidget(InputWidget iw) {
		// replace the progressbar with the visual for the iw
		View v = iw.getView(mContext);
		if(v != null) {
			removeView(pb);
			v.setPadding(50, v.getPaddingTop(), v.getPaddingRight(), v.getPaddingBottom());
			addView(v, new RelativeLayout.LayoutParams(
						RelativeLayout.LayoutParams.MATCH_PARENT,
						RelativeLayout.LayoutParams.WRAP_CONTENT));
			iWidget = iw;
			sem.release();
		} else {
			System.out.println("CON10: Null view returned from 'getView' or perhaps context is null? " + mContext == null);
		}
	}

	// Hack: this method should really not be on the InputSpec class
	// since I only really need it for filling a field of a record-value
	public void fillField(final String fieldName, final PoetsValue pv) {
		(new AsyncTask<Void,Void,Boolean>(){
			@Override 
			protected Boolean doInBackground(Void... d) {
				try {
					sem.acquire();
					while(iWidget == null) {
						sem.acquire();
					}
				} catch (InterruptedException e) {
					return false;
				}
				return true;
			}
			@Override
			protected void onPostExecute(Boolean wokeUp) {
				if(wokeUp != null && wokeUp) {
					if(iWidget instanceof RecordInput) {
						((RecordInput)iWidget).fillField(fieldName, pv);
					}
				}
				sem.release();
			}
		}).execute();
	}

	@Override
	public void fill(final PoetsValue pv) {
		if(pv != null) {
			(new AsyncTask<Void,Void,Boolean>(){
				@Override 
				protected Boolean doInBackground(Void... d) {
					try {
						sem.acquire();
						while(iWidget == null) {
							sem.acquire();
						}
					} catch (InterruptedException e) {
						return false;
					}
					return true;
				}
				@Override
				protected void onPostExecute(Boolean wokeUp) {
					if(wokeUp != null && wokeUp) {
						iWidget.fill(pv);
					}
					sem.release();
				}
			}).execute();
		}
	}

	@Override
	public PoetsValue getValue() {
		if(iWidget == null) {
			return null;
		} else {
			return iWidget.getValue();
		}
	}

	public enum WidgetContext {
		NORMAL, LIST
	}

	public static class Builder {
		private Context mContext;
		private Entry<String, FieldDefinition> entry;
		private String recordName;
		private PoetsType ptype;

		public Builder(Context c) {
			mContext = c;
		}

		public Builder setWidgetContext(WidgetContext wc) {
			return this;
		}
		public Builder setType(PoetsType t) {
			ptype = t;
			return this;
		}
		public Builder setEntry(Entry<String, FieldDefinition> e) {
			entry = e;
			return this;
		}
		public Builder setRecordName(String rName) {
			recordName = rName;
			return this;
		}
		public View create() {
			InputSpec is = new InputSpec(mContext);			
			makeIW(is);
			return is;
		}

		private Option<String> getRecordRestriction(Set<FieldAttribute> fattrs) {
			for(FieldAttribute fa : fattrs) {
				if(fa.isSetFieldRestriction()) {
					return Some.mk(fa.getFieldRestriction());
				}
			}
			return None.mk();
		}
	
		/* This method is NOT run on the main GUI thread */
		private InputWidget makeIWTyped(InputSpec is) {
			InputWidget retIW = null;
			if(entry != null) { // we have an entry to generate from
				FieldDefinition fdef = entry.getValue();
				if(fdef != null && fdef.isSetFieldAttributes()) {
					Option<String> restrictionAttr = getRecordRestriction(fdef.getFieldAttributes());
					if(restrictionAttr instanceof Some) {
						String reportName = ((Some<String>)restrictionAttr).value;
						PoetsValue pVal = Utils.withQuery(reportName, new ArrayList<PoetsValue>());
						if(pVal instanceof ListV) {
							List<PoetsValue> vals = ((ListV)pVal).val;
							return new SimpleRestrictedInput(vals);	
						}
					}
				}

				Type type = entry.getValue().getFieldType();
				Typ roottyp = type.getTypes().get(type.getRoot());
				if(roottyp.isSetTypeConstant()) {
					switch(roottyp.getTypeConstant()) {
						case String:
							retIW = new StringInput(entry);
							break;
						case DateTime:
							retIW = new DateTimeInput(entry);
							break;
						case Time:
							retIW = new TimeInput(entry);
							break;
						case Date:
							retIW = new DateInput(entry);
							break;
						case Bool:
						case Duration:
						case Int:
							retIW = new IntInput(entry);
							break;
						case Real:
							retIW = new DoubleInput(entry);
							break;
					}
				} else if(roottyp.isSetTypeRecord()) {
					retIW = new RecordInput(entry);
				} else if(roottyp.isSetTypeEntity()) {
					retIW = new RefInput(entry);
				} else if(roottyp.isSetTypeList()) {
					retIW = new ListInput(entry);
				}
			} else if (recordName != null) { // we have a recordtype name to build from
				retIW = new RecordInput(recordName);
			} else if (ptype != null) {
				System.out.println("CON10: making widget from type: " + ptype);
				if(ptype instanceof RecT) {
					recordName = ((RecT)ptype).name;
					retIW = new RecordInput(recordName);
				} else {
					//FIXME: we should rewrite this entire method to not rely on
					//'Type' (as defined in thrift) but rather the
					//PoetsType-class
					FieldDefinition fd = new FieldDefinition();
					fd.setFieldType(convertPoetsType(ptype));
					entry = new SimpleEntry<String, FieldDefinition>("No label", fd);
					retIW = makeIWTyped(is);
				}
			} 

			return retIW;
		}
		
		private Type convertPoetsType(PoetsType pt) {
			Type type = new Type();
			Map<Integer, Typ> types = new HashMap<Integer,Typ>();
			type.setRoot(0);
			type.setTypes(types);
			Typ typ = new Typ();
			if(pt.isDateTime()) {
				typ.setTypeConstant(TypeConstant.DateTime);
			} else if(pt.isDate()) {
				typ.setTypeConstant(TypeConstant.Date);
			} else if(pt.isTime()) {
				typ.setTypeConstant(TypeConstant.Time);
			} else if(pt.isString()) {
				typ.setTypeConstant(TypeConstant.String);
			} else if(pt.isInt()) {
				typ.setTypeConstant(TypeConstant.Int);
			} else if(pt.isDouble()) {
				typ.setTypeConstant(TypeConstant.Real);
			}

			types.put(0, typ);
			return type;
		}



		private void makeIW(final InputSpec is) {
			(new AsyncTask<Void, Void, InputWidget>() {
				@Override
				protected InputWidget doInBackground(Void... arg0) {
					// construct the InputWidget async from the main GUI thread
					return makeIWTyped(is);
				}
				@Override
				protected void onPostExecute(InputWidget iw) {
					// add the iwidget to the inputspec thereby forcing a
					// getView call on iwidget to replace the progressbar
					// which is OK because we are on the GUI thread now
					if(iw != null) {
						is.addInputWidget(iw);
					}
				}
			}).execute();
		}
	}

}
