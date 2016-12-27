package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import java.util.Map.Entry;

import java.util.Set;

import android.animation.LayoutTransition;

import android.content.Context;

import android.graphics.Typeface;

import android.os.AsyncTask;

import android.view.View;

import android.widget.Button;
import android.widget.HorizontalScrollView;
import android.widget.LinearLayout;
import android.widget.TextView;

import dk.diku.poets.gen.thrift.data.FieldAttribute;
import dk.diku.poets.gen.thrift.data.RecordDefinition;

import dk.diku.poets.gen.thrift.PoetsServer;

import dk.diku.poets.gen.thrift.type.Typ;
import dk.diku.poets.gen.thrift.type.Type;
import dk.diku.poets.gen.thrift.data.FieldDefinition;
import dk.diku.poets.gen.thrift.data.BasicRecordAttribute;


import dk.diku.poets.poetsserver.ServerUtils;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.RecV;
import dk.diku.poets.record.RecBuilder;
import dk.diku.poets.record.RecordDecode;

import static dk.diku.poets.android.tablet.Utils.Option;
import static dk.diku.poets.android.tablet.Utils.None;
import static dk.diku.poets.android.tablet.Utils.Some;


// TODOs:
// 1: handle when selected subtype contains no fields (in particular
// when _all_ subtypes contains no fields)
// 2: handle that some fields are pre-filled
// 3: handle that some fields are constrained
public class RecordInput extends InputWidget {

	LinearLayout ll, typeSelect;
	Entry<String, FieldDefinition> entry;
	RecordDefinition rDef;
	Set<RecordDefinition> subtypes;
	HumaniseCamelCase hcc = new HumaniseCamelCase();
	Section<Set<RecordDefinition>> typeSection;
	Section<RecordDefinition> dataSection;
	Context mContext;
	RecordDefinition selectedSubType;
	Map<String, Input> fieldInputs;
	Boolean isEnumType = true;
	RecV prefill = null;
	Map<String, PoetsValue> prefillFields;


	private Option<Integer> getFieldOrder(FieldDefinition fdef) {
		for(FieldAttribute fa : fdef.getFieldAttributes()) {
			if(fa.isSetFieldOrder()) {
				return Some.mk(fa.getFieldOrder());
			}
		}
		return None.mk();

	}

	private Comparator<Entry<String, FieldDefinition>> fcomp = 
		new Comparator<Entry<String, FieldDefinition>>(){
			@Override
			public int compare(Entry<String, FieldDefinition> e1,
				                 Entry<String, FieldDefinition> e2) {
				Option<Integer> order1opt = getFieldOrder(e1.getValue());
				Option<Integer> order2opt = getFieldOrder(e2.getValue());
				if(order1opt instanceof None) {
					if(order2opt instanceof None) {
						return 0;
					} else {
						return 1;
					}
				} else {
					if(order2opt instanceof None) {
						return -1;
					} else {
						return ((Some<Integer>)order1opt).value.compareTo(
										((Some<Integer>)order2opt).value);
					}
				}
			}
		};

	private View makeFieldsView(Context c, RecordDefinition rdef) {
		if(c != null) {
			mContext = c;
		}
		if(mContext == null) {
			System.out.println("CON12: mContext = " + mContext + " c = " + c);
			mContext = ll.getContext();
		}

		LinearLayout locll = new LinearLayout(mContext);
		locll.setOrientation(LinearLayout.VERTICAL);
		locll.setPadding(50, locll.getPaddingTop(), locll.getPaddingRight(), locll.getPaddingBottom());
		List<Entry<String, FieldDefinition>> fentries = 
			new ArrayList<Entry<String, FieldDefinition>>(rdef.getFieldsDefinitions().entrySet());
		Collections.sort(fentries, fcomp);
		//for(Entry<String, FieldDefinition> fieldEntry : rdef.getFieldsDefinitions().entrySet()) {
		for(Entry<String, FieldDefinition> fieldEntry : fentries) {
			TextView label = new TextView(mContext);
			label.setText(hcc.humanise(fieldEntry.getKey()));
			label.setTextSize(24);
			locll.addView(label);
			InputSpec is = (InputSpec) new InputSpec.Builder(mContext).setEntry(fieldEntry).create();
			if(prefill != null) {
				is.fill(prefill.getField(fieldEntry.getKey()));
			} else if(prefillFields != null) {
				PoetsValue pv = prefillFields.get(fieldEntry.getKey());
				if(pv != null) {
					is.fill(pv);
				}
			}
			fieldInputs.put(fieldEntry.getKey(), (Input)is);
			locll.addView((View)is, new LinearLayout.LayoutParams(
						LinearLayout.LayoutParams.MATCH_PARENT,
						LinearLayout.LayoutParams.WRAP_CONTENT));
		}
		return locll;
	}

	Section.MkView<RecordDefinition> dataEnable =
		new Section.MkView<RecordDefinition> () {
			@Override
				public View mk(final RecordDefinition selectedRDef) {
					selectedSubType = selectedRDef;
					return makeFieldsView(mContext, selectedRDef);
				}
		};

	private void setup(String recordName) {
		prefillFields = new HashMap<String, PoetsValue>();
		fieldInputs = new HashMap<String, Input>();
		PoetsServer.Iface pServer = ServerUtils.getServer();
		try {
			rDef = pServer.getRecordDefinition(recordName);
			Set<String> locsubs = pServer.getSubTypes(recordName);
			locsubs.add(recordName);
			subtypes = new HashSet<RecordDefinition>();
			for(String subtype : locsubs) {
				RecordDefinition r = pServer.getRecordDefinition(subtype);
				if(!RecordDecode.isAbstract(r)) {
					subtypes.add(r);
				}
				isEnumType &= r.getFieldsDefinitions().size() == 0;
			}	
		} catch (Exception ex) {
			System.out.println("CON11: problem in setup");
		}
	}

	public RecordInput(String recordName) {
		setup(recordName);
	}

	public RecordInput(Entry<String, FieldDefinition> e) {
		entry = e;
		Type type = entry.getValue().getFieldType();
		Typ rootTyp = type.getTypes().get(type.getRoot());
		setup(rootTyp.getTypeRecord());
	}

	@Override
	public PoetsValue getValue() {
	    try {
		if(selectedSubType != null && fieldInputs != null) {
		    RecBuilder rb = new RecBuilder(selectedSubType.getRecordName());
		    for(String fieldName : fieldInputs.keySet()) {
			PoetsValue pval = fieldInputs.get(fieldName).getValue();
			rb.setField(fieldName, pval);
		    }
		    return rb.create();
		} else {
		    System.out.println("CON11: explicit return of null: " + selectedSubType + "," + fieldInputs); 
		    System.out.println("CON11: subtypes = " + subtypes);
		    return null;
		}
	    } catch (Exception ex) {
		System.out.println("CON11: problem in RecV.getValue");
		return null;
	    }
	}

	@Override
		protected View getView(final Context c) {
			if(subtypes.size() == 1) {
				RecordDefinition rdef = (RecordDefinition)subtypes.toArray()[0];
				selectedSubType = rdef;
				if(rdef.getFieldsDefinitionsSize() == 0) {
					Button tv = new Button(c);
					tv.setTextSize(24);
					tv.setTypeface(Typeface.DEFAULT_BOLD);
					tv.setText(rdef.getRecordName());
					return tv;
				} else {
					return makeFieldsView(c, rdef);
				}
			}

			ll = new LinearLayout(c);
			ll.setLayoutTransition(new LayoutTransition());
			ll.setOrientation(LinearLayout.VERTICAL);
			ll.setPadding(50, ll.getPaddingTop(), ll.getPaddingRight(), ll.getPaddingBottom());

			// first section: select type of record
			// enter data section; changes when selecting diff. type
			typeSection = new Section<Set<RecordDefinition>>(c,
					Section.makeSectionHeader(c, "a.", "Select type"));
			typeSection.setMK(new Section.MkView<Set<RecordDefinition>>() {
					@Override
					public View mk(Set<RecordDefinition> data) {
					HorizontalScrollView hsv = new HorizontalScrollView(c);
					hsv.setPadding(50,2,2,2);
					typeSelect = new LinearLayout(c);
					hsv.addView(typeSelect);
					for(final RecordDefinition rtype : data) {
						final Button b = new Button(c);
						b.setTextSize(24);
						b.setText(rtype.getRecordName());
						b.setPadding(10,10,10,10);
						typeSelect.addView(b);
						b.setOnClickListener(new View.OnClickListener () {
							@Override
							public void onClick(View v) {
								// enable next section with proper argument
								mContext = c;
								enableAllBut(b, typeSelect);
								if(!isEnumType) {
									if(rtype.getFieldsDefinitionsSize() > 0) {
										if(Utils.getChildIndex(ll, dataSection) == Utils.NOVIEW) {
											ll.addView(dataSection);
										}
										dataSection.enable(rtype);
									} else {
										// remove data-section
										ll.removeView(dataSection);
										selectedSubType = rtype;
									}
								} else {
									selectedSubType = rtype;
								}
							}
						});
					}
					return hsv;
					}
			});
			typeSection.enable(subtypes);

			ll.addView(typeSection);
			if(!isEnumType) {
				Section.addRuler(c, ll);

				dataSection = new Section<RecordDefinition>(c,
						Section.makeSectionHeader(c, "b.", "Enter data"));
				dataSection.setMK(dataEnable);
				ll.addView(dataSection);
			}


			return ll;
		}

		private void enableAllBut(Button b, LinearLayout locll) {
			for(int i = 0; i < locll.getChildCount(); i++) {
				View v = locll.getChildAt(i);
				if(v instanceof Button) {
					if(v.equals(b)) {
						b.setEnabled(false);
					} else {
						((Button)v).setEnabled(true);
					}
				}
			}
		}

		public void fillField(String fieldName, PoetsValue pv) {
			if(pv != null) {
				prefillFields.put(fieldName, pv);
			}
			if(fieldInputs.containsKey(fieldName)) {
				fieldInputs.get(fieldName).fill(pv);
			} else {
				System.out.println("CON09: record does not yet contain widget for " + fieldName);
			}
		}

		@Override
		public void fill(PoetsValue pv) {
			RecV rv = (RecV)pv;
			prefill = rv.getIsAbstract()?rv.getInstance():rv;
			System.out.println("CON10: filling value " + pv);
			RecordDefinition rtype = null;
			// We need to figure our what the current record-view is
			// 1) Simple enumeration type
			// 2) At least one available subtype 
			for(RecordDefinition rdef : subtypes) {
				if(prefill.getName().equals(rdef.getRecordName())) {
					rtype = rdef;
					break;
				}
			}

			// no subtype found!
			if(rtype == null) {
				System.out.println("CON10: no subtype found?");
				return;
			} else {
				selectedSubType = rtype;
			}

			// enable proper button
			if(subtypes.size() > 1) {
				for(int i = 0; i < typeSelect.getChildCount(); i++) {
					View vi = typeSelect.getChildAt(i);
					if(vi instanceof Button) {
						if(((Button)vi).getText().toString().equals(prefill.getName())) {
							enableAllBut((Button)vi, typeSelect);
						}
					}
				}
				if(!isEnumType) {
						//dataSection.disable();
						dataSection.enable(rtype);
				}
			} else {
				for(String fieldName : fieldInputs.keySet()) {
					fillField(fieldName, prefill.getField(fieldName));
				}	
			}
		}
			
}

