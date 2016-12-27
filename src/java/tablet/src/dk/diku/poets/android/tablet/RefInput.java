package dk.diku.poets.android.tablet;

import java.util.Map.Entry;

import java.util.Set;

import android.content.Context;

import android.graphics.Typeface;

import android.os.AsyncTask;

import android.view.Gravity;
import android.view.View;

import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import dk.diku.poets.gen.thrift.PoetsServer;

import dk.diku.poets.gen.thrift.type.Typ;
import dk.diku.poets.gen.thrift.type.Type;
import dk.diku.poets.gen.thrift.data.FieldDefinition;

import dk.diku.poets.gen.thrift.value.Value;

import dk.diku.poets.poetsserver.ServerUtils;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.RecV;
import dk.diku.poets.record.PoetsValue.RefV;

import dk.diku.poets.record.RecordEncode;

public class RefInput extends InputWidget {
	Entry<String, FieldDefinition> entry;
	String refTypeName;
	LinearLayout ll;
	Section<Boolean> actionSection; 
	Section<Boolean> entrySection;
	Button newB;
	Button useB;
	View recEdit;
	RefV refV, prefill;
	SelectBox prevView;

	public RefInput(Entry<String, FieldDefinition> e) {
		entry = e;
		Type type = entry.getValue().getFieldType();
		Typ rootTyp = type.getTypes().get(type.getRoot());
		refTypeName = rootTyp.getTypeEntity();
	}

	@Override
		public PoetsValue getValue() {
			// We will only generate a new reference if the environment
			// calls for a 'getValue'. 
			// Either we the use has pressed the 'new' button and we should
			// be able to confirm it by the presse
			if(recEdit != null) {
			    try {
				RecV recV = (RecV) ((Input)recEdit).getValue();
				PoetsServer.Iface pServer = ServerUtils.getServer();
				// TODO: assumes we are always creating references to
				// Records, but we could also create references to atomic
				// values
				System.out.println("CON10: Rec: " + recV);
				Value valuedata = RecordEncode.encodeValue(recV);
				System.out.println("CON10: Value = " + valuedata);
				int id = pServer.createEntity(valuedata, refTypeName);
				refV = new RefV(recV.getName(), id);
				return refV;
			    } catch (Exception e) {
			    }
			} else if(prevView != null && refV != null) {
			    return refV;
			}
			return null;
		}

	View.OnClickListener actionListener = new View.OnClickListener() {
		@Override
			public void onClick(View v) {
				boolean newPressed = !(v == useB);
				if(newPressed) {
					useB.setTypeface(Typeface.DEFAULT);
					newB.setTypeface(Typeface.DEFAULT_BOLD);
					refV = null;
				} else {
					newB.setTypeface(Typeface.DEFAULT);
					useB.setTypeface(Typeface.DEFAULT_BOLD);
				}

				entrySection.enable(newPressed);
			}
	};

	@Override
		protected View getView(final Context c) {
			ll = new LinearLayout(c);
			ll.setOrientation(LinearLayout.VERTICAL);
			actionSection = new Section<Boolean>(c,
					Section.makeSectionHeader(c, "a.", "Select action"));
			ll.addView(actionSection);
			actionSection.setMK(new Section.MkView<Boolean> (){
				@Override
				public View mk(Boolean data) {
					LinearLayout locll = new LinearLayout(c);
					locll.setPadding(50, locll.getPaddingTop(), locll.getPaddingRight(), locll.getPaddingBottom());
					newB = new Button(c);
					useB = new Button(c);
					locll.addView(newB);
					locll.addView(useB);
	
					newB.setTextSize(24);
					newB.setOnClickListener(actionListener);
					useB.setTextSize(24);
					useB.setOnClickListener(actionListener);

					newB.setText("New " + refTypeName);
					useB.setText("Pick previous");

					// set click-action for 'new'
					// set click-action for 'prev'

					return locll;
				}
			});
			Section.addRuler(c, ll);
			entrySection = new Section<Boolean>(c,
					Section.makeSectionHeader(c, "b.", "Modify data"));
			ll.addView(entrySection);
			entrySection.setMK(new Section.MkView<Boolean> () {
				@Override
				public View mk(Boolean newE) {
					if(newE) {
						recEdit = new InputSpec.Builder(c).setRecordName(refTypeName).create();
						return recEdit;
					} else {
						// fetch entities of right type, present list, and allow
						// user to select an entity
						return makeRefPicker(c);
					}
				}
			});

			actionSection.enable(true);
			return ll;
		}

	public View makeRefPicker(final Context c) {
		final LinearLayout refLL =
			new LinearLayout(c);
		ProgressBar pb = new ProgressBar(c);
		pb.setIndeterminate(true);
		refLL.addView(pb);
		TextView tv = new TextView(c);
		tv.setGravity(Gravity.CENTER);
		tv.setTextSize(18);
		tv.setText("Fetching " + refTypeName + " entities");
		refLL.addView(tv);

		Utils.getEntities(refTypeName,
				new Utils.RunWithArg<Set<PoetsValue>>() {
					@Override
					public void run(Set<PoetsValue> entities) {
						refLL.removeAllViews();
						refLL.setOrientation(LinearLayout.VERTICAL);
						for(final PoetsValue pv : entities) {
							if(pv instanceof RefV) {
								System.out.println("CON10: getting ViewSpec for " + pv);
								ViewSpec vs = ViewSpecGetter.getViewSpec(pv);
								System.out.println("CON10: got ViewSpec: " + vs);
								final SelectBox b = new SelectBox(c, vs.getView(c, ViewSpec.EmbedAs.TILE));
								refLL.addView(b);
								if(prefill != null && prefill.equals(pv)) {
									b.setEnabled(true);
									refV = (RefV) pv;
									if(prevView != null) {
										prevView.setEnabled(false);
									}
									prevView = b;
								}
								b.setOnClickListener(new View.OnClickListener() {
									@Override
									public void onClick(View v) {
										b.setEnabled(true);
										refV = (RefV) pv;
										if(prevView != null && prevView != v) {
											prevView.setEnabled(false);
										}
										prevView = b;
									}
								});
							}
						}
					}
		});

		return refLL;
	}

	@Override
	public void fill(PoetsValue pv) {
		// we must simulate 'pick previous'
		prefill = (RefV)pv;
	 	actionListener.onClick(useB);
	}
}

