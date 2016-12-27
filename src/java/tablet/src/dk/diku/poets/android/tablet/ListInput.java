package dk.diku.poets.android.tablet;

import java.util.Map.Entry;
import java.util.concurrent.ExecutionException;

import android.app.AlertDialog;

import android.content.Context;
import android.content.DialogInterface;

import android.os.AsyncTask;

import android.view.Gravity;

import android.view.View;
import android.view.ViewGroup;

import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.ScrollView;

import dk.diku.poets.gen.thrift.data.FieldDefinition;
import dk.diku.poets.record.PoetsType;

import dk.diku.poets.record.PoetsType.ListT;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.ListV;
import dk.diku.poets.record.RecordDecode;

public class ListInput extends InputWidget {

	private Entry<String, FieldDefinition> entry;
	private PoetsType elementType;

	private RelativeLayout rl;
	private LinearLayout ll;

	private AlertDialog ad;
	private View iwv;

	private Context mContext;

	int llid = 42;

	private static class ListEntryView extends Button implements View.OnClickListener {
		private AlertDialog ad;
		private String[] actions = new String[2];
		private final int rmIdx = 0;
		private final int edIdx = 1;
		private PoetsValue pVal;
		public ListEntryView(final Context c, PoetsValue pv) {
			super(c);
			pVal = pv;
			actions[rmIdx] = "Remove";
			actions[edIdx] = "Edit";
			setText(pv.toString());
			setTextSize(24);
			setOnClickListener(this);
			final InputSpec is = (InputSpec) 
				new InputSpec.Builder(c)
				.setType(pVal.toType()).create();
			final ScrollView sv = new ScrollView(c);
			ad = new AlertDialog.Builder(c)
				.setCancelable(true)
				.setTitle("Select action")
				.setItems(actions,new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface arg0, int arg1) {
						switch(arg1) {
							case rmIdx:
								ViewGroup par = (ViewGroup)getParent();
								par.removeView(ListEntryView.this);
								break;
							case edIdx:
								if(sv.getChildCount() == 0) {
									sv.addView(is);
								}
								System.out.println("CON10: (edit) filling value " + pVal);
								is.fill(pVal);
								new AlertDialog.Builder(c)
								.setCancelable(true)
								.setTitle("Edit data")
								.setNegativeButton("Cancel", null)
								.setPositiveButton("Submit modification", new DialogInterface.OnClickListener() {
									@Override
									public void onClick(DialogInterface arg0, int arg1) {
										(new AsyncTask<Void,Void,PoetsValue>() {
											@Override
											protected PoetsValue doInBackground(Void... v) {
												return is.getValue();
											}
											@Override
											protected void onPostExecute(PoetsValue newVal) {
												if(newVal != null) {
													pVal = newVal;
													ListEntryView.this.setText(pVal.toString());
												}
											}
										}).execute();
									}
								})
								.setView(sv)
								.create()
								.show();
								break;
							default:
						}
					}
				})
				.create();
		}
		@Override
		public void onClick(View arg0) {
			ad.show();
		}
		public PoetsValue getValue() {
			return pVal;
		}
	}

	public ListInput(Entry<String, FieldDefinition> e) {
		entry = e;
		elementType = 
			((ListT)RecordDecode.decodeType(entry.getValue().getFieldType())).elementType;
	}

	@Override
	public PoetsValue getValue() {
	    ListV lv = new ListV();
	    for(int i = 0; i < ll.getChildCount(); i++) {
		View v = ll.getChildAt(i);
		if(v instanceof ListEntryView) {
		    lv.addElement(((ListEntryView)v).getValue());
		}
	    }
	    return lv;
	}
	
	DialogInterface.OnClickListener diaListen =
		new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int whichButton) {
				switch(whichButton) {
					case DialogInterface.BUTTON_NEGATIVE:
						break;
					case DialogInterface.BUTTON_POSITIVE:
					    try {
						if(iwv != null) {
						    AsyncTask<Void,Void,PoetsValue> at = new AsyncTask<Void,Void,PoetsValue>() {
							@Override
							    protected PoetsValue doInBackground(Void... v) {
							    return ((Input)iwv).getValue();
							}
						    };
						    at.execute();
						    PoetsValue pval = at.get();
						    Context cxt = iwv.getContext();
						    ListEntryView v = new ListEntryView(cxt, pval);
						    if(v != null) {
							ll.addView(v);
							} else {
							System.out.println("CON11: null-value");
						    }
						}
					    }
					    catch (Exception e) {
						e.printStackTrace();
					    }
						break;
					default:
				}
			}
		};

	@Override
	protected View getView(final Context c) {
		mContext = c;
		rl = new RelativeLayout(c);
		ll = new LinearLayout(c);
		ll.setId(llid);
		ll.setOrientation(LinearLayout.VERTICAL);
		rl.addView(ll);
		ScrollView sv = new ScrollView(c);
		iwv = new InputSpec.Builder(c).setType(elementType).create();
		sv.addView(iwv);
		ad = new AlertDialog.Builder(c)
			.setCancelable(true)
			.setTitle("Enter data for new element")
			.setNegativeButton("Cancel", diaListen)
			.setPositiveButton("Add element", diaListen)
			.setView(sv)
			.create();

		Button plusB = new Button(c);
		plusB.setText("+");
		plusB.setTextSize(48);
		plusB.setPadding(4,4,4,4);
		plusB.setGravity(Gravity.CENTER);
		RelativeLayout.LayoutParams rllp =
			new RelativeLayout.LayoutParams(
					RelativeLayout.LayoutParams.WRAP_CONTENT,
					RelativeLayout.LayoutParams.WRAP_CONTENT);
		rllp.addRule(RelativeLayout.CENTER_HORIZONTAL);
		rllp.addRule(RelativeLayout.BELOW, llid);
		rl.addView(plusB, rllp);
		plusB.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				ad.show();
			}
		});
		return rl;
	}

	@Override
	public void fill(PoetsValue pv) {
		ListV ls = (ListV)pv;
		ll.removeAllViews();
		for(PoetsValue elem : ls.val) {
			ll.addView(new ListEntryView(mContext, elem));
		}
	}
}
