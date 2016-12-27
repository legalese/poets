package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;

import android.text.Html;

import android.view.Gravity;
import android.view.View;

import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;

import dk.diku.poets.record.PoetsValue.IntV;
import dk.diku.poets.record.PoetsValue.RecV;
import dk.diku.poets.record.PoetsValue.StringV;

public class ViewSpecData {


	private static String[] PutContractType =
		new String[] {"CreateContract", "UpdateContract"};

	@SuppressWarnings("serial")
	private static List<String[]> ArrangementFields =
		new ArrayList<String[]>() {{
			add(new String[] {"contract", "arrivalDate"});
			add(new String[] {"contract", "contactPerson"});
		}};

	public static class ArrangementViewSpec extends RecViewSpec.DeclarativeRecViewSpec {
		public ArrangementViewSpec(String[] types, List<String[]> reqFs) {
			super(types, reqFs);
		}

		@Override
		public View getView(Context mContext, EmbedAs emb) {
			switch(emb) {
				case FULL:
				case TILE:
					View v = super.getView(mContext, emb);
					if(v instanceof LinearLayout) {
						LinearLayout ll = (LinearLayout)v;
						TextView header = new TextView(mContext);
						RecV rec = (RecV)((RecV) this.pVal).getField("contract");
						header.setText(Html.fromHtml("<b>"+rec.getName()+"</b>"));
						header.setTextSize(20);
						ll.addView(header, 0);
						return ll;
					} else { 
						return v;
					}
				default:
					TextView tv = new TextView(mContext);
					tv.setText("?");
					return tv;
			}
		}
	}

	@SuppressWarnings("serial")
	public static RecViewSpec PutContractGenericViewSpec =
		new ArrangementViewSpec(
				PutContractType,
				new ArrayList<String[]>(ArrangementFields) {{
					add(new String[] {"contract", "occasionInfo"});
				}});
	
	@SuppressWarnings("serial")
	public static RecViewSpec PutContractBirthdayViewSpec =
		new ArrangementViewSpec(
				PutContractType,
				new ArrayList<String[]>(ArrangementFields) {{
					add(new String[] {"contract", "birthdayChildren"});
				}});

	public static RecViewSpec ArrangementView =
		new RecViewSpec.TypeIndexed(new String[] {"Birthday", "Occasion"}) {
			@Override
			public View getView(Context mContext, EmbedAs embedding) {
				final RecV arrRec = (RecV)pVal;
				return new EDSL() {{
					with(arrRec);
					Table();
						TableRow();Bold("Arrival");  Text(f("arrivalDate"));End();
						TableRow();Bold("Location"); Location(f("placement"));End();
						TableRow();
							NextTo();
								Bold("Children"); Text(on(f("participants"),"numberOfChildren"));
								Bold("Babies"); Text(on(f("participants"),"numberOfBabies"));
							End();
							NextTo();
								Bold("Adults");   Text(on(f("participants"),"numberOfAdults"));
								Bold("Grandpars"); Text(on(f("participants"),"numberOfGrandparents"));
							End();
						End();
						TableRow();Bold("Food");     Food(f("food")); End();
						TableRow();
							Bold("Contact");  
							AdultRef(f("contactPerson"));
						End();
						if(!f("comments").toString().equals("")) {
							TableRow();Bold("Comments");Text(f("comments"));End();
						}
					End();
				}}.getView(mContext);
			}
			
		};

	public static RecViewSpec MeRec =
		new RecViewSpec.TypeIndexed("Me") {
			@Override
			public View getView(Context mContext, EmbedAs embedding) {
				final Button nameView = new Button(mContext);
				nameView.setText("Me");
				nameView.setTextSize(18);
				nameView.setGravity(Gravity.LEFT|Gravity.CENTER_VERTICAL);

				return nameView;
			}
		};

	public static RecViewSpec AdultViewSpec =
		new RecViewSpec.TypeIndexed(
				new String[] {"Adult"}
		) {
			@Override
			public View getView(Context mContext, EmbedAs embedding) {
				return EDSL.mk(pVal).getView(mContext);
					//(new EDSL(pVal) {{Adult(pVal);}}).getView(mContext);
			}
		};


	@SuppressWarnings("serial")
	public static RecViewSpec ChildViewSpec =
		new RecViewSpec.FieldIndexed(
				"Child",
				new ArrayList<String[]>(){{
					add(new String[] {"name"});
					add(new String[] {"age"});
				}}
		) {
			@Override
			public View getView(Context mContext, EmbedAs embedding) {
				List<FieldPair> fieldValues = getFieldValues();
				int age = ((IntV)fieldValues.get(1).pathVal).val;
				String name = ((StringV)fieldValues.get(0).pathVal).val;
				TextView nameView = new TextView(mContext);

				nameView.setText(Html.fromHtml("<b>"+name+"</b>  " + Integer.toString(age) + " years"));
				nameView.setTextSize(18);
				nameView.setGravity(Gravity.LEFT|Gravity.CENTER_VERTICAL);

				return nameView;
			}
		};


	public static ListViewSpec GenericListViewSpec = new ListViewSpec();
	public static RefViewSpec  GenericRefViewSpec  = new RefViewSpec();
	public static AtomicViewSpec GenericAtomicViewSpec = new AtomicViewSpec();
}
