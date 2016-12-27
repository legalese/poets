package dk.diku.poets.android.tablet;


import java.util.List;

import android.content.Context;

import android.graphics.Typeface;

import android.view.View;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.DateTimeV;
import dk.diku.poets.record.PoetsValue.RecV;
import dk.diku.poets.record.PoetsValue.ListV;

public class ReportViewSpecs {
	public static RecViewSpec ScheduleViewSpec =
		new RecViewSpec.TypeIndexed("Schedule") {
			@Override
			public View getView(Context mContext, EmbedAs embedding) {
				final RecV rec = Utils.get((RecV)pVal);
				return (new EDSL() {{
					Table();
						TableRow();
							Header("Contact person");
							Header("Info");
							Header("Placement");
							Header("Arrival time");
							Header("Participants");
							Header("Food");
							Header("Comments");
							Header("Payment");
						End();
						with(rec);
						for(PoetsValue pv : l("arrangements")) {
							with(pv);
							TableRow();
								AdultRef(f("contactPerson"));
								Info(Utils.get((RecV)pv));
								Location(f("placement"));
								Text(time(f("arrivalDate")));
								Participants(f("participants"));
								Food(f("food"));
								Text(f("comments"));
								Text(" ");
							End();
						}
					End();
				}}).setSendTo("info@legejunglen.dk").getView(mContext);
			}
		};

	public static RecViewSpec InventoryViewSpec =
		new RecViewSpec.TypeIndexed("Inventory") {
			@Override
			public View getView(Context mContext, EmbedAs embedding) {
				final RecV rec = Utils.get((RecV)pVal);
				return (new EDSL() {{
					Table();
						TableRow();
							Header("Item");
							Header("Quantity");
						End();
						with(rec);
						for(PoetsValue pv : l("availableItems")) {
							with(pv);
							TableRow();
								ItemType(f("itemType"));
								Text(f("quantity"));
							End();
						}
					End();
				}}).getView(mContext);
			}
		};

	public static RecViewSpec UnpaidInvoicesViewSpec =
		new RecViewSpec.TypeIndexed("UnpaidInvoices") {
			@Override
			public View getView(Context mContext, EmbedAs embedding) {
				final RecV rec = Utils.get((RecV)pVal);
				return (new EDSL() {{
					Table();
						TableRow();
							Header("Remainder");
							Header("Customer");
							Header("Order lines");
						End();
						with(rec);
						for(PoetsValue pv : l("invoices")) {
							with(pv);
							TableRow();
								Moneys(l("remainder"));
							    with(f("invoice"));
								Customer(f("receiver"));
								OrderLines(l("orderLines"));
							End();
						}
					End();
				}}).getView(mContext);
			}
		};



	public static RecViewSpec MonthlyOverviewSpec =
		new RecViewSpec.TypeIndexed("MonthlyOverview") {

			@Override
			public View getView(Context mContext, EmbedAs embedding) {
				final RecV rep = Utils.get((RecV)pVal);
				return (new EDSL() {{
					with(rep);
					OnTop();
						Header("Overview of montly payments");
						NextTo();Text("Payments received:");Text(f("total"));End();
						Text("Arrangements held", 18, Typeface.DEFAULT_BOLD);
						Table();
							TableRow();Header("Date");Header("Contact");Header("Info");Text("Payment");End();
							for(PoetsValue pv : l("arrangements")) {
								RecV arr = Utils.get((RecV)pv);
								makeArr(arr);
							}
						End();
					End();
				}
					void makeArr(RecV arr) {
						with(arr);
						TableRow();
							Text(((DateTimeV)(on(f("arrangement"),"arrivalDate"))).val.getDate());
							AdultRef(on(f("arrangement"),"contactPerson"));
							Info(f("arrangement"));
							Text(on(on(f("payment"), "money"), "amount"));
						End();
					}}).getView(mContext);
			}
		};

	// Generic visualization of report
	public static RecViewSpec GenericReportViewSpec = 
		new RecViewSpec() {
			//private HumaniseCamelCase hcc;
			@Override
			public View getView(Context mContext, EmbedAs embedding) {
				//hcc = new HumaniseCamelCase();
				final RecV rep = Utils.get((RecV)pVal);
				// header is name of report humanised
				return (new DSL(){{
					with(rep);
					OnTop();
					   Bold(rep.getName());
					   Table();
					   for(String fieldName : rep.getKeySet()) {
					       PoetsValue fVal = rep.getField(fieldName);
					       TableRow();
					       Text(fieldName);
					       if (fVal instanceof ListV) {
						   List<PoetsValue> l = ((ListV)fVal).val;
						    if (l.size() > 0) {
							NextTo();
							for (PoetsValue pv : l) {
							    RecV val = Utils.get((RecV)pv);
							    if(isA(val, "Money")) {
								Text((Utils.get((RecV)on(val,"currency"))).
								     getName() +" " + on(val,"amount"));
							    } else
								Text(val.toString());
							}
							End();
						    } else {
							Text("None");
						    }
						}
						else { 
						    Text(fVal);
						}
					       End();
					   }
					   End();
					End();
				}}).getView(mContext);
			}
		};
}
