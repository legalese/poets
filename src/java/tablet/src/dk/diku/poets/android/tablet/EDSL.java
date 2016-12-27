package dk.diku.poets.android.tablet;

import java.io.File;
import java.io.FileOutputStream;

import java.lang.reflect.Method;

import java.util.List;

import android.content.Context;
import android.content.Intent;

import android.graphics.Typeface;

import android.net.Uri;

import android.os.AsyncTask;
import android.os.Environment;

import android.view.View;

import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.Toast;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.DateTimeV;
import dk.diku.poets.record.PoetsValue.RecV;
import dk.diku.poets.record.PoetsValue.RefV;
import dk.diku.poets.record.PoetsValue.TimeV;

public class EDSL extends DSL {

	public EDSL() {
		super();
	}

	public EDSL(PoetsValue pv) {
		super(pv);
	}

	void Header(String t) {
		Text(t, 24, Typeface.DEFAULT_BOLD);
	}

	String time(PoetsValue pv) {
		if(pv instanceof TimeV) {
			TimeV dt = (TimeV)pv;
			return dt.val.getTime();
		} else if(pv instanceof DateTimeV) {
			DateTimeV dt = (DateTimeV)pv;
			return dt.val.toString();
		} else {
			return "N/A";
		}
	}

	@SuppressWarnings("unchecked")
	public static EDSL mk(PoetsValue pv) {
		EDSL edsl = new EDSL(pv);
		String name = (pv instanceof RecV)?Utils.get((RecV)pv).getName():pv.toType().toString();
		System.out.println("MK: name = " + name);
		Class<EDSL> e = (Class<EDSL>) edsl.getClass();
		try {
			Method m = e.getDeclaredMethod(name, PoetsValue.class);
			m.invoke(edsl, pv);
			return edsl;
		} catch (Exception ee) {
			throw new RuntimeException(ee);
		} 
	}

	void Location(PoetsValue pv) {
		if(pv instanceof RecV) {
			RecV loc = Utils.get((RecV)pv);
			if(isA(loc, "Salen")) {
				List<PoetsValue> tables = l(loc, "tableNumbers");
				if(tables.size() == 0) {
					Text("Salen");
				} else {
					OnTop();
						Text("Salen");
						String tStr = "";
						for(PoetsValue table : tables) {
							tStr += table + " ";
						}
						Text(tStr);
					End();
				}
			} else {
				Text(loc.toString());
			}
		}
	}

	void ItemType(PoetsValue pv) {
		if(pv instanceof RecV) {
			RecV loc = Utils.get((RecV)pv);
			if(isA(loc, "Bicycle")) {
			    Text("Bike (model: "+ on(loc,"model") +")");
			} else if(isA(loc, "BikeChain")) {
			    Text("Bike chain (size: "+ on(loc,"size") +")");
			}else {
				Text(loc.toString());
			}
		}
	}

	void Moneys(List<PoetsValue> l) {
	    if (l.size() > 0) {
		NextTo();
		for (PoetsValue m : l) {
		    Money(m);
		}
		End();
	    } else {
		Text("0");
	    }
	}
	void Money(PoetsValue pv) {
		if(pv instanceof RecV) {
		    RecV money = Utils.get((RecV)pv);
		    if(isA(money, "Money")) {
			Text((Utils.get((RecV)on(money,"currency"))).getName() +" " + on(money,"amount"));
		    } else
			Text(money.toString());
		}
	}
	void Agent(PoetsValue pv) {
		if(pv instanceof RecV) {
			RecV agent = Utils.get((RecV)pv);
			Text(agent.getName());
		}
	}

	void Customer(PoetsValue pv) {
		if(pv instanceof RecV) {
			RecV agent = Utils.get((RecV)pv);

			    Text(on(agent,"name"));

		}
	}

	void OrderLines(List<PoetsValue> l) {
	    if (l.size() > 0) {
		OnTop();
		for (PoetsValue m : l) {
		    OrderLine(m);
		}
		End();
	    } else {
		Text("<None>");
	    }
	}
	void OrderLine(PoetsValue pv) {
		if(pv instanceof RecV) {
			RecV line = Utils.get((RecV)pv);
			if(isA(line, "OrderLine")) {
			    with(on(line,"item"));
			    NextTo();
			    Text(f("quantity"));
			    Text(" X ");
			    ItemType(f("itemType"));
			    End();
			}
		}
	}

	void Adult(PoetsValue pv) {
		System.out.println("Calling 'Adult' with pv = " + pv);
		if(pv instanceof RecV) {
			RecV adult = Utils.get((RecV)pv);
			if(isA(adult, "Adult")) {
				Text(on(adult,"name") + ", " + on(adult, "phoneNumber"));
			}
		}
	}

	void AdultRef(PoetsValue pv) {
		if(pv instanceof RefV) {
			RefV ref = (RefV) pv;
			Ref(ref,new Cont() {
				public DSL get(final PoetsValue pv) {
					return (new DSL() {{
						with(Utils.get((RecV)pv));
						Text(f("name") + ", " + f("phoneNumber"));
					}});}});
		}
	}

	void Info(PoetsValue pv) {
		System.out.println("INFO for pv: " + pv);
		if(!(pv instanceof RecV)) return;
		RecV rec = Utils.get((RecV)pv);
		if(isA(rec, "Birthday")) {
			OnTop();
				for(PoetsValue child : l(rec, "birthdayChildren")) {
					Text(on(child,"name") + ", " + 
							 on(child,"age")
							);
				}
			End();
		} else if(isA(rec, "Occasion")) {
			Text(on(rec, "occasionInfo"));
		} else {
			Text("?");
		}
	}

	void Participants(PoetsValue pv) {
		RecV parts = Utils.get((RecV)pv);
		OnTop();
			NextTo();
				Text("Child: "  + on(parts,"numberOfChildren"));
				Text("Baby: "   + on(parts,"numberOfBabies"));
			End();
			NextTo();
				Text("Adult: "  + on(parts,"numberOfAdults"));
				Text("Grandp: " + on(parts,"numberOfGrandparents"));
			End();
		End();
	}
	
	void Food(PoetsValue pv) {
		RecV food = Utils.get((RecV)pv);
		if(isA(food, "WithoutFood")) {
			Text("Without food");
		} else if(isA(food,"FoodIncluded")) {
			OnTop();
				Text("Time: " + time(on(food,"eatingTime")));
				Text("Menu: " + on(food,"suggestion"));
				if(!on(food,"extraInfo").toString().equals("")){
					Text("Info: " + on(food,"extraInfo"));
				}
			End();
		} else {
			Text("?");
		}
	}

	private String sendTo = null;
	public EDSL setSendTo(String s) {
		sendTo = s;
		return this;
	}

	@Override
	public View getView(final Context mContext) {
		View superView = super.getView(mContext);
		if(sendTo == null) {
			return superView;
		}
		LinearLayout ll = new LinearLayout(mContext);
		ll.setOrientation(LinearLayout.VERTICAL);
		Button email = new Button(mContext);
		email.setText("Send by email");
		email.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				(new AsyncTask<Void,Void,String>() {
					@Override
					protected String doInBackground(Void... e) {
						return getHtml();
					}
					@Override
					protected void onPostExecute(String htmlString) {
						if(htmlString != null) {
							File sdCard = Environment.getExternalStorageDirectory();
							File dir = new File(sdCard.getAbsolutePath());
							File file = new File(dir, "report.html");

							try {
								FileOutputStream f = new FileOutputStream(file);
								f.write(htmlString.getBytes());
								f.close();


								Intent i = new Intent(Intent.ACTION_SEND);
								i.setType("text/html");
								i.putExtra(Intent.EXTRA_EMAIL, new String[] {sendTo});
								i.putExtra(Intent.EXTRA_SUBJECT, "Generated schedule");
								i.putExtra(Intent.EXTRA_STREAM, Uri.parse(
											"file://"+Environment.getExternalStorageDirectory().getAbsolutePath()+"/report.html"));
								try {
									mContext.startActivity(Intent.createChooser(i, "Send report by email..."));
								} catch(android.content.ActivityNotFoundException ex) {
									Toast.makeText(mContext, "No email clients?", Toast.LENGTH_SHORT).show();
								}
							} catch (Exception e) {
								System.out.println("ERROR HTML");
								e.printStackTrace();
							}

						}
					}
				}).execute();
			}});
		ll.addView(superView, new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));
		ll.addView(email);
		return ll;
	}
}
