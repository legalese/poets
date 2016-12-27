package dk.diku.poets.android.tablet;

import java.io.File;
import java.io.FileOutputStream;

import java.util.List;

import android.content.Context;
import android.content.Intent;

import android.net.Uri;

import android.os.AsyncTask;
import android.os.Environment;

import android.view.View;

import android.webkit.WebView;

import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.Toast;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.DateTimeV;
import dk.diku.poets.record.PoetsValue.IntV;
import dk.diku.poets.record.PoetsValue.ListV;
import dk.diku.poets.record.PoetsValue.RecV;

public class ViewSpecLargeData {

	public static ListViewSpec ArrangementListViewSpec = 
		new ListViewSpec.TypeIndexed(new String[] {"Birthday", "Occasion", "Arrangement"}) {
			private StringBuilder sb;
			private WebView wv;
			private String htmlString;
			private void a(String s) {
				sb.append(s);
			}

			private void th(String s) {
				a("<th style=\"border-bottom: thin solid black;\">");a(s);a("</th>");
			}

			private void td(String s) {
				//a("<td style=\"border-right: thin solid black\">");a(s);a("</td>");
				a("<td>");a(s);a("</td>");
			}

			private String getInfo(RecV araRec) {
				String infoString;
				if(araRec.getName().equals("Birthday")) {
					List<PoetsValue> children = ((ListV)araRec.getField("birthdayChildren")).val;
					infoString = "<ul style=\"list-style-type: none; padding: 0px; margin: 0px;\">";
					for(PoetsValue childVal : children) {
						if(childVal instanceof RecV) {
							RecV childRec = (RecV) childVal;
							String name = childRec.getField("name").toString();
							Integer age = ((IntV)childRec.getField("age")).val;
							infoString += "<li>"+name+", "+age+"yrs</li>";
						}
					}
					infoString += "</ul>";
				} else if(araRec.getName().equals("Occasion")) {
					infoString = araRec.getField("occasionInfo").toString();
				} else {
					infoString = "info";
				}
				return infoString;
			}

			private String makeLocation(RecV locRec) {
				if(locRec.getName().equals("Salen")) {
					String locString = "Salen";
					List<PoetsValue> tables = ((ListV)locRec.getField("tableNumbers")).val;
					for(PoetsValue table : tables) {
						if(table instanceof IntV) {
							locString += " " + ((IntV)table).val;
						}
					}
					return locString;
				} else {
					return locRec.toString();
				}
			}

			private String makeFood(RecV foodRec) {
				if(foodRec.getName().equals("FoodIncluded")) {
					String menu = 
						foodRec.getField("suggestion").toString() +
						" " + foodRec.getField("suggestionInfo");
					DateTimeV dateV = (DateTimeV) foodRec.getField("eatingTime");
					return "Menu: " + menu + "<br/>" + dateV.val.getTime();
				} else {
					return foodRec.toString();
				}
			}

			private void addRow(final RecV araRec) {
				if(araRec == null) {
					a("<tr><td>NULL</td></tr>");
					return;
				}
				RecV contactP = (RecV) araRec.getField("contactPerson");
				a("<tr>");
				String info = getInfo(araRec);
				Integer num = ((IntV)contactP.getField("phoneNumber")).val;
				td(contactP.getField("name") 
						+ " " + (num==0?"":num.toString())
						+ "<br/>" + info);

				String location = makeLocation(Utils.get((RecV)araRec.getField("placement")));
				td(location);

				String arrival = Utils.getValueFromPath(araRec, "arrivalDate").toString();
				td(arrival);

				String numChild = Utils.getValueFromPath(araRec, "participants", "numberOfChildren").toString();
				td(numChild);

				String numAdults= Utils.getValueFromPath(araRec, "participants", "numberOfAdults").toString();
				td(numAdults);

				String food = makeFood(Utils.get((RecV)araRec.getField("food")));
				td(food);

				td("&nbsp;&nbsp;");

				a("</tr>");
			}

			private void makeHTML() {
				(new AsyncTask<Void,Void,String>() {
					@Override
					protected String doInBackground(Void... v) {
						try {
							sb = new StringBuilder();
							List<PoetsValue> pVals = ((ListV)pVal).val;
							System.out.println("makeHTML: " + pVals);
							a("<html>");
								a("<head>");
									a("<style type=\"text/css\">");
										a("body { font-size: 2em; }");
									a("</style>");
								a("</head>");
								a("<body>");
									a("<table frame=\"box\" cellspacing=\"20\" style=\"font-size: 0.8em;\"");
										a("<thead>");
											a("<tr>");th("Contact/info");th("Location");th("Arrival time");th("No. children");th("No. adults"); th("Food"); th("Payment");a("</tr>");
										a("</thead>");
										for(PoetsValue pv : pVals) {
											addRow(Utils.get((RecV)pv));
										}
									a("</table>");
								a("</body>");
							a("</html>");

							return sb.toString();
						} catch (Exception e) {
							e.printStackTrace();
							return "Error";
						}
					}
					@Override
					protected void onPostExecute(String htmlStr) {
						htmlString = htmlStr;
						wv.loadData(htmlString, "text/html", "utf-8");
					}
				}).execute();
			}

			private void makeArrTable(Context mContext, LinearLayout ll) {
				wv = new WebView(mContext);
				ll.addView(wv, new LinearLayout.LayoutParams(
							LinearLayout.LayoutParams.WRAP_CONTENT,
							LinearLayout.LayoutParams.WRAP_CONTENT));
				makeHTML();
			}
			@Override 
			public View getView(final Context mContext, EmbedAs emb) {
				LinearLayout ll = new LinearLayout(mContext);
				ll.setOrientation(LinearLayout.VERTICAL);
				makeArrTable(mContext, ll);
				final Button sendBut = new Button(mContext);
				sendBut.setText("Send by email");
				ll.addView(sendBut);
				sendBut.setOnClickListener(new View.OnClickListener() {
					@Override
					public void onClick(View v) {
						if(sendBut != null && !sendBut.getText().toString().equals("")) {
							File sdCard = Environment.getExternalStorageDirectory();
							File dir = new File(sdCard.getAbsolutePath());
							File file = new File(dir, "report.html");

							try {
								FileOutputStream f = new FileOutputStream(file);
								f.write(htmlString.getBytes());
								f.close();


								Intent i = new Intent(Intent.ACTION_SEND);
								i.setType("text/html");
								i.putExtra(Intent.EXTRA_EMAIL, new String[] {"info@legejunglen.dk"});
								i.putExtra(Intent.EXTRA_SUBJECT, "Generated schedule");
								//i.putExtra(Intent.EXTRA_TEXT, htmlString);
								i.putExtra(Intent.EXTRA_STREAM, Uri.parse(
										"file://"+Environment.getExternalStorageDirectory().getAbsolutePath()+"/report.html"));
								try {
									mContext.startActivity(Intent.createChooser(i, "Send email..."));
								} catch(android.content.ActivityNotFoundException ex) {
									Toast.makeText(mContext, "No email clients?", Toast.LENGTH_SHORT).show();
								}
							} catch (Exception e) {
								System.out.println("ERROR HTML");
								e.printStackTrace();
							}
						}
					}
				});
				return ll;
			}
		};
}
