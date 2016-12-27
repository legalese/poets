package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import android.app.AlertDialog;

import android.content.Context;
import android.content.DialogInterface;

import android.graphics.Color;
import android.graphics.Typeface;

import android.os.AsyncTask;

import android.text.Html;

import android.view.Display;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;

import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

import dk.diku.poets.formfiller.RecordFiller;

import dk.diku.poets.gen.thrift.contracts.Transaction;
import dk.diku.poets.gen.thrift.contracts.TransactionKind;
import dk.diku.poets.gen.thrift.contracts.TransactionPattern;

import dk.diku.poets.gen.thrift.PoetsServer;

import dk.diku.poets.gen.thrift.value.DateTime;
import dk.diku.poets.gen.thrift.value.Value;

import dk.diku.poets.poetsserver.ServerUtils;

import dk.diku.poets.record.Calendar;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.DateTimeV;
import dk.diku.poets.record.PoetsValue.IntV;
import dk.diku.poets.record.PoetsValue.ListV;
import dk.diku.poets.record.PoetsValue.RecV;

import dk.diku.poets.record.RecordDecode;
import dk.diku.poets.record.RecordEncode;

public class ContractView extends LinearLayout {
	private Context mContext;
	private Integer contractId;
	private RecV putC;
	private InputSpec in;
	private PoetsServer.Iface pServer;

	private View buildTPForm(final TransactionPattern tp) {
		final InputSpec formView = (InputSpec)
			new InputSpec.Builder(mContext)
			.setRecordName(tp.getTransactionType())
			.create();
		(new AsyncTask<Void,Void,Map<String, PoetsValue>>() {
			@Override
			protected Map<String, PoetsValue> doInBackground(Void... v) {
				try {
					RecordFiller rf = new RecordFiller(tp.getPredicate());
					return rf.inferEnv();
				} catch (Exception e) {
					return null;
				}
			}
			@Override
			protected void onPostExecute(Map<String, PoetsValue> envMap) {
				if(envMap != null) {
					System.out.println("CON09: inferred some values");
					for(String fieldName : envMap.keySet()) {
						System.out.println("CON09: " + fieldName + " = " + envMap.get(fieldName));
						formView.fillField(fieldName, envMap.get(fieldName));
					}
				} else {
					System.out.println("CON09: no values inferred for " + tp);
				}
			}
		}).execute();
		return formView;
	}

	private View makeTPTile(final TransactionPattern tp) {
		LinearLayout ll = new LinearLayout(mContext);

		Calendar upperCal = RecordDecode.decodeCalendar(tp.getDeadline().getUpperLimit());

		TextView dueText = new TextView(mContext);
		dueText.setText(" (" + upperCal.getDate() + ")");
		LinearLayout.LayoutParams duelp = 
			new LinearLayout.LayoutParams(
					0,
					LinearLayout.LayoutParams.WRAP_CONTENT);
		duelp.weight = 0.2f;

		TextView timeText = new TextView(mContext);
		timeText.setText("Before " + upperCal.getTime());
		LinearLayout.LayoutParams timelp = 
			new LinearLayout.LayoutParams(
					0,
					LinearLayout.LayoutParams.WRAP_CONTENT);
		timelp.weight = 0.2f;

		Button tv = new Button(mContext);
		tv.setText(tp.getTransactionType());
		tv.setTextSize(18);
		LinearLayout.LayoutParams tvlp = 
			new LinearLayout.LayoutParams(
					0,
					LinearLayout.LayoutParams.WRAP_CONTENT);
		tvlp.weight = 0.6f;
		
		ll.addView(timeText, timelp);
		ll.addView(dueText, duelp);
		ll.addView(tv, tvlp);
		
		if(Utils.before(upperCal, Calendar.getInstance())) {
			timeText.setTextColor(Color.RED);
			timeText.setText("Overdue ");
			tv.setEnabled(false);
		}


		final View formView = buildTPForm(tp);
		ScrollView b = new ScrollView(mContext);
		b.addView(formView);

		DialogInterface.OnClickListener clickListen =
			new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface arg0, int arg1) {
					switch(arg1) {
						case DialogInterface.BUTTON_NEGATIVE:
							refresh();
							break;
						case DialogInterface.BUTTON_POSITIVE:
							// fetch values from View b and try to register
							// transaction
							(new AsyncTask<Void, Void, Boolean>() {
								@Override
								protected Boolean doInBackground(Void... e) {
									try {
										PoetsValue formValue = ((Input)formView).getValue();
										if(formValue != null && pServer != null) {
											List<Transaction> tList = new ArrayList<Transaction>();
											Transaction t = new Transaction();
											Value tData = RecordEncode.encodeValue(formValue);
											DateTime datetime = RecordEncode.encodeCalendarToDateTime(Calendar.getInstance());
											t.setContractId(contractId);
											t.setTimeStamp(datetime);
											t.setTransactionData(tData);
											tList.add(t);
											System.out.println("CON10: trying to register transaction: " + t);
											pServer.registerTransactions(tList);
											return true;
										} else {
											// Failed to fetch data from form
											return false;
										}
									} catch (Exception exn) {
										return false;
									}
								}
								@Override
								protected void onPostExecute(Boolean res) {
									if(res != null && res) {
										Toast.makeText(mContext, tp.getTransactionType() + " registered", Toast.LENGTH_LONG).show();
									} else {
										Toast.makeText(mContext, "FAILED to register " + tp.getTransactionType(), Toast.LENGTH_LONG).show();
									}
									refresh();
								}
							}).execute();
							break;
						default:
							break;
					}
				}
			};

		final AlertDialog ad =
					new AlertDialog.Builder(mContext)
					.setCancelable(true)
					.setTitle("Registration of " + tp.getTransactionType())
					.setNegativeButton("Cancel", clickListen)
					.setPositiveButton("Register", clickListen) //TODO : allow undoing this action
					.setView(b).create();
		// trick to make dialog a specific width instead of adjusting to
		// the initial width of the view; the problem is that the view
		// expands (which may actually be the real problem)
		final WindowManager.LayoutParams lp = new WindowManager.LayoutParams();
		lp.copyFrom(ad.getWindow().getAttributes());
		Display display = ((WindowManager) mContext.getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay();
		int width = display.getWidth();
		lp.width = (int) (width * 0.9);
		lp.height = WindowManager.LayoutParams.WRAP_CONTENT;


		tv.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				ad.show();
				ad.getWindow().setAttributes(lp);
			}});

		return ll;
	}

	private boolean includeTP(TransactionPattern tp) {
		System.out.println("Include tp? : " + tp);
		Calendar now = Calendar.getInstance();
		boolean isPermission = tp.getTransactionKind() == TransactionKind.Permission;
		Calendar upperLimit = RecordDecode.decodeCalendar(tp.getDeadline().getUpperLimit());
		Calendar lowerLimit = RecordDecode.decodeCalendar(tp.getDeadline().getLowerLimit());
		System.out.println("Upperlimit =  " + upperLimit);
		System.out.println("Lowerlimit =  " + lowerLimit);
		System.out.println("Is permission " + isPermission);

		boolean b =
		Utils.before(lowerLimit,now) && 
		(!isPermission || Utils.before(now,upperLimit));
		System.out.println(b?"Yes":"No");
		return b;
	}

	private void fetchTPs(final ProgressBar pb, final LinearLayout localll) {
		(new AsyncTask<Void, Void, List<TransactionPattern>>() {
			@Override
			protected List<TransactionPattern> doInBackground(Void... v) {
				try {
					return pServer.getExpectedTransactions(contractId, new ArrayList<Transaction>());
				} catch (Exception e) {
					return null;
				}
			}
			@Override
			protected void onPostExecute(List<TransactionPattern> tps) {
				if(tps != null) {
					localll.removeView(pb);
					System.out.println("Finding relevant TPS from " + tps);
					for(TransactionPattern tp : tps) {
						// only consider tp if it is actually due
						if(includeTP(tp)) {
							localll.addView(makeTPTile(tp));
						}
					}
				}
			}
		}).execute();
	}

	private void makeNext() {
		final LinearLayout localll =
			new LinearLayout(mContext);
		localll.setOrientation(LinearLayout.VERTICAL);
		// show all pending transactions
		ProgressBar pb =
			new ProgressBar(mContext);
		addView(localll, new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));
		
		TextView nextHeader =
			new TextView(mContext);
		nextHeader.setText("Pending transactions");
		nextHeader.setTypeface(Typeface.DEFAULT_BOLD);
		nextHeader.setTextSize(24);
		localll.addView(nextHeader, new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));

		localll.addView(pb,
				new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));

		fetchTPs(pb, localll);

		localll.setPadding(4,10,4,10);
	}

	private void fetchHistory(final ProgressBar pb, final LinearLayout localll) {
		(new AsyncTask<Void, Void, List<PoetsValue>>() {
			@Override
			protected List<PoetsValue> doInBackground(Void... v) {
				try {
					Value vId = RecordEncode.encodeValue(new IntV(contractId));
					List<Value> args = new ArrayList<Value>(1);
					args.add(vId);
					Value res = pServer.queryReport("ContractHistory",
						new ArrayList<Value>(), args);
					return ((ListV)RecordDecode.decodeValue(res)).val;
				} catch (Exception e) {
					return null;
				}
			}
			@Override
			protected void onPostExecute(List<PoetsValue> history) {
				if(history != null) {
					localll.removeView(pb);
					for(PoetsValue pv : history) {
						if(pv instanceof RecV) { 
							RecV transV = (RecV)pv;
							Button tv = new Button(mContext);
							tv.setText(
									transV.getName().equals("TransactionEvent")?
										((RecV)Utils.getValueFromPath(transV, "transaction")).getName():
										transV.getName()
							);
							localll.addView(tv);
						}
					}
				}
			}
		}).execute();
	}

	private View makeDeleteButton() {
		Button b = new Button(mContext);
		b.setText("Delete " + Utils.getValueFromPath(putC, "contract", "templateName").toString());
		b.setOnClickListener(new View.OnClickListener() {
			public void onClick(View v) {
				(new AsyncTask<Void,Void,Boolean>() {
					protected Boolean doInBackground(Void... a) {
						try {
							PoetsServer.Iface pServer = ServerUtils.getServer();
							pServer.deleteContract(contractId);
							return true;
						} catch (Exception e) {
							return false;
						}
					}
					protected void onPostExecute(Boolean deleted) {
						if(deleted != null && deleted) {
							Toast.makeText(mContext, "Deleted contract from system", Toast.LENGTH_LONG).show();
						} else {
							Toast.makeText(mContext, "FAILED to delete contract from system", Toast.LENGTH_LONG).show();
						}
					}
				}).execute();
			}
		});
		return b;
	}

	private void fetchMeta(final ProgressBar pb, final LinearLayout localll) {
		(new AsyncTask<Void, Void, Boolean>() {
			@Override
			protected Boolean doInBackground(Void... v) {
				try {
					return pServer.isConcludable(contractId);
				} catch (Exception e) {
					return null;
				}
			}
			@Override
			protected void onPostExecute(Boolean concludeable) {
				if(concludeable != null) {
					localll.removeView(pb);
					if(concludeable) {
						localll.addView(makeConcludeButton());
					}
					localll.addView(makeEditButton());
				}
				localll.addView(makeDeleteButton());
			}
		}).execute();
	}
		
	private void makeHistory() {
		// show all transaction relevant for contract
		final LinearLayout localll =
			new LinearLayout(mContext);
		localll.setOrientation(LinearLayout.VERTICAL);
		
		ProgressBar pb =
			new ProgressBar(mContext);
		addView(localll, new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));
		TextView nextHeader =
			new TextView(mContext);
		nextHeader.setText("Previous transactions");
		nextHeader.setTypeface(Typeface.DEFAULT_BOLD);
		nextHeader.setTextSize(24);
		localll.addView(nextHeader, new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));

		localll.addView(pb,
				new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));

		fetchHistory(pb, localll);
		localll.setPadding(4,10,4,10);

	}

	private void makeMeta() {
		// show latest meta-data for contract
		// allow showing older versions of meta as well
		// show all transaction relevant for contract
		final LinearLayout localll =
			new LinearLayout(mContext);
		localll.setOrientation(LinearLayout.VERTICAL);
		
		ProgressBar pb =
			new ProgressBar(mContext);
		addView(localll, new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));
		TextView nextHeader =
			new TextView(mContext);
		nextHeader.setText("Contract actions");
		nextHeader.setTypeface(Typeface.DEFAULT_BOLD);
		nextHeader.setTextSize(24);
		localll.addView(nextHeader, new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));

		localll.addView(pb,
				new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));

		fetchMeta(pb, localll);
		localll.setPadding(4,10,4,10);

	}

	private void removeConView() {
		((ViewGroup)ContractView.this.getParent()).removeView(ContractView.this);
	}

	private void makeInit() {
		setOrientation(LinearLayout.VERTICAL);
		setLayoutParams(new LinearLayout.LayoutParams(500, LinearLayout.LayoutParams.MATCH_PARENT));
		setBackgroundColor(Color.argb(0x90,0x20,0x20,0x30));
		setPadding(10,10,10,10);
		
		TextView header = new TextView(mContext);
		header.setTextSize(32);
		header.setText(((RecV)putC.getField("contract")).getName());
		addView(header, 0);
		Button closeB = new Button(mContext);
		closeB.setText("Close");
		closeB.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				removeConView();
			}});
		addView(closeB);
		View r = new View(mContext);
		r.setBackgroundColor(Color.argb(0xff,0x40,0x40,0x40));
		r.setLayoutParams(
				new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT, 1));
		addView(r);
		// get participants for contract ???
		addView(ViewSpecGetter
				.getViewSpec((RecV)putC.getField("contract"))
				.getView(mContext, ViewSpec.EmbedAs.FULL));
	}

	private View makeConcludeButton() {
		Button conc = new Button(mContext);
		conc.setText("Conclude " + Utils.getValueFromPath(putC, "contract", "templateName").toString());
		conc.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				(new AsyncTask<Void,Void,Boolean>() {
					@Override
					protected Boolean doInBackground(Void... a) {
						try {
							pServer.concludeContract(contractId);
							return true;
						} catch(Exception e) {
							return false;
						}
					}
				@Override
					protected void onPostExecute(Boolean concluded) {
						if(concluded != null) {
							if(concluded) {
								Toast.makeText(mContext, "Contract concluded", Toast.LENGTH_LONG).show();
								removeConView();
							} else {
								Toast.makeText(mContext, "FAILED to conclude contract", Toast.LENGTH_LONG).show();
							}
						}
					}
				}).execute();
			}});
		return conc;
	}

	private View makeEditButton() {
		Button edit = new Button(mContext);
		Calendar lastEdit = ((DateTimeV)putC.getField("internalTimeStamp")).val;
		edit.setText(Html.fromHtml(
				"Edit contract <font color=\"#A5A1A0\">("+lastEdit.getDate()+" @"+lastEdit.getTime()+")</font>"));

		final String recName = ((RecV)Utils.getValueFromPath(putC, "contract")).getName();
		in = (InputSpec) new InputSpec.Builder(mContext).setRecordName(recName).create();
		final ScrollView v = new ScrollView(mContext);
		v.addView((View)in);
		
		// I now want to make sure the view gets filled out with the
		// values from the previous contract-metadata stored in putC

		DialogInterface.OnClickListener l =
			new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface arg0, int arg1) {
					switch(arg1) {
						case DialogInterface.BUTTON_NEGATIVE:
							break;
						case DialogInterface.BUTTON_POSITIVE:
							(new AsyncTask<Void,Void,Boolean>() {
								@Override
								protected Boolean doInBackground(Void... e) {
									try {
										PoetsValue pv = in.getValue();

										if(pv != null) {
											Value newMetaValue = RecordEncode.encodeValue(pv);
											pServer.updateContract(contractId,newMetaValue);
											return true;
										} else {
											return false;
										}
										
									} catch (Exception exn) {
										return null;
									}
								}
								@Override
								protected void onPostExecute(Boolean updated) {
									if(updated != null && updated) {
										Toast.makeText(mContext, "Contract updated", Toast.LENGTH_LONG).show();
										ContractView.this.refresh();
										v.removeAllViews();
										in = (InputSpec) new InputSpec.Builder(mContext).setRecordName(recName).create();
										v.addView((View)in);
									} else {
										Toast.makeText(mContext, "FAILED to update contract", Toast.LENGTH_LONG).show();
									}
								}
							}).execute();
							break;
					}
				}
			};

		final AlertDialog ad = new AlertDialog.Builder(mContext)
			.setTitle("Edit " + Utils.getValueFromPath(putC, "contract", "templateName").toString())
			.setCancelable(true)
			.setPositiveButton("Update contract", l)
			.setNegativeButton("Cancel", l)
			.setView(v).create();

		// Need: 
		// - create widget of right type
		// - fill widget with some given putC
		edit.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				ad.show();
				in.fill(putC.getField("contract"));
			}
		});
		return edit;
	}

	public void refresh() {
		(new AsyncTask<Void,Void, RecV>() {
			@Override
			protected RecV doInBackground(Void... e) {
				try {
					pServer = ServerUtils.getServer();
					Value vCid = RecordEncode.encodeValue(new IntV(contractId));
					ArrayList<Value> vals = new ArrayList<Value>();
					vals.add(vCid);
					Value v = pServer.queryReport("ContractSummary",
						new ArrayList<Value>(),vals);
					return (RecV) ((ListV)RecordDecode.decodeValue(v)).val.get(0);
				} catch (Exception exn) {
					System.out.println("CON10: error in refresh of ContractView");
					exn.printStackTrace();
					return null;
				}
			}
			@Override
			protected void onPostExecute(RecV newPutC) {
				if(newPutC != null) {
					putC = newPutC;
				}

				removeAllViews();
				makeInit();
				makeNext();
				//ruler?
				makeHistory();
				//ruler?
				makeMeta();
			}
		}).execute();
	}

	public ContractView(Context c, Integer cid, RecV put) {
		super(c);
		mContext = c;
		contractId = cid;
		putC = put;

		refresh();

	}
}
