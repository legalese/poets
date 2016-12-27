package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.thrift.TException;

import android.animation.LayoutTransition;

import android.app.Fragment;
import android.app.ProgressDialog;

import android.content.Context;
import android.content.Intent;

import android.graphics.Color;
import android.graphics.Typeface;

import android.os.AsyncTask;
import android.os.Bundle;

import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;

import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

import dk.diku.poets.gen.thrift.contracts.Transaction;
import dk.diku.poets.gen.thrift.contracts.TransactionPattern;

import dk.diku.poets.gen.thrift.PoetsServer;
import dk.diku.poets.gen.thrift.RunTimeException;

import dk.diku.poets.gen.thrift.reporting.ReportNotFoundException;
import dk.diku.poets.gen.thrift.value.TypeException;
import dk.diku.poets.gen.thrift.value.Value;

import dk.diku.poets.poetsserver.ServerUtils;

import dk.diku.poets.record.Calendar;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.IntV;
import dk.diku.poets.record.PoetsValue.ListV;
import dk.diku.poets.record.PoetsValue.RecV;

import dk.diku.poets.record.RecordDecode;

public class ContractsFragment extends Fragment implements POETS.Refresh {

	private LinearLayout ll, activeContractViews, todayList, soonList, laterList;
	private Context mContext;
	private ProgressBar pb;
	private List<PoetsValue> activeContracts;
	private Map<Integer, List<TransactionPattern>> expectedTransactions;
	private LinearLayout conList;

	private static final int startConMenuItemId = 1;
	private static final int refreshId = 2;

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater infl) {

		menu.add(Menu.NONE, refreshId, Menu.NONE, "Refresh");

		menu.add(Menu.NONE, startConMenuItemId, Menu.NONE, "Start contract")
			  .setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
	}


	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch(item.getItemId()) {
			case startConMenuItemId:
				Intent i = new Intent();
				i.setClass(mContext, StartContractActivity.class);
				mContext.startActivity(i);
				return true;
			case refreshId:
				final ProgressDialog pd = ProgressDialog.show(mContext, 
						"Contacting server", 
						"Please wait while reconnecting to server", true, false);
				(new AsyncTask<Void, Void, Boolean>() {
					@Override
					protected Boolean doInBackground(Void... v) {
						ServerUtils.refreshServer();
						return true;
					}
					@Override
					protected void onPostExecute(Boolean refreshed) {
						if(refreshed != null && refreshed) {
							pd.dismiss();
							refresh();
						}
					}
				}).execute();
				return true;
			default:
				break;
		} 
		return false;
	}

	private void refreshActiveContracts(PoetsServer.Iface pServer) 
		throws ReportNotFoundException, 
					 RunTimeException, TypeException, TException {
		Value conValueList = pServer.queryReport("Contracts",
				new ArrayList<Value>(),
				new ArrayList<Value>());
		ListV l = (ListV) RecordDecode.decodeValue(conValueList);
		activeContracts = l.val;
	}

	private void refreshTransactionPatterns(PoetsServer.Iface pServer) {
		if(activeContracts != null) {
			if(expectedTransactions == null) {
				expectedTransactions = new HashMap<Integer, List<TransactionPattern>>();
			}
			try {
				System.out.println("CON11: activeContracts = " + activeContracts);
				for(PoetsValue pv : activeContracts) {
					RecV conR = (RecV) pv;
					Integer cid = ((IntV)Utils.getValueFromPath(conR, "contractId")).val;
					List<TransactionPattern> tps = 
						pServer.getExpectedTransactions(cid, new ArrayList<Transaction>());
					System.out.println("CON11: tps = " + tps);
					expectedTransactions.put(cid, tps);
				}
			} catch(Exception e) {
				System.out.println("CON11: error fetching tps'");
				e.printStackTrace();
			}
		}
	}
		
	private void fetchActiveContracts() {
		(new AsyncTask<Void, Void, Boolean>() {
			@Override
			protected Boolean doInBackground(Void... a) {
				try {
					PoetsServer.Iface pServer = ServerUtils.getServer();

					// refresh list of contracts
					refreshActiveContracts(pServer);

					// refresh map of TPs for each contract
					refreshTransactionPatterns(pServer);

					return true;
				} catch (Exception e) {
					return false;
				}
			}
			@Override
			protected void onPostExecute(Boolean refreshed) {
				if(refreshed != null && refreshed) {
					updateContractsUI();
				} else {
					Toast.makeText(mContext, "Error refreshing contract-list", Toast.LENGTH_LONG).show();
				}
			}
		}).execute();
	}


	private void addActiveContractView(Integer cid, RecV putC) {
		/*
		int numActive = activeContractViews.getChildCount();
		for(int i = 0; i < numActive; i++) {
			View vi = activeContractViews.getChildAt(i);
			if(vi.getTag() == cid) {
				activeContractViews.removeView(vi);
				activeContractViews.addView(vi,0);

				((ContractView)vi).refresh();
				return;
			}
		}
		*/
		activeContractViews.removeAllViews();
		View cv = new ContractView(mContext, cid, putC);
		cv.setTag(cid);
		activeContractViews.addView(cv,0,
				new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));
	}

	private View makeConTile(final RecV putC) {

		final int cid = ((IntV)Utils.getValueFromPath(putC, "contractId")).val;

		View conTile = new ConTile(mContext, putC, expectedTransactions.get(cid));
		//tileRL.setBackgroundColor(Color.argb(0x90,0x20,0x20,0x40));
		conTile.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				addActiveContractView(cid, putC);
			}
		});

		return conTile;
	}

	private View mkLLRule(int height) {
		View r = new View(mContext);
		r.setBackgroundColor(Color.argb(0x90, 0x4d,0x4d,0xcf));
		r.setLayoutParams(new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT, height));
		return r;
	}

	private TextView mkHeader(String text) {
		TextView header = new TextView(mContext);
		header.setText(text);
		header.setTextSize(24);
		header.setTypeface(Typeface.DEFAULT_BOLD);
		header.setGravity(Gravity.CENTER_VERTICAL|Gravity.RIGHT);
		header.setBackgroundColor(Color.argb(0x40,0x4d,0x4d,0xcf));
		return header;
	}

	private void setupConLists() {
		TextView todayHeader = mkHeader("Today");
		todayList = new LinearLayout(mContext);
		todayList.setOrientation(LinearLayout.VERTICAL);
		todayList.setPadding(todayList.getPaddingLeft(), todayList.getPaddingTop(), todayList.getPaddingRight(), 20);
		conList.addView(todayHeader);
		conList.addView(mkLLRule(6));
		conList.addView(todayList);

		TextView soonHeader = mkHeader("Next 7 days");
		soonList = new LinearLayout(mContext);
		soonList.setOrientation(LinearLayout.VERTICAL);
		soonList.setPadding(soonList.getPaddingLeft(), soonList.getPaddingTop(), soonList.getPaddingRight(), 20);
		conList.addView(soonHeader);
		conList.addView(mkLLRule(6));
		conList.addView(soonList);

		TextView laterHeader = mkHeader("Eventually");
		laterList = new LinearLayout(mContext);
		laterList.setOrientation(LinearLayout.VERTICAL);
		laterList.setPadding(laterList.getPaddingLeft(), laterList.getPaddingTop(), laterList.getPaddingRight(), 20);
		conList.addView(laterHeader);
		conList.addView(mkLLRule(6));
		conList.addView(laterList);
	}

	// returns true if a TP in TPS must be performed 'today'
	private boolean containsToday(List<TransactionPattern> tps) {
		Calendar midnightToday = Calendar.getInstance();
		midnightToday.set(Calendar.HOUR, 23);
		midnightToday.set(Calendar.MINUTE, 59);

		for(TransactionPattern tp : tps) {
			Calendar tpCal = RecordDecode.decodeCalendar(tp.getDeadline().getUpperLimit());
			if(Utils.before(tpCal, midnightToday)) {
				return true;
			}
		}
		return false;
	}

	// returns true if a TP in tps is only due within the next seven
	// days from 'today'
	private boolean containsSoon(List<TransactionPattern> tps) {
		Calendar sevenDays = Calendar.getInstance();
		sevenDays.set(Calendar.HOUR, 23);
		sevenDays.set(Calendar.MINUTE, 59);
		sevenDays.add(Calendar.DAY_OF_MONTH, 7);
		for(TransactionPattern tp : tps) {
			Calendar tpCal = RecordDecode.decodeCalendar(tp.getDeadline().getUpperLimit());
			if(Utils.before(tpCal, sevenDays)) {
				return true;
			}
		}
		return false;
	}

	private void addConTile(RecV putC) {
		View conTile = makeConTile(putC);
		ViewGroup targetGroup = null;

		Integer cid = ((IntV)Utils.getValueFromPath(putC, "contractId")).val;
		if(expectedTransactions != null) {
			List<TransactionPattern> tps = expectedTransactions.get(cid);
			if(tps != null) {
				if(containsToday(tps)) {
					targetGroup = todayList;
				} else if(containsSoon(tps)) {
					targetGroup = soonList;
				} else {
					targetGroup = laterList;
				}
				targetGroup.addView(conTile);
				conTile.setBackgroundResource(R.drawable.border);
			}
		}
	}

	private void addContracts() {
		setupConLists();

		for(PoetsValue pVal : activeContracts) {
			if(pVal instanceof RecV) {
				RecV putC = (RecV) pVal;
				if(putC.getIsAbstract()) {
					putC = putC.getInstance();
				}
				addConTile(putC);
			}
		}
	}

	private void updateContractsUI() {
		if(activeContracts != null) {
			if(expectedTransactions == null) {
				expectedTransactions = new HashMap<Integer, List<TransactionPattern>>();
			}
			if(ll.getChildCount() == 1) {
				ll.removeView(pb);
			}
			ScrollView conSV = new ScrollView(mContext);
			conList = new LinearLayout(mContext);
			conList.setOrientation(LinearLayout.VERTICAL);
			//conList.setBackgroundColor(Color.argb(0x90,0x10,0x20,0x40));
			conList.setPadding(20,20,20,20);
			addContracts();

			LinearLayout.LayoutParams lllp =
				new LinearLayout.LayoutParams(
						//LinearLayout.LayoutParams.WRAP_CONTENT,
						0,
						LinearLayout.LayoutParams.MATCH_PARENT);
			lllp.weight = 0.3f;
			conSV.addView(conList);
			ll.addView(conSV, lllp);
			activeContractViews = new LinearLayout(mContext);
			activeContractViews.setLayoutTransition(new LayoutTransition());
			ScrollView sv = new ScrollView(mContext);
			sv.addView(activeContractViews, new LinearLayout.LayoutParams(
						LinearLayout.LayoutParams.MATCH_PARENT,
						LinearLayout.LayoutParams.WRAP_CONTENT));
			sv.setBackgroundColor(Color.argb(0x90,0x15,0x15,0x25));

			View vRule = new View(mContext);
			vRule.setBackgroundColor(Color.GRAY);
			ll.addView(vRule, new LinearLayout.LayoutParams(1, LinearLayout.LayoutParams.MATCH_PARENT));

			LinearLayout.LayoutParams svlp =
				new LinearLayout.LayoutParams(0,
						LinearLayout.LayoutParams.MATCH_PARENT);
			svlp.weight = 0.7f;


			ll.addView(sv, svlp);

		}
	}

	@Override
	public View onCreateView(LayoutInflater infl, ViewGroup container, Bundle state) {

		setHasOptionsMenu(true);
		mContext = getActivity();

		refresh();
		return ll;
	}


	@Override
	public void refresh() {
		if(ll == null) {
			ll = new LinearLayout(mContext);
		} else {
			ll.removeAllViews();
			activeContracts = null;
			expectedTransactions = null;
			activeContractViews = null;
		}

		pb = new ProgressBar(mContext);
		pb.setIndeterminate(true);

		LinearLayout.LayoutParams lllp =
			new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT,
					LinearLayout.LayoutParams.WRAP_CONTENT);
		lllp.gravity = Gravity.CENTER;

		ll.addView(pb, lllp);
		ll.setLayoutParams(new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.MATCH_PARENT));
		fetchActiveContracts();
	}
}
