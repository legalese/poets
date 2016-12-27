package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import android.app.AlertDialog;
import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.app.ProgressDialog;

import android.content.Context;
import android.content.DialogInterface;

import android.graphics.Color;

import android.os.AsyncTask;
import android.os.Bundle;

import android.text.Html;

import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;

import android.widget.LinearLayout;
import android.widget.TextView;

import dk.diku.poets.android.tablet.Utils.RunWithArg;

import dk.diku.poets.gen.thrift.PoetsServer;

import dk.diku.poets.gen.thrift.reporting.Report;

import dk.diku.poets.gen.thrift.type.Type;
import dk.diku.poets.poetsserver.ServerUtils;

import dk.diku.poets.record.PoetsType;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.ListV;
import dk.diku.poets.record.PoetsValue.StringV;

import dk.diku.poets.record.RecordDecode;

public class ReportsFragment extends Fragment implements POETS.Refresh {

	private static final int addId = 1;
	private static final int refreshId = 2;
	private Context mContext;
	private LinearLayout rootll;
	private static final String rrTag = "ReportResultFragTag";
	private Fragment rrFragment;
	private static final int rootid = 117;

	@Override
	public View onCreateView(LayoutInflater infl, ViewGroup container, Bundle state) {

		setHasOptionsMenu(true);
		mContext = getActivity();

		refreshWithState(state);

		return rootll;
	}
/*
	@Override
	public void onPause() {
		super.onPause();
		if(rrFragment != null) {
			if(rrFragment.isAdded()) {
				this.getFragmentManager().beginTransaction()
					.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_CLOSE)
					.remove(rrFragment)
					.commit();
			}
		}
	}

	@Override
	public void onResume() {
		super.onResume();
		if(rrFragment == null) return;

		if(!rrFragment.isAdded()) {
			System.out.println("in onResume: not added");
			this.getFragmentManager().beginTransaction()
				.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN)
				.add(rootid, rrFragment, rrTag)
				.commit();
		} else if(!rrFragment.isVisible()) {
			System.out.println("in onResume: not visisble");
			this.getFragmentManager().beginTransaction()
				.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN)
				.show(rrFragment)
				.commit();
		}
	}
*/
	private void doRefresh( ) {
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
	}	

	private List<Report> externalReports;
	private QuerySpec pendingQuery;

	private DialogInterface.OnClickListener reportItemsListener =
		new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface arg0, int reportItem) {
				if(externalReports != null) {
					Report selectedReport = externalReports.get(reportItem);

					LinearLayout configureView =
						new LinearLayout(mContext);
					configureView.setOrientation(LinearLayout.VERTICAL);

					TextView descrHeader = new TextView(mContext);
					descrHeader.setText(Html.fromHtml("<b>Report description</b>"));
					descrHeader.setTextSize(18);
					configureView.addView(descrHeader);

					TextView desc = new TextView(mContext);
					desc.setText(selectedReport.getDescription());
					configureView.addView(desc);

					// parameters
					View rule = new View(mContext);
					rule.setBackgroundColor(Color.GRAY);
					configureView.addView(rule, new LinearLayout.LayoutParams(
								LinearLayout.LayoutParams.MATCH_PARENT,
								3));

					TextView confHeader = new TextView(mContext);
					confHeader.setTextSize(18);
					confHeader.setText(Html.fromHtml("<b>Enter parameters</b>"));
					configureView.addView(confHeader);

					pendingQuery = new QuerySpec(selectedReport);
					for(Type argType : selectedReport.getType().getArgTypes()) {
						PoetsType pType = RecordDecode.decodeType(argType);

						ConstantArgSpec cas = new ConstantArgSpec(pType);
						pendingQuery.addArgument(cas);
						configureView.addView(cas.getView(mContext));
					}

					AlertDialog ad = 
						new AlertDialog.Builder(mContext)
						.setTitle("Configure " + selectedReport.getName())
						.setView(configureView)
						.setPositiveButton("Add report", addListener)
						.setCancelable(true)
						.create();
					ad.show();
				}
			}
		};

	DialogInterface.OnClickListener addListener =
		new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int arg1) {
				if(pendingQuery != null) {
					// pendingQuery is the one to add to the list af
					// availableQueries (to be persisted somehow)
					// FIXME: Temporary 'solution'
					pendingQuery.runQuery(new Utils.RunWithArg<PoetsValue>() {
						@Override
						public void run(PoetsValue t) {
							if(t != null) {
								((ReportResultFragment)rrFragment).refreshValue(t);
							}
						}
					});
				}
			}
		};

	// When a report is 'added' it is available for the user to query.
	// The user queries the report by tapping its name in the left-most
	// column. To add a report the user must specify input parameters to
	// be used when performing the actual query of the report.
	// Parameters are specified "symbolically" and then interpreted at
	// the time of report query.
	private void doAdd() {
		// query the server for a list of external reports available
		// present list of reports to user
		// based on report-type, make user specify arguments to the report
		// add report to 'added reports column' and
		// select newly added report thereby forcing a query
		//
		
		ListV none = new ListV(new StringV(""));
		none.addElement(new StringV("internal"));
		ListV all = new ListV(new StringV("E"));
		List<PoetsValue> args = new ArrayList<PoetsValue>(2);
		args.add(all);
		args.add(none);

		final ProgressDialog pd = ProgressDialog.show(mContext, 
				"Contacting server", 
				"Fetching list of available reports", true, false);
		Utils.withQuery("ReportNamesByTags", args, new RunWithArg<PoetsValue>() {
			@Override
			public void run(PoetsValue t) {
				if(t != null) {
					final List<PoetsValue> names = ((ListV)t).val;
					(new AsyncTask<Void, Void, List<Report>>() {
						@Override
						protected List<Report> doInBackground(Void... arg0) {
							externalReports = new ArrayList<Report>();
							PoetsServer.Iface pServ = ServerUtils.getServer();
							for(PoetsValue pv : names) {
								if(pv instanceof StringV) {
									String name = ((StringV)pv).val;
									try {
										externalReports.add(pServ.getReport(name));
									} catch (Exception e) {
										e.printStackTrace();
									} 
								}
							}
							return externalReports;
						}
						@Override
						protected void onPostExecute(List<Report> reports) {
							if(reports != null) {
								pd.dismiss();
								List<String> namesList = new ArrayList<String>();
								for(Report rep : reports) {
									namesList.add(rep.getName());
								}
								AlertDialog ad =
									new AlertDialog.Builder(mContext)
									.setTitle("Select report to add")
									.setCancelable(true)
									.setItems(namesList.toArray(new String[namesList.size()]), reportItemsListener)
									.create();
								ad.show();
							}
						}
					}).execute();				
				}
			}
		});
	}

	public boolean onOptionsItemSelected(MenuItem item) {
		switch(item.getItemId()) {
			case refreshId:
				doRefresh();
				return true;
			case addId:
				doAdd();
				return true;
			default:
				break;
		} 
		return false;
	}
	
	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater infl) {

		menu.add(Menu.NONE, refreshId, Menu.NONE, "Refresh");
		
		menu
			.add(Menu.NONE, addId, Menu.NONE, "Add report")
			.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

	}

	private void refreshWithState(Bundle state) {
		// construct UI here
		// the UI should basically be a split-view; one is a list of all
		// added reports and the other shows dtails of the report
		// currently selected (if any); 
		// we should put both views into fragments because then we can
		// potentially reuse them later on in other contexts
		FragmentManager fm = this.getFragmentManager();
		System.out.println("CON10: state = " + state + " rootll = " + rootll + " rrFragment = " + rrFragment);
		if(rootll == null) {
			rootll = new LinearLayout(mContext);
		} 
		rootll.setId(rootid);
		
		rrFragment = rrFragment==null?fm.findFragmentByTag(rrTag):rrFragment;
		
		if(rrFragment == null) {
			rrFragment = new ReportResultFragment();
		}
		
		if(!rrFragment.isAdded()) {
			System.out.println("CON10: not added");
			fm.beginTransaction()
				.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN)
				.add(rootid, rrFragment, rrTag)
				.commit();
		} else if(!rrFragment.isVisible()) {
			System.out.println("CON10: not visible (added? "+rrFragment.isAdded()+")");
			fm.beginTransaction()
				.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN)
				.show(rrFragment)
				.commit();
		} else {
			System.out.println("CON10: nothing to do?");
		}	
		
		
	}

	@Override
	public void refresh() {
		refreshWithState(null);
	}

}
