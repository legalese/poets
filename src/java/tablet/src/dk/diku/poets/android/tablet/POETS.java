package dk.diku.poets.android.tablet;

import android.app.ActionBar;

import android.app.ActionBar.Tab;
import android.app.ActionBar.TabListener;
import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentTransaction;

import android.content.Context;

import android.os.Bundle;

import android.widget.LinearLayout;

import dk.diku.poets.poetsserver.ServerUtils;

public class POETS extends Activity
{
	Context mContext;
	LinearLayout rootll;
	private static final int rootid = 42;
		
	
	public interface Refresh {
		public void refresh();
	}


	public static class TListener implements TabListener {
		private Fragment mFragment;
		private String tag;

		public TListener(Fragment tcf, String t) {
			mFragment = tcf;
			tag = t;
		}

		@Override
		public void onTabReselected(Tab arg0, FragmentTransaction arg1) {
			System.out.println("CON10: REselected tab <" + tag + ">");
			if(mFragment instanceof Refresh) {
				((Refresh)mFragment).refresh();
			}
		}

		@Override
		public void onTabSelected(Tab arg0, FragmentTransaction ft) {
			System.out.println("CON10: SELECTED tab <" + tag + ">");
			if(!mFragment.isAdded()) {
				System.out.println("CON10: adding selected tab");
				ft.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN);
				ft.add(rootid, mFragment, tag);
			}
		}

		@Override
		public void onTabUnselected(Tab arg0, FragmentTransaction ft) {
			System.out.println("CON10: onTabUnselect <"+tag+">");
			if(mFragment.isAdded()) {
				System.out.println("CON10: removing <"+tag+">");
				ft.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_CLOSE);
				ft.remove(mFragment);
			} 
		}
	}

	private Fragment consFragment;
	private Fragment repsFragment;

	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putInt("curTab", getActionBar().getSelectedNavigationIndex());
	}

	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		mContext = this;

		//ServerUtils.setIp(192,168,168,104); // IP at Legejunglen
		//ServerUtils.setIp(192,38,115,208); // IP of 3gerp.ekstranet.diku.dk
		//ServerUtils.setIp(10,61,32,15);
		ServerUtils.setIp(10,0,2,2);

		rootll = new LinearLayout(mContext);
		rootll.setId(rootid);
		setContentView(rootll);

		// setup Action Bar for tabs
    final ActionBar actionBar = getActionBar();
    actionBar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);
    // remove the activity title to make space for tabs
    actionBar.setDisplayShowTitleEnabled(false);		
		
    // instantiate fragment for the tab
    if(savedInstanceState == null) {
			consFragment = new ContractsFragment();
			repsFragment = new ReportsFragment();
		} else {
			consFragment = this.getFragmentManager().findFragmentByTag("ContractsFragTag");
			if(consFragment == null) {
				consFragment = new ContractsFragment();
			}
			repsFragment = this.getFragmentManager().findFragmentByTag("ReportsFragTag");
			if(repsFragment== null) {
				repsFragment = new ReportsFragment();
			}
		}

    // add a new tab and set its title text and tab listener
    actionBar.addTab(
				actionBar
				.newTab()
				.setText("Contracts")
				.setTabListener(new TListener(consFragment, "ContractsFragTag")), 0, false);
    
		// instantiate fragment for the tab
    // add a new tab and set its title text and tab listener
    actionBar.addTab(
				actionBar
				.newTab()
				.setText("Reports")
				.setTabListener(new TListener(repsFragment, "ReportsFragTag")), 1, false);

		if(savedInstanceState != null) {
			// restore last tab
			int curTab = savedInstanceState.getInt("curTab", 0);
			actionBar.setSelectedNavigationItem(curTab);
		} else {
			actionBar.setSelectedNavigationItem(0);
		}
		System.out.println("CON10: visible consFrag = " + consFragment.isVisible() +
				" visible repFrag = " + repsFragment.isVisible());
	}
}
