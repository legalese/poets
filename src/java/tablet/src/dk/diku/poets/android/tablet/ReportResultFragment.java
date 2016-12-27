package dk.diku.poets.android.tablet;

import android.app.Fragment;

import android.content.Context;

import android.os.Bundle;

import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import android.widget.HorizontalScrollView;
import android.widget.LinearLayout;
import android.widget.TextView;

import dk.diku.poets.record.PoetsValue;

public class ReportResultFragment extends Fragment
	implements POETS.Refresh {


	private Context mContext;

	private TextView tmpView;
	private LinearLayout ll;

	// performs a query of a report and shows the result in the view
	public void queryReport() {
	}

	@Override
	public View onCreateView(LayoutInflater infl, ViewGroup container, Bundle state) {
		mContext = getActivity();
		refreshText();
		ll = new LinearLayout(mContext);
		ll.setLayoutParams(new ViewGroup.LayoutParams(
					ViewGroup.LayoutParams.MATCH_PARENT,
					ViewGroup.LayoutParams.WRAP_CONTENT));
		return ll;
	}

	private void refreshText() {
		if(ll == null) {
			ll = new LinearLayout(mContext);
		}
		if(mContext == null) {
			mContext = getActivity();
		}
		if(tmpView == null) {
			tmpView = new TextView(mContext);
		}
		ll.removeAllViews();
		ll.addView(tmpView);
		tmpView.setText("Tap 'Add report'");
	}

	// refresh the view by re-quering the report re-interpreting the
	// arguments given to the report
	@Override
	public void refresh() {
		refreshText();
	}

	public void refreshValue(PoetsValue pv) {
		ll.removeAllViews();
		LinearLayout.LayoutParams lp = new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT);
		lp.gravity = Gravity.CENTER;
		View v = ViewSpecGetter.getViewSpec(pv).getView(mContext, ViewSpec.EmbedAs.FULL);
		HorizontalScrollView hsv = new HorizontalScrollView(mContext);
		hsv.addView(v);
		ll.addView(hsv, lp);
	}
}
