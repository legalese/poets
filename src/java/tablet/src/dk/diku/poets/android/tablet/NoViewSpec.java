package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.view.View;

import android.widget.TextView;

public class NoViewSpec extends ViewSpec {

	@Override
	protected List<PreCond> getPre() {
		return new ArrayList<PreCond>();
	}

	@Override
	public View getView(Context mContext, EmbedAs embedding) {
		TextView noView = new TextView(mContext);
		noView.setText("No view for: " + pVal);
		return noView;
	}
}
