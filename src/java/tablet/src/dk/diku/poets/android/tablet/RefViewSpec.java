package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.view.View;

import android.widget.LinearLayout;
import android.widget.TextView;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.RefV;

public class RefViewSpec extends ViewSpec {

	@Override
	protected List<PreCond> getPre() {
		List<PreCond> pres = new ArrayList<PreCond>(1);
		pres.add(new PreCond() {
			@Override
			public boolean check(PoetsValue pv) {
				return pv instanceof RefV;
			}
		});

		return pres;
	}

	@SuppressWarnings("serial")
	@Override
	public View getView(final Context mContext, EmbedAs embedding) {
		final LinearLayout ll = new LinearLayout(mContext);
		final TextView pleaseWait = new TextView(mContext);
		pleaseWait.setText("...");

		ll.addView(pleaseWait);
		final RefV ref = (RefV)pVal;
		Utils.withQuery("GetEntity", new ArrayList<PoetsValue>() {{add(ref);}},
				new Utils.RunWithArg<PoetsValue>() {
					@Override
					public void run(PoetsValue t) {
						if(t != null) {
							ViewSpec vs = ViewSpecGetter.getViewSpec(t);
							ll.removeView(pleaseWait);
							ll.addView(vs.getView(mContext, ViewSpec.EmbedAs.TILE));
						}
					}
				});
		return ll;
	}
}
