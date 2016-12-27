package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import android.widget.TextView;

import dk.diku.poets.record.PoetsValue;

import android.content.Context;
import android.view.View;

public class AtomicViewSpec extends ViewSpec {

	@SuppressWarnings("serial")
	@Override
	protected List<PreCond> getPre() {
		return new ArrayList<PreCond>() {{
			add(new PreCond() {
				@Override
				public boolean check(PoetsValue pv) {
					return Utils.isAtomic(pv);
				}
			});
		}};
	}

	@Override
	public View getView(Context mContext, EmbedAs embedding) {
		TextView tv = new TextView(mContext);
		tv.setText(pVal.toString());
		tv.setTextSize(14);
		return tv;
	}
}
