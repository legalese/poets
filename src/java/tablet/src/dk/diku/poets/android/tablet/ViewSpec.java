package dk.diku.poets.android.tablet;

import java.util.List;

import android.content.Context;

import android.view.View;

import dk.diku.poets.record.PoetsValue;

public abstract class ViewSpec {

	public enum EmbedAs {
		FULL, TILE
	}

	protected PoetsValue pVal;

	protected ViewSpec setValue(PoetsValue pv) {
		pVal = pv;
		return this;
	}

	protected abstract List<PreCond> getPre();
	
	public boolean checkPre(PoetsValue pv) {
		for(PreCond pc : getPre()) {
			if(!pc.check(pv)) {
				return false;
			}
		}

		return true;
	}

	// should only every be called AFTER 'setValue'
	public abstract View getView(Context mContext, EmbedAs embedding);
}
