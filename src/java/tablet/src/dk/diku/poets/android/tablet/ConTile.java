package dk.diku.poets.android.tablet;

import java.util.List;

import android.content.Context;

import android.widget.RelativeLayout;

import dk.diku.poets.android.tablet.ViewSpec;

import dk.diku.poets.gen.thrift.contracts.TransactionPattern;

import dk.diku.poets.record.PoetsValue.IntV;
import dk.diku.poets.record.PoetsValue.RecV;

public class ConTile extends RelativeLayout {

	private Context mContext;
	private RecV putC;
	private List<TransactionPattern> tps;
	private int cid;

	public ConTile(Context c, RecV putC, List<TransactionPattern> currentTPs) {
		super(c);
		mContext = c;
		this.putC = putC.getIsAbstract()?putC.getInstance():putC;
		tps = currentTPs;
		cid = ((IntV)Utils.getValueFromPath(putC, "contractId")).val;


		setConTileDetails();
	}

	private void setConTileDetails() {
		addView(ViewSpecGetter.getViewSpec(putC).getView(mContext,ViewSpec.EmbedAs.TILE));
	}

}
