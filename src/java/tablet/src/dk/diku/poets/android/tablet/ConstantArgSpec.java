package dk.diku.poets.android.tablet;

import android.content.Context;

import android.os.AsyncTask;

import android.view.View;

import dk.diku.poets.record.PoetsType;
import dk.diku.poets.record.PoetsValue;

public class ConstantArgSpec 
	implements QuerySpec.ArgSpec, Input 
{

	private PoetsType pType;
	private Input input;

	public ConstantArgSpec(PoetsType pT) {
		pType = pT;
	}

	public View getView(Context mContext) {
		View v = new InputSpec.Builder(mContext)
			.setType(pType)
			.create();
		input = (Input)v;
		return v;
	}

	@Override
	public PoetsValue eval() {
		try {
		    return input.getValue();
		} catch(Exception e) {
			System.out.println("CON10: ConstantArgSpec Exception:");
			e.printStackTrace();
			return null;
		}
	}

	@Override
	public PoetsValue getValue() {
		return input.getValue();
	}

	@Override
	public void fill(PoetsValue pv) {
		input.fill(pv);
	}
}
