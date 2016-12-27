package dk.diku.poets.android.tablet;

import android.os.AsyncTask;

import dk.diku.poets.record.PoetsValue;

public interface Input {
	public PoetsValue getValue();

	public void fill(PoetsValue pv);
}
