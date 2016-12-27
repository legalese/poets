package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import android.content.Context;
import android.os.AsyncTask;
import android.view.View;

import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Spinner;
import dk.diku.poets.record.PoetsValue;

public class SimpleRestrictedInput extends InputWidget {

	private List<PoetsValue> values;
	private PoetsValue curSelection;

	public SimpleRestrictedInput(List<PoetsValue> values) {
		this.values = values;
	}

	@Override
	public PoetsValue getValue() {
	    return curSelection;
	}

	@Override
	public void fill(PoetsValue pv) {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected View getView(Context c) {
		Spinner spinner = new Spinner(c);
		ArrayAdapter<PoetsValue> adapter =
			new ArrayAdapter<PoetsValue>(c, android.R.layout.simple_spinner_dropdown_item,
					values.toArray(new PoetsValue[values.size()]));
		spinner.setAdapter(adapter);
		spinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {

			@Override
			public void onItemSelected(AdapterView<?> parent, View view,
					                       int pos, long id) 
			{
				curSelection = values.get(pos);	
			}

			@Override
			public void onNothingSelected(AdapterView<?> arg0) {
				// Do nothing on no selection
			}
		});

		return spinner;
	}
}



