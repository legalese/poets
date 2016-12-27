package dk.diku.poets.android.tablet;

import java.util.Map.Entry;

import android.content.Context;
import android.os.AsyncTask;

import android.text.InputType;
import android.view.View;

import android.widget.EditText;

import dk.diku.poets.gen.thrift.data.FieldDefinition;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.DoubleV;

public class DoubleInput extends InputWidget {
	private EditText tv;
	private Entry<String, FieldDefinition> entry;
	private HumaniseCamelCase hcc = new HumaniseCamelCase();

	public DoubleInput(Entry<String, FieldDefinition> ent) {
		entry = ent;
	}

	@Override
	public PoetsValue getValue() {
	    try {
		double i = Double.parseDouble(tv.getText().toString());
		return (PoetsValue) new DoubleV(i);
	    } catch (Exception e) {
	    }
	    return (PoetsValue) new DoubleV(0);
	}

	@Override
	protected View getView(Context c) {
		tv = new EditText(c);
		tv.setTextSize(24);
		tv.setHint("Tap to enter " + hcc.humanise(entry.getKey()));
		tv.setInputType(
				InputType.TYPE_CLASS_NUMBER |
				InputType.TYPE_NUMBER_FLAG_DECIMAL);

		return tv;
	}

	@Override
	public void fill(PoetsValue pv) {
		DoubleV dv = (DoubleV) pv;
		tv.setText(Double.toString(dv.val));
	}
}

