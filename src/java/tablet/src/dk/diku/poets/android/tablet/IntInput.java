package dk.diku.poets.android.tablet;

import java.util.Map.Entry;

import android.content.Context;
import android.os.AsyncTask;

import android.text.InputType;
import android.view.View;

import android.widget.EditText;

import dk.diku.poets.gen.thrift.data.FieldDefinition;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.IntV;

public class IntInput extends InputWidget {
	private EditText tv;
	private Entry<String, FieldDefinition> entry;
	private HumaniseCamelCase hcc = new HumaniseCamelCase();

	public IntInput(Entry<String, FieldDefinition> ent) {
		entry = ent;
	}

	@Override
	public PoetsValue getValue() {
	    try {
		int i = Integer.parseInt(tv.getText().toString());
		return (PoetsValue) new IntV(i);
	    } catch (Exception e) {}
	    return (PoetsValue) new IntV(0);
	}

	@Override
	protected View getView(Context c) {
		tv = new EditText(c);
		tv.setTextSize(24);
		tv.setHint("Tap to enter " + hcc.humanise(entry.getKey()));
		tv.setInputType(
				InputType.TYPE_CLASS_NUMBER |
				InputType.TYPE_NUMBER_FLAG_SIGNED);

		return tv;
	}

	@Override
	public void fill(PoetsValue pv) {
		IntV iv = (IntV) pv;
		tv.setText(Integer.toString(iv.val));
	}
}
