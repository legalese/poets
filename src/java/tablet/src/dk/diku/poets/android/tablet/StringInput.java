package dk.diku.poets.android.tablet;

import java.util.Map.Entry;

import android.content.Context;

import android.os.AsyncTask;

import android.text.InputType;

import android.view.View;

import android.widget.EditText;

import dk.diku.poets.gen.thrift.data.FieldDefinition;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.StringV;

public class StringInput extends InputWidget {
	private EditText ed;
	private Entry<String, FieldDefinition> entry;
	private HumaniseCamelCase hcc = new HumaniseCamelCase();
	private StringV prefill;

	public StringInput(Entry<String, FieldDefinition> e) {
		entry = e;
	}

	@Override
	public PoetsValue getValue() {
	    return (PoetsValue) new StringV(ed.getText().toString().trim());
	}

	@Override
		protected View getView(Context c) {
			ed = new EditText(c);
			ed.setTextSize(24);
			ed.setHint("Tap to enter " + hcc.humanise(entry.getKey()));
			ed.setInputType(InputType.TYPE_CLASS_TEXT);
			if(prefill != null) {
				ed.setText(prefill.val);
			}
			return ed;
		}

	@Override
	public void fill(PoetsValue pv) {
		prefill = (StringV) pv;
		ed.setText(prefill.val);
	}
}


