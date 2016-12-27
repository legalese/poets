package dk.diku.poets.android.tablet;

import java.util.Map.Entry;

import android.app.TimePickerDialog;

import android.content.Context;

import android.os.AsyncTask;

import android.view.View;

import android.widget.Button;
import android.widget.TimePicker;

import dk.diku.poets.gen.thrift.data.FieldDefinition;

import dk.diku.poets.record.Calendar;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.TimeV;

public class TimeInput extends InputWidget implements 
TimePickerDialog.OnTimeSetListener, View.OnClickListener {

		TimePickerDialog tpd;
		Button timeb;
		Calendar cal;	

		public TimeInput(Entry<String, FieldDefinition> e) {
			cal = Calendar.getInstance();
		}

		@Override
		public PoetsValue getValue() {
		    if(cal != null) {
			return (PoetsValue) new TimeV(cal);
		    }
		    return null;
		}

		@Override
			public void onClick(View v) {
				tpd.show();
			}

		@Override
			protected View getView(Context c) {
				tpd = new TimePickerDialog(c, this, cal.get(Calendar.HOUR), cal.get(Calendar.MINUTE), true);

				timeb = new Button(c);
				timeb.setTextSize(24);
				timeb.setText("Tap to set time");
				timeb.setOnClickListener(this);

				fill(new TimeV(cal));
				return timeb;
			}

		private String pad(int c) {
			if (c >= 10) {
				return String.valueOf(c);
			} else {
				return "0" + String.valueOf(c);
			}
		}

		@Override
			public void onTimeSet(TimePicker arg0, int hSet, int mSet) {
				cal.set(Calendar.HOUR, hSet);
				cal.set(Calendar.MINUTE, mSet);
				timeb.setText(pad(cal.get(Calendar.HOUR)) + ":" + pad(cal.get(Calendar.MINUTE)));
			}

		@Override
		public void fill(PoetsValue pv) {
			TimeV dtv = (TimeV)pv;
			onTimeSet(null, dtv.val.get(Calendar.HOUR), dtv.val.get(Calendar.MINUTE));
		}
}


