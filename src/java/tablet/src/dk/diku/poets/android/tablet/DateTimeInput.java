package dk.diku.poets.android.tablet;

import java.util.Map.Entry;

import android.app.DatePickerDialog;
import android.app.TimePickerDialog;

import android.content.Context;

import android.os.AsyncTask;

import android.view.View;

import android.widget.Button;
import android.widget.DatePicker;
import android.widget.LinearLayout;
import android.widget.TimePicker;

import dk.diku.poets.gen.thrift.data.FieldDefinition;

import dk.diku.poets.record.Calendar;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.DateTimeV;

public class DateTimeInput extends InputWidget implements 
TimePickerDialog.OnTimeSetListener, View.OnClickListener,
	DatePickerDialog.OnDateSetListener {

		LinearLayout ll;
		Button timeb, dateb;
		TimePickerDialog tpd;
		DatePickerDialog dpd;
		Calendar cal;	

		public DateTimeInput(Entry<String, FieldDefinition> e) {
			cal = Calendar.getInstance();
		}

		@Override
			public PoetsValue getValue() {
				if(cal != null) {
				    return (PoetsValue) new DateTimeV(cal);
				}
				return null;
			}

		@Override
			public void onClick(View v) {
				if(v == timeb) {
					tpd.show();
				}	else if(v == dateb) {
					dpd.show();
				}		
			}

		@Override
			protected View getView(Context c) {
				tpd = new TimePickerDialog(c, this, cal.get(Calendar.HOUR), cal.get(Calendar.MINUTE), true);
				dpd = new DatePickerDialog(c, this, 
						cal.get(Calendar.YEAR), cal.get(Calendar.MONTH) - 1, cal.get(Calendar.DAY_OF_MONTH));

				ll = new LinearLayout(c);

				dateb = new Button(c);
				dateb.setTextSize(24);
				dateb.setText("Tap to set date");
				dateb.setPadding(dateb.getPaddingLeft(), dateb.getPaddingTop(), 10, dateb.getPaddingBottom());
				ll.addView(dateb);
				dateb.setOnClickListener(this);


				timeb = new Button(c);
				timeb.setTextSize(24);
				timeb.setText("Tap to set time");
				ll.addView(timeb);
				timeb.setOnClickListener(this);

				fill(new DateTimeV(cal));
				return ll;
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
			public void onDateSet(DatePicker arg0, int year, int month, int day) {
				cal.set(Calendar.YEAR, year);
				cal.set(Calendar.MONTH, month + 1);  // month is 0 indexed in DatePicker
				cal.set(Calendar.DAY_OF_MONTH, day);
				dateb.setText(cal.get(Calendar.DAY_OF_MONTH) + "-" + cal.get(Calendar.MONTH) + "-" + cal.get(Calendar.YEAR));
			}

		@Override
		public void fill(PoetsValue pv) {
			DateTimeV dtv = (DateTimeV)pv;
			onTimeSet(null, dtv.val.get(Calendar.HOUR), dtv.val.get(Calendar.MINUTE));
			onDateSet(null, 
					dtv.val.get(Calendar.YEAR),
					dtv.val.get(Calendar.MONTH) - 1,
					dtv.val.get(Calendar.DAY_OF_MONTH));
		}
}

