package dk.diku.poets.android.tablet;

import java.util.Map.Entry;

import android.app.DatePickerDialog;

import android.content.Context;

import android.os.AsyncTask;

import android.view.View;

import android.widget.Button;
import android.widget.DatePicker;

import dk.diku.poets.gen.thrift.data.FieldDefinition;

import dk.diku.poets.record.Calendar;
import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.DateV;

public class DateInput extends InputWidget implements 
View.OnClickListener, DatePickerDialog.OnDateSetListener {

		Button dateb;
		DatePickerDialog dpd;
		Calendar cal;	

		public DateInput(Entry<String, FieldDefinition> e) {
			cal = Calendar.getInstance();
		}

		@Override
			public PoetsValue getValue() {
				if(cal != null) {
				    return new DateV(cal);
				}
				return null;
			}

		@Override
			public void onClick(View v) {
				dpd.show();
			}

		@Override
			protected View getView(Context c) {
				dpd = new DatePickerDialog(c, this, 
						cal.get(Calendar.YEAR), cal.get(Calendar.MONTH) - 1, cal.get(Calendar.DAY_OF_MONTH));

				dateb = new Button(c);
				dateb.setTextSize(24);
				dateb.setText("Tap to set date");
				dateb.setPadding(dateb.getPaddingLeft(), dateb.getPaddingTop(), 10, dateb.getPaddingBottom());
				dateb.setOnClickListener(this);


				fill(new DateV(cal));
				return dateb;
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
			DateV dtv = (DateV)pv;
			onDateSet(null, 
					dtv.val.get(Calendar.YEAR),
					dtv.val.get(Calendar.MONTH) - 1,
					dtv.val.get(Calendar.DAY_OF_MONTH));
		}
}


