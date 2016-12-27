package dk.diku.poets.android.tablet;

import android.content.Context;

import android.graphics.Color;

import android.view.View;

import android.widget.LinearLayout;
import android.widget.TextView;

public class SelectBox extends LinearLayout {
	private Context mContext;
	private View selectView;
	private TextView statusText;
	private boolean enabled;

	public SelectBox(Context ctx, View v) {
		super(ctx);
		mContext = ctx;
		setOrientation(LinearLayout.VERTICAL);
		selectView = v;
		setPadding(10,10,10,10);
		statusText = new TextView(mContext);
		statusText.setTextSize(18);
		addView(selectView);
		addView(statusText);
		setEnabled(false);
		setBackgroundResource(R.drawable.border);
	}

	public void setEnabled(boolean enable) {
		enabled = enable;
		if(enabled) {
			statusText.setText("Selected");
			setBackgroundColor(Color.argb(0x90, 0x4d, 0x4d, 0xcf));
		} else {
			statusText.setText("Tap here to select");
			setBackgroundColor(Color.TRANSPARENT);
		}
	}

}
