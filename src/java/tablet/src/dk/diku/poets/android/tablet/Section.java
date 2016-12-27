package dk.diku.poets.android.tablet;

import android.animation.LayoutTransition;

import android.content.Context;

import android.graphics.Color;

import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;

import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;

public class Section <T> extends FrameLayout {

	public static interface MkView <T> {
		public abstract View mk(T data);
	}

	protected Context mContext;
	private View overlay;
	private View header;
	private LinearLayout ll;
	private MkView<T> mk;

	public static void addRuler(Context mContext, LinearLayout ll) {
		View ruler = new View(mContext);
		ruler.setBackgroundColor(Color.argb(0xff, 0x10, 0x10, 0x30));
		ll.addView(ruler, new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT, 2));
	}

	public static View makeSectionHeader(Context mContext, String itemNum, String itemText) {
		LinearLayout ll = new LinearLayout(mContext);
		TextView num1 = new TextView(mContext);
		num1.setText(itemNum);
		num1.setTextSize(48);
		num1.setGravity(Gravity.CENTER);
		num1.setPadding(5,5,5,5);
		
		TextView selT = new TextView(mContext);
		selT.setText(itemText);
		selT.setTextSize(24);

		View ruler = new View(mContext);
		ruler.setBackgroundColor(Color.argb(0xff, 0x10, 0x10, 0x30));

		ll.addView(num1);
		ll.addView(ruler, new LinearLayout.LayoutParams(
					8, LinearLayout.LayoutParams.MATCH_PARENT));
		ll.addView(selT);

		return ll;
	}

	public Section(Context c, View h) {
		super(c);
		mContext = c;
		header = h;

		ll = new LinearLayout(mContext);
		ll.setOrientation(LinearLayout.VERTICAL);
		ll.setLayoutTransition(new LayoutTransition());
		ll.addView(header);
		addView(ll, 0);
		disable();
	}

	public void setMK(MkView<T> mker) {
		this.mk = mker;
	}

	public void enable(T data) {
	//	, MkView<T> mker) {
		removeView(overlay);
		overlay = null;
		if(ll.getChildCount() > 1) {
			// we remove the previous views if the section is being
			// reenabled
			ll.removeAllViews();
			ll.addView(header);
		}
		if(mk != null) {
			ll.addView(mk.mk(data));
		}
	}

	@Override
	public boolean onInterceptTouchEvent(MotionEvent e) {
		if(overlay == null) {
			return super.onInterceptTouchEvent(e);
		} else {
			return true;
		}
	}

	public void disable() {
		overlay = new View(mContext);
		overlay.setBackgroundColor(Color.argb(0x90,0x00,0x00,0x00));
		addView(overlay, 1, new LayoutParams(
					LayoutParams.MATCH_PARENT,
					LayoutParams.MATCH_PARENT));
	}
}
