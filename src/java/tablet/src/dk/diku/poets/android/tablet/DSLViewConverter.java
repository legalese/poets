package dk.diku.poets.android.tablet;

import android.content.Context;

import android.text.util.Linkify;

import android.view.View;
import android.view.ViewGroup;

import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.TextView;

import dk.diku.poets.android.tablet.DSL;
import dk.diku.poets.android.tablet.Utils;

import dk.diku.poets.record.PoetsValue;

public class DSLViewConverter implements DSLVisitor {
	View view;
	Context ctx;
	public DSLViewConverter(Context ctx) {
		this.ctx = ctx;
	}
	public void visit(DSL.PContainer pc) {
		ViewGroup vg;
		switch(pc.type) {
			case NextTo:
				vg = new LinearLayout(ctx);
				((LinearLayout)vg).setOrientation(LinearLayout.HORIZONTAL);
				break;
			case OnTop:
				vg = new LinearLayout(ctx);
				((LinearLayout)vg).setOrientation(LinearLayout.VERTICAL);
				break;
			case Row:
				vg = new TableRow(ctx);
				((TableRow)vg).setBackgroundResource(R.drawable.border);
				break;
			case Table:
				vg = new TableLayout(ctx);
				break;
			default:
				throw new RuntimeException(
						"Unknown PContainer-type to convert" + pc);
		}
		for(DSL.PView child : pc.elements) {
			child.accept(this);
			vg.addView(view);
		}
		view = vg;
	}
	public void visit(DSL.PText pt) {
		TextView tv = new TextView(ctx);
		tv.setAutoLinkMask(Linkify.EMAIL_ADDRESSES);
		tv.setText(pt.text);
		tv.setPadding(7,7,7,7);
		tv.setTextSize(pt.size);
		tv.setTypeface(pt.typeface);
		view = tv;
	}
	public void visit(DSL.PQuery pq) {
		final DSL.Cont c = pq.cont;
		final FrameLayout fl = new FrameLayout(ctx);
		Utils.withQuery(pq.report, pq.args, new Utils.RunWithArg<PoetsValue>() {
			@Override
			public void run(PoetsValue t) {
				fl.addView(c.get(t).getView(ctx));
			}
		});
		view = fl;
	}
	public void visit(DSL.PExt pe){
		view = pe.cont.get(pe.pVal).getView(ctx);
	}
	public View getView() {
		if(view != null) {
			return view;
		} else {
			throw new RuntimeException("Something went wrong converting to View");
		}
	}
}


