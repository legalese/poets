package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;

import android.graphics.Typeface;

import android.view.View;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.DateTimeV;
import dk.diku.poets.record.PoetsValue.ListV;
import dk.diku.poets.record.PoetsValue.RecV;

public class DSL {

	private List<PView> roots;
	private PView curView;


	public static abstract class PView implements DSLElement {

	}

	public enum ContainerType { OnTop, NextTo, Table, Row };

	public static class PContainer extends PView {
		public ContainerType type;
		public List<PView> elements;
		public PContainer() {
			elements = new ArrayList<PView>();
		}
		public void accept(DSLVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class PText extends PView {
		public Typeface typeface;
		public Integer size;
		public String text;
		public void accept(DSLVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class PQuery extends PView {
		public String report;
		public List<PoetsValue> args;
		public Cont cont;
		public void accept(DSLVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class PExt extends PView {
		public Cont cont;
		public PoetsValue pVal;
		@Override
		public void accept(DSLVisitor visitor) {
			visitor.visit(this);
		}
	}

	protected interface Cont {
		public DSL get(PoetsValue pv);
	}

	protected PoetsValue pVal;

	public DSL() {
		roots = new ArrayList<PView>();
	}
	public DSL(PoetsValue val) {
		this();
		pVal = val;
	}

	protected void with(PoetsValue rec) {
		if(rec instanceof RecV) {
			this.pVal = Utils.get((RecV)rec);
		} else {
			this.pVal = rec;
		}
	}

	private void addVG(PContainer vg) {
		if(roots.size() == 0) {
			// new top-level
			roots.add(vg);
		} else {
			// add vg to previous container
			((PContainer)roots.get(0)).elements.add(vg);
			// make vg new ViewGroup-target
			roots.add(0, vg);
		}
	}

	private void addV(PView v) {
		if(roots.size() == 0) {
			roots.add(v);
		} else {
			((PContainer)roots.get(0)).elements.add(v);
		}
		curView = v;
	}

	protected void Table() {
		PContainer pc = new PContainer();
		pc.type = ContainerType.Table;
		addVG(pc);
	}

	protected void TableRow() {
		PContainer pc = new PContainer();
		pc.type = ContainerType.Row;
		addVG(pc);
	}

	protected void OnTop() {
		PContainer pc = new PContainer();
		pc.type = ContainerType.OnTop;
		addVG(pc);	
	}

	protected void NextTo() {
		PContainer pc = new PContainer();
		pc.type = ContainerType.NextTo;
		addVG(pc);	
	}

	protected void Ext(Cont cont, PoetsValue pv) {
		PExt pe = new PExt();
		pe.cont = cont;
		pe.pVal = pVal;
		addV(pe);
	}

	@SuppressWarnings("serial")
	protected void Ref(final PoetsValue ref, Cont cont) {
		Query("GetEntity", 
		      new ArrayList<PoetsValue>() {{ add(ref); }},
					cont
			   );
	}

	protected void Query(String report, List<PoetsValue> args, Cont cont) {
		PQuery pq = new PQuery();
		pq.report = report;
		pq.args = args;
		pq.cont = cont;
		addV(pq);
	}


	protected void Text(String t, int size, Typeface tf) {
		PText tv = new PText();
		tv.typeface = tf;
		tv.size = size;
		tv.text = t;
		addV(tv);
	}

	protected void Text(String t) {
		this.Text(t, 18, Typeface.DEFAULT);
	}

	protected void Text(PoetsValue txt) {
		this.Text(txt.toString(), 18, Typeface.DEFAULT);
	}

	protected void Bold(String txt) {
		this.Text(txt, 18, Typeface.DEFAULT_BOLD);
	}

	protected void Bold(PoetsValue txt) {
		this.Text(txt.toString(), 18, Typeface.DEFAULT_BOLD);
	}

	protected void End() {
		curView = roots.remove(0);
	}

	

	protected DateTimeV t(String field) {
		return ((DateTimeV)((RecV)pVal).getField(field));
	}

	protected PoetsValue f(String field) {
		return ((RecV)pVal).getField(field);
	}

	protected List<PoetsValue> l(String field) {
		return ((ListV)((RecV)pVal).getField(field)).val;
	}

	protected List<PoetsValue> l(RecV rec, String field) {
		return ((ListV)rec.getField(field)).val;
	}

	protected boolean isA(PoetsValue pVal, String s) {
		return
		 	(pVal instanceof RecV) &&
			((RecV)pVal).getName().equals(s);
	}

	protected boolean isA(String s) {
		return
		 	(pVal instanceof RecV) &&
			((RecV)pVal).getName().equals(s);
	}

	protected PoetsValue on(PoetsValue pv, String field) {
		return ((RecV)pv).getField(field);
	}

	private PView getPView() {
		PView pv;
		if(roots.size() == 1) {
			pv = roots.get(0);
		} else if(roots.size() == 0 && curView != null) {
			pv = curView;
		} else {
			throw new RuntimeException("No View available, either forgot to call End() or the top-level is strange" +
				 " [roots.size() = " + roots.size() + " curView = " + curView + "]");
		}
		return pv;
	}

	public View getView(Context ctx) {
		PView pv = getPView();
		DSLViewConverter vc = new DSLViewConverter(ctx);
		pv.accept(vc);
		return vc.getView();
		//return convertView(pv);
	}

	public String getHtml() {
		PView pv = getPView();
		DSLHTMLConverter vc = new DSLHTMLConverter();
		pv.accept(vc);
		return vc.getHtml();
	}

}

