package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import android.graphics.Typeface;

import dk.diku.poets.android.tablet.DSL;
import dk.diku.poets.android.tablet.Utils;

public class DSLHTMLConverter implements DSLVisitor {
	private interface StringContainer {
		public String getString();
	}

	private List<StringContainer> container;

	private void mk(final String s) {
		StringContainer sc = new StringContainer() {
			public String getString() {
				return s;
			}
		};
		container.add(sc);
	}

	public DSLHTMLConverter() {
		container = new ArrayList<StringContainer>();
	}

	public void visit(DSL.PContainer pc) {
		switch(pc.type) {
			case NextTo:
				mk("<ul style=\"list-style-type:none; margin: 0; padding: 0;\">");
				for(DSL.PView child : pc.elements) {
					mk("<li style=\"display:inline;\">");
					child.accept(this);
					mk("</li>");
				}
				mk("</ul>");
				break;
			case OnTop:
				mk("<div>");
				for(DSL.PView child : pc.elements) {
					child.accept(this);
				}
				mk("</div>");
				break;
			case Row:
				mk("<tr>");
				for(DSL.PView child : pc.elements) {
					mk("<td>");
					child.accept(this);
					mk("</td>");
				}
				mk("</tr>");
				break;
			case Table:
				mk("<table frame=\"box\" cellspacing=\"10\" rules=\"rows\">");
				for(DSL.PView child : pc.elements) {
					child.accept(this);
				}
				mk("</table>");
				break;
			default:
				throw new RuntimeException(
						"Unknown PContainer-type to convert" + pc);
		}
	}
	public void visit(DSL.PText pt) {
		String txt = pt.text;
		if(pt.typeface == Typeface.DEFAULT_BOLD) {
			txt = "<b>"+txt+"</b>";
		}
		mk("<p style=\"font-size:"+pt.size+";\">");mk(txt);mk("</p>");
	}
	public void visit(final DSL.PQuery pq) {
		container.add(new StringContainer() {
			public String getString() {
				return pq.cont.get(Utils.withQuery(pq.report, pq.args)).getHtml();
			}
		});
	}
	public void visit(final DSL.PExt pe){
		container.add(new StringContainer() {
			public String getString() {
				return pe.cont.get(pe.pVal).getHtml();
			}
		});
	}

	// Don't run this on the UI thread!
	public String getHtml() {
		StringBuffer sb = new StringBuffer();
		for(StringContainer sc : container) {
			sb.append(sc.getString());
		}
		return "<html><body>"+sb.toString()+"</body></html>";
	}
}


