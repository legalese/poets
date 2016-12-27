package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;

import android.text.Html;
import android.view.View;

import android.webkit.WebView;

import android.widget.LinearLayout;
import android.widget.TextView;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.RecV;

public abstract class RecViewSpec extends ViewSpec {
	@SuppressWarnings("serial")
	@Override
	protected List<PreCond> getPre() {
		return new ArrayList<PreCond>() {{
			add(new PreCond() {
				@Override
				public boolean check(PoetsValue pv) {
					return pv instanceof RecV;
				}
			});
		}};
	}

	public static abstract class TypeIndexed extends RecViewSpec {
		private String[] supportedTypes;
		public TypeIndexed(String... supportedTypes) {
			this.supportedTypes = supportedTypes;
		}

		public TypeIndexed(String oneType) {
			supportedTypes = new String[] {oneType};
		}

		@Override
		protected List<PreCond> getPre() {
			List<PreCond> superPre = super.getPre();
			superPre.add(new PreCond() {
				@Override
				public boolean check(PoetsValue pv) {
					String recName = Utils.get((RecV) pv).getName();
					for(String supportedType : supportedTypes) {
						if(supportedType.equals(recName)) {
							return true;
						}
					}
					return false;
				}
			});
			return superPre;
		}
	}

	public static abstract class FieldIndexed extends TypeIndexed {
		private List<String[]> requiredFields;
		public FieldIndexed(String[] types, List<String[]> reqFs) {
			super(types);
			requiredFields = reqFs;
		}
		public FieldIndexed(String type, List<String[]> reqFs) {
			super(type);
			requiredFields = reqFs;
		}
		public FieldIndexed(String[] types, String[] reqFs) {
			super(types);
			requiredFields = new ArrayList<String[]>(1);
			requiredFields.add(reqFs);
		}
		public FieldIndexed(String type, String[] reqFs) {
			super(type);
			requiredFields = new ArrayList<String[]>(1);
			requiredFields.add(reqFs);
		}

		public static class FieldPair {
			public String[] path;
			public PoetsValue pathVal;
			public FieldPair(String[] path, PoetsValue pVal) {
				this.path = path;
				this.pathVal = pVal;
			}
		}

		public List<FieldPair> getFieldValues() {
			List<FieldPair> fieldVals = new ArrayList<FieldPair>();
			RecV recV = Utils.get((RecV)this.pVal);
			for(String[] reqField : requiredFields) {
				PoetsValue val = Utils.getValueFromPath(recV, reqField);
				fieldVals.add(new FieldPair(reqField, val));
			}

			return fieldVals;
		}

		@Override 
		protected List<PreCond> getPre() {
			List<PreCond> superPre = super.getPre();
			superPre.add(new PreCond() {
				@Override
				public boolean check(PoetsValue pv) {
					RecV rec = (RecV) pv;
					for(String[] path : requiredFields) {
						PoetsValue tmppv = Utils.getValueFromPath(rec, path);
						if(tmppv == null) {
							return false;
						}
					}
					return true;
				}
			});
			return superPre;
		}
	}

	public static class DeclarativeRecViewSpec extends FieldIndexed {
		
		private HumaniseCamelCase hcc;

		public DeclarativeRecViewSpec(String[] types, List<String[]> reqFs) {
			super(types, reqFs);
			hcc = new HumaniseCamelCase();
		}

		@Override
		public View getView(Context mContext, EmbedAs embedding) {
			LinearLayout ll = new LinearLayout(mContext);
			ll.setOrientation(LinearLayout.VERTICAL);
	
			for(FieldPair fPair : getFieldValues()) {
				String label = hcc.humanise(fPair.path[fPair.path.length - 1]);
				LinearLayout fieldContainer = new LinearLayout(mContext);
				fieldContainer.setOrientation(LinearLayout.VERTICAL);
				TextView labelView = new TextView(mContext);
				labelView.setText(Html.fromHtml("<b>" + label + ":</b> "));
				labelView.setTextSize(16);

				ViewSpec vs = ViewSpecGetter.getViewSpec(fPair.pathVal);

				fieldContainer.addView(labelView);
				fieldContainer.addView(vs.getView(mContext, ViewSpec.EmbedAs.TILE));

				ll.addView(fieldContainer, new LinearLayout.LayoutParams(
							LinearLayout.LayoutParams.MATCH_PARENT,
							LinearLayout.LayoutParams.WRAP_CONTENT));
			}

			return ll;
		}

	}

	public static class HtmlRecView extends FieldIndexed {
		protected String templateString;

		public HtmlRecView(String templateString, String[] types, List<String[]> reqFs) {
			super(types, reqFs);
			this.templateString= templateString;
		}


		private String getReplacement(PoetsValue pValue) {
			String replacementString;
			if(Utils.isAtomic(pValue)) {
				replacementString = pValue.toString();
			} else {
				replacementString = "TODO("+pValue.toString()+")";
			}
			return replacementString;
		}

		private String getTarget(String[] path) {
			String targetString = "${";
			for(int i = 0; i < path.length - 1; i++) {
				targetString += path[i] + ".";
			}
			targetString += path[path.length - 1] + "}";
			return targetString;
		}

		private String makeHTML() {
			//replace variables mentioned in declared fields with
			//their corresponding value; TODO: something about toString and
			//availability of htmlviews for the values of fields 
			String htmlString = new String(templateString);
			for(FieldPair fPair : getFieldValues()) {
				String replacement = getReplacement(fPair.pathVal);
				String target = getTarget(fPair.path);
				htmlString = htmlString.replace(target, replacement);
			}
			return htmlString;
		}

		@Override
		public View getView(Context mContext, EmbedAs embedding) {
			WebView wv = new WebView(mContext);
			String htmlString = makeHTML();
			wv.loadData(htmlString, "text/html", "utf-8");
			return wv;
		}
	}

}
