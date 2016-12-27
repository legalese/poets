package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;

import android.widget.HorizontalScrollView;
import android.widget.LinearLayout;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.ListV;
import dk.diku.poets.record.PoetsValue.RecV;

public class ListViewSpec extends ViewSpec {

	static public class TypeIndexed extends ListViewSpec {
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
					try {
						String typeName = Utils.get((RecV)((ListV)pv).getElementType()).getName();
						for(String supportedType : supportedTypes) {
							if(supportedType.equals(typeName)) {
								return true;
							}
						}
						return false;
					} catch (Exception e) {
						return false;
					}
				}
			});
			return superPre;
		}
	}

	@Override
	protected List<PreCond> getPre() {
		List<PreCond> pres = new ArrayList<PreCond>();
		pres.add(new PreCond() {
			@Override
			public boolean check(PoetsValue pv) {
				return pv instanceof ListV;
			}
		});
		return pres;
	}

	@Override
	public View getView(Context mContext, EmbedAs embedding) {
		ViewGroup root;
		LinearLayout ll = new LinearLayout(mContext);
		if(embedding == EmbedAs.FULL) {
			ll.setOrientation(LinearLayout.VERTICAL);
			root = ll;
		} else {
			HorizontalScrollView hsv = 
				new HorizontalScrollView(mContext);
			hsv.addView(ll);
			root = hsv;
		}

		List<PoetsValue> valList = ((ListV)pVal).val;
		for(PoetsValue listVal : valList) {
			ViewSpec vs = ViewSpecGetter.getViewSpec(listVal);
			View v = vs.getView(mContext, embedding);
			if(embedding == EmbedAs.TILE) {
				v.setPadding(
						v.getPaddingLeft(),
						v.getPaddingTop(),
						7,
						v.getPaddingBottom());
			}
			ll.addView(v,
					new LinearLayout.LayoutParams(
						LinearLayout.LayoutParams.MATCH_PARENT,
						LinearLayout.LayoutParams.WRAP_CONTENT));
		}
		return root;
	}
}
