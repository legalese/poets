package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import dk.diku.poets.record.PoetsValue;

public class ViewSpecGetter {

	public static ViewSpec getViewSpec(PoetsValue pv) {
		if(pv == null) return null;

		for(ViewSpec vs : viewSpecs) {
			if(vs.checkPre(pv)) {
				return vs.setValue(pv);
			}
		}
		return new NoViewSpec().setValue(pv);
	}
	
	
	@SuppressWarnings("serial")
	private static List<ViewSpec> viewSpecs =
		new ArrayList<ViewSpec>() {{
			add(ViewSpecData.MeRec);
			add(ViewSpecData.ChildViewSpec);
			add(ViewSpecData.AdultViewSpec); 
			add(ViewSpecData.PutContractBirthdayViewSpec); 
			//^^covers create+update of birthday
			add(ViewSpecData.PutContractGenericViewSpec); 
			//^^covers create+update of generic
			add(ViewSpecData.ArrangementView);
			add(ViewSpecData.GenericListViewSpec);
			//^^generic display of lists
			add(ViewSpecData.GenericRefViewSpec);
			add(ViewSpecData.GenericAtomicViewSpec);
			add(ReportViewSpecs.ScheduleViewSpec);
			add(ReportViewSpecs.MonthlyOverviewSpec);
			add(ReportViewSpecs.InventoryViewSpec);
			add(ReportViewSpecs.UnpaidInvoicesViewSpec);
			add(ReportViewSpecs.GenericReportViewSpec);

		}};
}
