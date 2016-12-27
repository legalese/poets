package dk.diku.poets.android.tablet;

import java.util.ArrayList;

import android.app.Activity;

import android.content.Context;

import android.os.AsyncTask;
import android.os.Bundle;

import android.view.Gravity;
import android.view.View;

import android.widget.Button;
import android.widget.HorizontalScrollView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

import dk.diku.poets.android.tablet.Section.MkView;

import dk.diku.poets.gen.thrift.data.RecordDefinition;

import dk.diku.poets.gen.thrift.PoetsServer;

import dk.diku.poets.gen.thrift.value.Value;

import dk.diku.poets.poetsserver.ServerUtils;

import dk.diku.poets.record.PoetsValue;

import dk.diku.poets.record.PoetsValue.ListV;
import dk.diku.poets.record.PoetsValue.RecV;
import dk.diku.poets.record.PoetsValue.StringV;

import dk.diku.poets.record.RecordDecode;
import dk.diku.poets.record.RecordEncode;

public class StartContractActivity extends Activity {

	private Context mContext;
	private TextView tv;
	private Section<ListV> templateSection;
	private Section<RecV> dataSection;
	private Section<RecordDefinition> submitSection;
	private InputSpec contractRecView;

	private Section.MkView<ListV> templMK =
		new MkView<ListV>() {
			@Override
				public View mk(ListV data) {
					HorizontalScrollView hsv = new HorizontalScrollView(mContext);
					hsv.setPadding(50,2,2,2);
					LinearLayout templSelect = new LinearLayout(mContext);
					hsv.addView(templSelect);

					for(PoetsValue pv : data.val) {
						// pv.recordType : String
						// pv.cslCode : String
						// pv.description : String
						final RecV pvRec = ((RecV) pv).getIsAbstract()?((RecV)pv).getInstance():(RecV)pv;
						final Button pcdText = new Button(mContext);
						pcdText.setTextSize(32);
						final String recType = ((StringV)pvRec.getField("name")).val;
						pcdText.setText(recType);
						pcdText.setPadding(10,10,10,10);
						templSelect.addView(pcdText);
						// set click listener
						pcdText.setOnClickListener(new View.OnClickListener (){
							@Override
							public void onClick(View arg0) {
								dataSection.enable(pvRec);
							}});
					}
					return hsv;
				}};


	private Section.MkView<RecV> dataMK = new Section.MkView<RecV>() {
		@Override
		public View mk(final RecV data) {
			final LinearLayout ll = new LinearLayout(mContext);
			ll.setOrientation(LinearLayout.VERTICAL);

			final ProgressBar pb = 
				new ProgressBar(mContext);
			ll.addView(pb, new LinearLayout.LayoutParams(
						LinearLayout.LayoutParams.MATCH_PARENT,
						LinearLayout.LayoutParams.WRAP_CONTENT));

			pb.setIndeterminate(true);
			(new AsyncTask<String, Void, RecordDefinition>() {
				@Override
				protected RecordDefinition doInBackground(String... typeName) {
					try {
						PoetsServer.Iface pServer = ServerUtils.getServer();
						return pServer.getRecordDefinition(typeName[0]);
					} catch (Exception e) {
						System.out.println("CON10: failed to find contract meta-data");
						return null;
					}
				}
				@Override
				protected void onPostExecute(final RecordDefinition recDef) {
					if(recDef != null) {
						ll.removeView(pb);
						contractRecView = (InputSpec)
							new InputSpec.Builder(mContext)
							.setRecordName(recDef.getRecordName())
							.create();
						ll.addView(contractRecView, new LinearLayout.LayoutParams(
								LinearLayout.LayoutParams.MATCH_PARENT,
								LinearLayout.LayoutParams.WRAP_CONTENT));
						contractRecView.fillField("templateName", data.getField("name"));
						submitSection.enable(recDef);
					}
				}
			}).execute(((StringV)data.getField("recordType")).val);
			
			return ll;
		}
	};

	private Section.MkView<RecordDefinition> submitMK = new Section.MkView<RecordDefinition>() {
		@Override
		public View mk(RecordDefinition data) {
			final Button submitB = new Button(mContext);
			submitB.setText("Submit " + data.getRecordName());
			submitB.setTextSize(24);
			submitB.setGravity(Gravity.CENTER);
			LinearLayout.LayoutParams lllp =
				new LinearLayout.LayoutParams(
						LinearLayout.LayoutParams.WRAP_CONTENT,
						LinearLayout.LayoutParams.WRAP_CONTENT);
			lllp.gravity = Gravity.RIGHT;
			lllp.setMargins(10,10,50,10);
			submitB.setLayoutParams(lllp);
			submitB.setOnClickListener(new View.OnClickListener() {
				@Override public void onClick(View v) {
					if(contractRecView != null) {
						try {		submitB.setEnabled(false);
								(new AsyncTask<Void, Void, Integer>() {
									@Override
									protected Integer doInBackground(
											Void... arg0) {
										try {
										    PoetsValue contractRec = ((Input)contractRecView).getValue();
										    System.out.println("CON10: getvalue = " + contractRec);
										    PoetsServer.Iface pServer = ServerUtils.getServer();
										    return pServer.createContract(RecordEncode.encodeValue(contractRec));
										} catch (Exception e) {
											System.out.println("Failed to start contract:");
											e.printStackTrace();
											return null;
										}
									}
									@Override
									protected void onPostExecute(Integer contractId) {
										if(contractId != null) {
											Toast.makeText(mContext, " contract started successfully", Toast.LENGTH_LONG).show();
											StartContractActivity.this.finish();
										} else {
											//TODO: show error message if possible to help
											//user decide what went wrong
											submitB.setEnabled(true);
										}
									}
								}).execute();
						} catch (Exception e) {
							System.out.println("getValue failed");
							e.printStackTrace();
						}
					}
				}
			    });
			return submitB;
		}
	};

	private void setupLoader() {
		(new AsyncTask<Void,Void,PoetsValue>() {
			@Override
			protected ListV doInBackground(Void... arg0) {
				try {
					PoetsServer.Iface pServer = ServerUtils.getServer();
					Value val = pServer.queryReport("ContractTemplates",
												new ArrayList<Value>(),
												new ArrayList<Value>());
					PoetsValue result = RecordDecode.decodeValue(val);
					return (ListV) result;
				} catch (Exception e) {
					System.out.println("CON10: failed to fetch contract templates");
					e.printStackTrace();
					return null;
				}
			}
			@Override
			protected void onPostExecute(PoetsValue v) {
				if(v != null && v instanceof ListV) {
					startContractUI((ListV)v);
				}
			}
		}).execute();
	}

	private void startContractUI(ListV templList) {
		ScrollView sv = new ScrollView(mContext);
		// ll holds the three sections
		LinearLayout ll = new LinearLayout(mContext);
		ll.setOrientation(LinearLayout.VERTICAL);
		sv.addView(ll);
		
		
		// 1. Select template
		templateSection = new Section<ListV>(mContext, 
				Section.makeSectionHeader(mContext, "1.", "Select Template"));
		templateSection.setMK(templMK);
		templateSection.enable(templList);


		dataSection = new Section<RecV>(mContext, 
				Section.makeSectionHeader(mContext, "2.", "Enter data"));
		dataSection.setMK(dataMK);

		submitSection = new Section<RecordDefinition>(mContext, 
				Section.makeSectionHeader(mContext, "3.", "Tap 'Submit' to start contract"));
		submitSection.setMK(submitMK);

		ll.addView(templateSection);
		Section.addRuler(mContext, ll);
		ll.addView(dataSection, new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT));
		Section.addRuler(mContext, ll);
		ll.addView(submitSection);

		setContentView(sv);
	}
		
	@Override
	public void onCreate(Bundle state) {
		super.onCreate(state);
		mContext = this;

		tv = new TextView(mContext);
		tv.setText("Fetching contract templates...");
		tv.setTextSize(24);
		tv.setGravity(Gravity.CENTER);

		setupLoader();

		setContentView(tv);
	}
}
