package dk.diku.poets.android.tablet;

import java.util.ArrayList;
import java.util.List;

import dk.diku.poets.gen.thrift.reporting.Report;

import dk.diku.poets.record.PoetsValue;

// class to represent 'configuration' to report (includes at least a
// specification of parameters to report)
public class QuerySpec {

	public interface ArgSpec {
		public PoetsValue eval();
	}

	private Report report;
	private List<ArgSpec> argSpecs;

	public QuerySpec(Report rp) {
		report = rp;
		argSpecs = new ArrayList<ArgSpec>();
	}

	public QuerySpec addArgument(ArgSpec argS) {
		if(argSpecs == null) {
			argSpecs = new ArrayList<ArgSpec>(1);
		}

		argSpecs.add(argS);

		return this;
	}

	public void runQuery(Utils.RunWithArg<PoetsValue> rwa) {
		List<PoetsValue> args = evalArgs();
		Utils.withQuery(report.getName(), args, rwa);
	}

	private List<PoetsValue> evalArgs() {
		List<PoetsValue> evaled = new ArrayList<PoetsValue>(argSpecs.size());

		for(ArgSpec argS : argSpecs) {
			evaled.add(argS.eval());
		}

		return evaled;
	}

}
