package dk.diku.poets.bootstrap;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.io.IOException;

import java.util.List;
import java.util.ArrayList;

import java.util.Map.Entry;

import org.apache.thrift.TException;
import org.ini4j.Ini;
import org.ini4j.InvalidFileFormatException;

import dk.diku.poets.gen.thrift.data.ParseError;
import dk.diku.poets.gen.thrift.data.ValidationError;
import dk.diku.poets.gen.thrift.reporting.ReportInitException;

import dk.diku.poets.gen.thrift.PoetsServer;

import dk.diku.poets.poetsserver.ServerUtils;

import dk.diku.poets.record.PoetsValue.RecV;
import dk.diku.poets.record.PoetsValue.RefV;
import dk.diku.poets.record.PoetsValue.StringV;

import dk.diku.poets.record.RecBuilder;
import dk.diku.poets.record.RecordEncode;

public class Bootstrap {

	private static PoetsServer.Iface pServer;

	public static void m(String s) {
		System.out.println("[Bootstrap] " + s);
	}

	private static void addCompany() {
		try {
			RecV me = 
				new RecBuilder("Me")
				.create();
			// create configuration entity
			PoetsServer.Iface pServer = ServerUtils.getServer();
			int companyId = pServer.createEntity(RecordEncode.encodeValue(me), "Me");
			m("Added custom 'Me' entity with id " + companyId);
		} catch (Exception e) {
			m("Something went wrong when adding company!");
			e.printStackTrace();
		}
	}

	private static String readFile(String filename) {
		StringBuffer contents = new StringBuffer();
		BufferedReader reader = null;

		try {
			File file = new File(filename);

			reader = new BufferedReader(new FileReader(file));
			String text = null;
			// repeat until all lines are read
			while ((text = reader.readLine()) != null)
			{
				contents
					.append(text)
					.append(System.getProperty("line.separator"));
			}
		} catch (IOException e) {
			m("*** IOException");
			e.printStackTrace();
		} finally {
			try {
				if (reader != null) {
					reader.close();
				}
			} catch (IOException e) {
				m("*** IOException when closing reader");
				e.printStackTrace();
			}
		}

		return contents.toString();
	}

	private static void printUsage() {
		m("Usage: boostrap <ini-file>");
	}

	static int port = 0;
	static String basedir = "";
	private static void handleSetup(Ini ini) {
		Ini.Section setup = ini.get("setup");

		String portString = setup.fetch("port");
		m("Port of server: " + portString);
		port = Integer.parseInt(portString);

		basedir = setup.fetch("basedir");
		m("Reading files from: " + basedir);

		// Setup localhost connection, 
		// TODO 
		// - respect port number 
		// - allow ip to be set in config also
		ServerUtils.setIp(127,0,0,1);
		//ServerUtils.setIp(192,38,115,208);
		pServer = ServerUtils.getServer();
	}

	private static void handleOntology(Ini ini) throws ValidationError, ParseError, TException {
		Ini.Section ontology = ini.get("ontology");

		m("Reading ontology files");
		List<String> pce = new ArrayList<String>();
		for(String key: ontology.keySet()) {
			m("Entry: " + key + " = " + ontology.fetch(key));
			pce.add(readFile(ontology.fetch(key)));
		}
		pServer.addDataDefinitions(pce);
	}

	private static void handleCustomEntities(Ini ini) {
		//TODO:
		//- allow specification of custom entities that should be present
		//  created before any reports and contracts are added
		m("Handling custom entities");
		addCompany();	
	}

	private static void handleReports(Ini ini) {
		Ini.Section reports = ini.get("reports");
                String prelude = readFile(reports.fetch("prelude", String.class));
		for(String val : reports.fetchAll("rep", String[].class)) {
			try {
				m("Adding report: " + val);
                                StringBuilder sb = new StringBuilder();
                                for(String line : readFile(val).split("\n")){
                                    sb.append(line + "\n");
                                    if (line.matches("tags:.*")){
                                        // HACK: Prelude is inlined (!)
                                        sb.append(prelude);
                                    }
                                }
				pServer.createReport(sb.toString());
			} catch (Exception e) {
				m("Failed to add report " + val);
				e.printStackTrace();
			}
		}
	}

	private static void handleContracts(Ini ini) {
		Ini.Section contracts = ini.get("contracts");
                String prelude = readFile(contracts.fetch("prelude", String.class));
		for(String val : contracts.fetchAll("csl", String[].class)) {
			try {
				m("Adding contract: " + val);
                                StringBuilder sb = new StringBuilder();
                                for(String line : readFile(val).split("\n")){
                                    sb.append(line + "\n");
                                    if (line.matches("description:.*")){
                                        // HACK: Prelude is inlined (!)
                                        sb.append(prelude);
                                    }
                                }
				pServer.createContractTemplate(sb.toString());
			} catch (Exception e) {
				m("Failed to add contract " + val);
				e.printStackTrace();
			}
		}
	}

	private static void go(Ini ini) throws ValidationError, ParseError, TException, ReportInitException {
		m("Starting bootstrap");

		handleSetup(ini);
		try {handleOntology(ini); } catch(Exception e) {}
		handleCustomEntities(ini);
		handleReports(ini);
		handleContracts(ini);
	}

	public static void main(String[] args) {
		if(args.length == 1) {
			File iniFile = new File(args[0]);
			try {
				Ini ini = new Ini(new FileReader(iniFile));
				go(ini);
			} catch (InvalidFileFormatException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ValidationError e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ParseError e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (TException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			printUsage();
		}
	}
}
