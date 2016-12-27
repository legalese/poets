package dk.diku.poets.poetsserver;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TFramedTransport;

import dk.diku.poets.gen.thrift.PoetsServer;
import dk.diku.poets.synchronizedfactory.SynchronizedFactory;

/**
 * Class for managing the synchronised connection to 
 * the Poets server.
 * @author jonsson
 *
 */
public class ServerUtils {
	private static PoetsServer.Iface pServerCached;
	private static TTransport transport;

	public static void refreshServer(){
		if(!isHostIpSet()) {
//			setIp(127, 0, 0, 1); 
			setIp(10, 0, 0, 6); // local VM
			//setIp(192, 38, 115, 208); // 3gerp.ekstranet.diku.dk
			//setIp(192, 38, 15, 106); // Morten Ib laptop
			//setIp(192, 168, 0, 14);  //PoetsDemo (123entotre)
		}
		pServerCached = null;
		transport = null;
		String hostIp = getIp();
		Integer hostPort = 7911;
		try{
			transport = new TSocket(hostIp, hostPort);
			//((TSocket)transport).setTimeout(2000);
			transport = new TFramedTransport(transport);
			TBinaryProtocol protocol = new TBinaryProtocol(transport);		
			PoetsServer.Iface poetsServer = new PoetsServer.Client(protocol);
			transport.open();
			pServerCached = SynchronizedFactory.makeSynchronized(PoetsServer.Iface.class, poetsServer);
		}catch (Exception e) {
			System.out.println("[POETS] *** getServer failed with ip: " 
					+ hostIp);
			e.printStackTrace();			
		}
	}
	public static PoetsServer.Iface getServer(){
		if(pServerCached == null) {
			refreshServer();
		}	
		return pServerCached;
	}
	private static Integer hostIp1 = 127;
	private static Integer hostIp2 = 0;
	private static Integer hostIp3 = 0;
	private static Integer hostIp4 = 1;

	public static void setIp(int ip1,
			int ip2,
			int ip3,
			int ip4) {
		hostIp1 = ip1;
		hostIp2 = ip2;
		hostIp3 = ip3;
		hostIp4 = ip4;
	}

	private static boolean isHostIpSet() {
		return 
		hostIp1 != null &&
		hostIp2 != null &&
		hostIp3 != null &&
		hostIp4 != null;
	}

	public static String getIp() {
		return 
		hostIp1.toString() + "." +
		hostIp2.toString() + "." +
		hostIp3.toString() + "." +
		hostIp4.toString();
	}
	public static void setServer(PoetsServer.Iface server){
		pServerCached = server;
	}
	public static void setTransport(TSocket transport){
		ServerUtils.transport = transport;
	}
}
