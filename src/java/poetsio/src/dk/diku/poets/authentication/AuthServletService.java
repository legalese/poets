package dk.diku.poets.authentication;

import java.rmi.Remote;
import java.rmi.RemoteException;

import dk.diku.poets.record.PoetsValue;


public interface AuthServletService extends Remote{
	
	public final String serviceName = "AuthServletService";

	public UserInformationImpl authorize(String userName, String password) throws RemoteException;

	public abstract UserInformationImpl checkValidity(String sessionID) throws RemoteException;
	
	public abstract void expire(String sessionID) throws RemoteException;

	public abstract boolean checkUserPermission(PoetsValue.RefV responsible,
			UserInformationImpl userInformation) throws RemoteException;

}
