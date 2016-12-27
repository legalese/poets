package dk.diku.poets.authentication;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.List;

@SuppressWarnings("serial")
public class UserInformationImpl
//extends UnicastRemoteObject 
implements UserInformation {
	protected UserInformationImpl() throws RemoteException {
		//super();
	}

	private String passwordHash;
	private List<String> roles;
	private String sessionId;
	private String userClass;
	private String userName;

	@Override
	public String getPasswordHash() {
		return passwordHash;
	}

	@Override
	public List<String> getRoles() {
		return roles;
	}

	@Override
	public String getSessionId() {
		return sessionId;
	}

	@Override
	public String getUserClass() {
		return userClass;
	}

	@Override
	public String getUsername() {
		return userName;
	}

	@Override
	public void setUserName(String userName) {
		this.userName = userName;
	}

	@Override
	public void setPasswordHash(String passwordHash) {
		this.passwordHash = passwordHash;
	}
	
	@Override
	public void setRoles(List<String> roles) {
		this.roles = roles;
	}

	@Override
	public void setSessionId(String sessionId) {
		this.sessionId = sessionId;
	}

	@Override
	public void setUserClass(String userClass) {
		this.userClass = userClass;
	}
}
