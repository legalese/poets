package dk.diku.poets.authentication;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;

import dk.diku.poets.record.PoetsValue;

public class AuthServletRmi implements AuthServletService{
	AuthServlet authServlet = new AuthServlet();

	public HashMap<String, UserInformation> initUsers(){
		try{
			/** Static user information: **/
			/** jens, thomas, phillip from LejDJ **/
			/** dj is a general DJ **/
			//Jens
			UserInformationImpl u1 = new UserInformationImpl();
			u1.setUserName("jens");
			u1.setPasswordHash(BCrypt.hashpw(u1.getUsername(), BCrypt.gensalt()));
			u1.setRoles(new ArrayList<String>());
			//Thomas
			UserInformationImpl u2 = new UserInformationImpl();
			u2.setUserName("thomas");
			u2.setPasswordHash(BCrypt.hashpw(u2.getUsername(), BCrypt.gensalt()));
			u2.setRoles(new ArrayList<String>());
			//Phillip
			UserInformationImpl u3 = new UserInformationImpl();
			u3.setUserName("phillip");
			u3.setPasswordHash(BCrypt.hashpw(u3.getUsername(), BCrypt.gensalt()));
			u3.setRoles(new ArrayList<String>());
			//DJ
			UserInformationImpl u4 = new UserInformationImpl();
			u4.setUserName("dj");
			u4.setPasswordHash(BCrypt.hashpw("lejdj", BCrypt.gensalt()));
			u4.setRoles(new ArrayList<String>());
			//Admin
			UserInformationImpl u5 = new UserInformationImpl();
			u5.setUserName("admin");
			u5.setPasswordHash(BCrypt.hashpw("123entotre", BCrypt.gensalt()));
			u5.setRoles(new ArrayList<String>());

			//Adding roles
			u1.getRoles().add("Company"); u1.getRoles().add("Customer"); u1.getRoles().add("DJ");
			u2.getRoles().add("Company"); u2.getRoles().add("Customer"); u2.getRoles().add("DJ");
			u3.getRoles().add("Company"); u2.getRoles().add("Customer"); u2.getRoles().add("DJ");
			u4.getRoles().add("DJ");
			u5.getRoles().add("LegalEntity");

			HashMap<String, UserInformation> map = new HashMap<String, UserInformation>();
			map.put(u1.getUsername(), u1);
			map.put(u2.getUsername(), u2);
			map.put(u3.getUsername(), u3);
			map.put(u4.getUsername(), u4);
			map.put(u5.getUsername(), u5);
			return map;
		}catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public AuthServletRmi() {
		initUsers();
		authServlet.initializeUsers(initUsers());
	}

	@Override
	public UserInformationImpl authorize(String userName, String password)
	throws RemoteException {
		UserInformation ui = authServlet.authorize(userName, password);
		if(ui != null){
			return (UserInformationImpl) ui;
		}else{
			throw new RemoteException();
		}
	}

	@Override
	public boolean checkUserPermission(PoetsValue.RefV responsible,
			UserInformationImpl userInformation) throws RemoteException{
		return authServlet.checkUserPermission(responsible, userInformation);
	}

	@Override
	public UserInformationImpl checkValidity(String sessionID) throws RemoteException{
		return (UserInformationImpl) authServlet.checkValidity(sessionID);
	}

	@Override
	public void expire(String sessionID) throws RemoteException{
		authServlet.expire(sessionID);
	}
}
