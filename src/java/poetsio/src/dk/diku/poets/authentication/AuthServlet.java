package dk.diku.poets.authentication;

import java.util.HashMap;
import java.util.Set;

import dk.diku.poets.poetsserver.ServerUtils;
import dk.diku.poets.record.PoetsValue;

public class AuthServlet {
	private HashMap<String, UserInformation> userMap;
	private HashMap<String, String> sessionIds;
	public AuthServlet(){
		userMap = new HashMap<String, UserInformation>();
		sessionIds = new HashMap<String, String>();
	}

	public UserInformation authorize(String userName, String password){
		String pwHash = getPasswordHash(userName);
		if(pwHash!=null){
			if( BCrypt.checkpw(password, pwHash) ){
				String newSid = Integer.toString(Math.max(sessionIds.size(), 1));
				sessionIds.put(newSid, userName);
				UserInformation ui = userMap.get(userName);
				ui.setSessionId(newSid);
				return ui;
			}
		}
		System.out.println("[AuthServlet] Could not authorize "+userName+" with "+password);
		return null;
	}

	private String getPasswordHash(String userName){
		if(userMap.containsKey(userName)){
			return userMap.get(userName).getPasswordHash();
		}
		return null;
	}

	public UserInformation checkValidity(String sessionID){
		System.out.println("[Authservlet] check valid: "+sessionID);
		if(!sessionIds.containsKey(sessionID)){
			return null;
		}
		UserInformation ret = userMap.get(sessionIds.get(sessionID));
		return ret;
	}
	public void expire(String sessionID){
		if(!sessionIds.containsKey(sessionID)){
			System.err.println("[AuthServlet] Session ID: "+sessionID+" does not exists");
		}
		sessionIds.remove(sessionID);
	}
	/**
	 * Check that the LegalEntity subtype responsible for the 
	 * event, is included in the roles of the user that is signed on.
	 * @param responsible The class responsible
	 * @return True is the user is permitted to fire the event. False otherwise
	 */
	public boolean checkUserPermission(PoetsValue responsibleVal, UserInformation userInformation) {
		if(userInformation.getRoles() == null){
			System.err.println("[AuthServlet] roles are null");
			return false;
		}
		if(responsibleVal == null){
			// No responsible was found. We assume everyone can perform this.
			//TODO: Do something here.
			System.out.println("[AuthServlet] no responsible was added");
			return true;
		}
		for(String le : userInformation.getRoles()){
			try{
				PoetsValue.RefV responsible = null;
				if(responsibleVal.isRec()){
					for(String fieldName : ((PoetsValue.RecV)responsibleVal).getKeySet()){
						if(((PoetsValue.RecV)responsibleVal).getField(fieldName).isRef()){
							responsible = (PoetsValue.RefV)((PoetsValue.RecV)responsibleVal).getField(fieldName);
							break;
						}
					}
				}else if(responsibleVal.isRef()){
					responsible = (PoetsValue.RefV) responsibleVal;
				}
				if(responsible.getRefName().equals(le)){
					return true;
				}
				Set<String> subTypes = ServerUtils.getServer().getSubTypes(le);
				if(subTypes.contains(responsible.getRefName())){
					return true;
				}

			}catch (Exception e) {
				e.printStackTrace();
				return false;
			}
		}
		return false;
	}
	/**
	 * Initialises the map of users
	 * @param userMap The {@link HashMap} of users indexed by user name
	 */
	public void initializeUsers(HashMap<String, UserInformation> userMap){
		this.userMap = userMap;
	}
}
