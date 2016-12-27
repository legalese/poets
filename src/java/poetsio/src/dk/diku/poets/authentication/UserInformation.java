package dk.diku.poets.authentication;

import java.io.Serializable;
import java.util.List;

public interface UserInformation extends Serializable{
	String getUsername();
	String getPasswordHash();
	String getSessionId();
	String getUserClass();
	List<String> getRoles();
	void setUserClass(String userClass);
	void setSessionId(String sessionId);
	void setRoles(List<String> roles);
	void setPasswordHash(String passwordHash);
	void setUserName(String userName);
}
