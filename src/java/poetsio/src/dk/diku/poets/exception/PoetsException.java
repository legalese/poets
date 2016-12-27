package dk.diku.poets.exception;

import java.io.Serializable;

/**
 * Wrapper for all exceptions thrown by the Poets Server.
 * @author jonsson
 *
 */
@SuppressWarnings("serial")
public class PoetsException extends java.lang.Exception implements Serializable{
	private String description;
	public PoetsException(){
	}
	public PoetsException(String description){
		this.description = description;
	}
	public String getDescription(){
		return description;
	}
}
