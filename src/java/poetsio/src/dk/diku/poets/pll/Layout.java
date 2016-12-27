package dk.diku.poets.pll;

import java.util.List;
import java.io.Serializable;

@SuppressWarnings("serial")
public class Layout implements Serializable{

	/**
	 * Gets the recordName for this instance.
	 *
	 * @return The recordName.
	 */
	public String getRecordName()
	{
		return this.recordName;
	}

	/**
	 * Sets the recordName for this instance.
	 *
	 * @param recordName The recordName.
	 */
	public void setRecordName(String recordName)
	{
		this.recordName = recordName;
	}

	/**
	 * Gets the title for this instance.
	 *
	 * @return The title.
	 */
	public String getTitle()
	{
		return this.title;
	}

	/**
	 * Sets the title for this instance.
	 *
	 * @param title The title.
	 */
	public void setTitle(String title)
	{
		this.title = title;
	}

	/**
	 * Gets the header for this instance.
	 *
	 * @return The header.
	 */
	public List<Field> getHeader()
	{
		return this.header;
	}

	/**
	 * Sets the header for this instance.
	 *
	 * @param header The header.
	 */
	public void setHeader(List<Field> header)
	{
		this.header = header;
	}

	/**
	 * Gets the body for this instance.
	 *
	 * @return The body.
	 */
	public List<Field> getBody()
	{
		return this.body;
	}

	/**
	 * Sets the body for this instance.
	 *
	 * @param body The body.
	 */
	public void setBody(List<Field> body)
	{
		this.body = body;
	}

	/**
	 * Gets the footer for this instance.
	 *
	 * @return The footer.
	 */
	public List<Field> getFooter()
	{
		return this.footer;
	}

	/**
	 * Sets the footer for this instance.
	 *
	 * @param footer The footer.
	 */
	public void setFooter(List<Field> footer)
	{
		this.footer = footer;
	}

	public static class Field implements Serializable{
		private String name;
		private String label;
		/**
		 * Gets the name for this instance.
		 *
		 * @return The name.
		 */
		public String getName()
		{
			return this.name;
		}
		/**
		 * Sets the name for this instance.
		 *
		 * @param name The name.
		 */
		public void setName(String name)
		{
			this.name = name;
		}
		/**
		 * Gets the label for this instance.
		 *
		 * @return The label.
		 */
		public String getLabel()
		{
			return this.label;
		}
		/**
		 * Sets the label for this instance.
		 *
		 * @param label The label.
		 */
		public void setLabel(String label)
		{
			this.label = label;
		}
	}
	
	private boolean checkField(String otherName, List<Field> mapped) {
		System.out.println("[Layout] check whether \"" + otherName + "\" is already mapped in " + mapped);
		for(Field thisField : mapped) {
			if(thisField.getName().equals(otherName)) {
				// we found the otherField
				return true;
			}
		}
		// otherField not found in mapped
		return false;
	}
	
	private boolean isMapped(Field field) {
		String otherName = field.getName();
		// check header
		if(checkField(otherName, this.getHeader())) {
			return true;
		}
		// check body
		if(checkField(otherName, this.getBody())) {
			return true;
		}
		// check footer
		if(checkField(otherName, this.getFooter())) {
			return true;
		}
		return false;
	}

	public Layout mergeLayout(Layout otherLayout) {
		// add all fields from otherLayout that are not already mapped in
		// this layout
		// add header fields
		for(Field field : otherLayout.getHeader()) {
			if(!isMapped(field)) {
				this.getHeader().add(field);
			}
		}
		// add body fields
		for(Field field : otherLayout.getBody()) {
			if(!isMapped(field)) {
				this.getBody().add(field);
			}
		}
		// add footer fields
		for(Field field : otherLayout.getFooter()) {
			if(!isMapped(field)) {
				this.getFooter().add(field);
			}
		}

		return this;
	}


	private String recordName;
	private String title;
	private List<Field> header;
	private List<Field> body;
	private List<Field> footer;
}
