package dk.diku.poets.android.tablet;

public interface DSLElement {
	void accept(DSLVisitor visitor);
}
