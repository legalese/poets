package dk.diku.poets.android.tablet;

public interface DSLVisitor {
	public void visit(DSL.PContainer pc);
	public void visit(DSL.PText pt);
	public void visit(DSL.PQuery pq);
	public void visit(DSL.PExt pe);
}
