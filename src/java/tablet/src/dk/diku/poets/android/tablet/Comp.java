package dk.diku.poets.android.tablet;

/**
 * The Comp class is used to defer async computations until a later
 * point.
 * 
 * The class uses an Executor to handle the running of the derf
 *
 * @author Jesper Andersen
 */
public class Comp <T> {
	private Defer<T> d;

	public Comp(Defer<T> r) {
		d = r;
	}

	public void force(Cont<T> c) {
		c.put(d.get());
	}
}
