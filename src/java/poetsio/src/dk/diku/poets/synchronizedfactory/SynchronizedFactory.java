package dk.diku.poets.synchronizedfactory;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
/**
 * Factory class for wrapping PoetsServer.IFace methods synchronously 
 * @author jonsson,jespera
 *
 */
public final class SynchronizedFactory {
	private SynchronizedFactory() {}

	@SuppressWarnings("unchecked")
	public static <T> T makeSynchronized(Class<T> ifCls, T object) {
		return (T) Proxy.newProxyInstance(
				object.getClass().getClassLoader(),
				new Class<?>[] {ifCls},
				new Handler<T>(object));
	}

	static class Handler<T> implements InvocationHandler {
		private final T object;

		Handler(T object) {
			this.object = object;
		}

		@Override
			public Object invoke(Object proxy, Method method,
					Object[] args) throws Throwable {
				synchronized (object) {
					return method.invoke(object, args);
				}
			}
	}
}
