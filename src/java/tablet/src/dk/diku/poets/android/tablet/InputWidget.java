package dk.diku.poets.android.tablet;

import android.content.Context;

import android.view.View;

public abstract class InputWidget implements Input {
	protected abstract View getView(Context c);
}

