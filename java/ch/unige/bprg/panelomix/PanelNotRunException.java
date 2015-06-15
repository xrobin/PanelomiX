package ch.unige.bprg.panelomix;

public class PanelNotRunException extends IllegalStateException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public PanelNotRunException(String[] errors) {
		super("The panel has not been run.");
	}

}
