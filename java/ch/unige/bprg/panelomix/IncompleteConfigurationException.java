package ch.unige.bprg.panelomix;

public class IncompleteConfigurationException extends IllegalStateException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public IncompleteConfigurationException(String[] errors) {
		super(makeErrorString(errors));
	}
	private static String makeErrorString(String[] errors) {
		// build string
		String str = "Configuration file not complete. Errors: \n";
		for (int i = 0; i < errors.length; i++) {
			str += "- " + errors[i] + "\n";
		}
		return str;
		// just let super work
	}

}
