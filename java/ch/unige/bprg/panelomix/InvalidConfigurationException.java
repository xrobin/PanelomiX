package ch.unige.bprg.panelomix;

public class InvalidConfigurationException extends IllegalArgumentException {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public InvalidConfigurationException(String error) {
		super("Invalid configuration line: " + error);
	}

}
