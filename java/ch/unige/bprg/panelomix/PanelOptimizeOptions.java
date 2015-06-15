package ch.unige.bprg.panelomix;


public class PanelOptimizeOptions {
	/** Meaning of focusOn:<ul>
	 * <li>-1: sensitivity</li>
	 * <li>0: accuracy</li>
	 * <li>1: specificity</li></ul>
	 */
	private byte focusOn;
	private float minConstr;
	
	/** Basic constructor */
	public PanelOptimizeOptions(String focus, float minConstr) {
		this.minConstr = minConstr;
		if (isSensitivity(focus))
			this.focusOn = -1;
		else if (isSpecificity(focus))
			this.focusOn = 1;
		else if (isAcc(focus))
			this.focusOn = 0;
		else
			throw new IllegalArgumentException("Illegal value for focus: " + focus);
	}

	/** Only for focus == "accuracy". Otherwise a minConstr is required */
	public PanelOptimizeOptions(String focus) {
		if (isAcc(focus))
			this.focusOn = 0;
		else if (isSensitivity(focus) || isSpecificity(focus)) 
			throw new IllegalArgumentException("minConstr required for focus " + focus);
		else
			throw new IllegalArgumentException("Illegal value for focus: " + focus);
	}
	
	/** Is 'focus' sensitivity? One of sensitivity, sens or se are accepted (case insensitive). Otherwise, false. */
	private boolean isSensitivity(String se) {
		se = se.toLowerCase();
		if (se.equals("sensitivity") || se.equals("sens") || se.equals("se"))
			return true;
		return false;
	}

	/** Is 'focus' specificity? One of specificity, spec or sp are accepted (case insensitive). Otherwise, false. */	
	private boolean isSpecificity(String sp) {
		sp = sp.toLowerCase();
		if (sp.equals("specificity") || sp.equals("spec") || sp.equals("sp"))
			return true;
		return false;
	}
	
	/** Is 'focus' accuracy? Either accuracy, acc, a, both or b are accepted (case insensitive). Otherwise, false. */	
	private boolean isAcc(String acc) {
		acc = acc.toLowerCase();
		if (acc.equals("accuracy") || acc.equals("acc") || acc.equals("a") || acc.equals("both") || acc.equals("b"))
			return true;
		return false;
	}

	/** What we focus on. One of:<ul>
	 * <li>-sensitivity</li>
	 * <li>accuracy</li>
	 * <li>specificity</li></ul>
	 */
	public String getFocus() {
		if (isFocusSe())
			return "sensitivity";
		else if (isFocusSp())
			return "specificity";
		else
			return "accuracy";
	}
	
	/** Does this PanelOptimizeOptions focus on 'focus'? */
	public boolean isFocus(String focus) {
		if (this.isFocusSe() && isSensitivity(focus))
			return true;
		else if (this.isFocusSp() && isSpecificity(focus))
			return true;
		else if (this.isFocusAcc() && isAcc(focus))
			return true;
		else 
			return false;
	}
	
	/** Does this PanelOptimizeOptions focus on sensitivity? */
	public boolean isFocusSe() {
		return focusOn == -1;
	}

	/** Does this PanelOptimizeOptions focus on specificity? */
	public boolean isFocusSp() {
		return focusOn == 1;
	}

	/** Does this PanelOptimizeOptions focus on accuracy? */
	public boolean isFocusAcc() {
		return focusOn == 0;
	}

	/** What was minConstr? Meaningless with "accuracy". */
	public float getMinConstr() {
		return minConstr;
	}
	
	public boolean equals(PanelOptimizeOptions options) {
		return options.focusOn == this.focusOn
			&& options.minConstr == this.minConstr;
	}
}
