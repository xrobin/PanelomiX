package ch.unige.bprg.panelomix;

/** Compares two panel with the same PanelOptimizeOptions, otherwise throws an IncomparablePanelsException.
 * @return <ul>
 * <li>0 if the panels have the same optimized performance, the same constrained performance (only for "both" optimization) and the same length</li>
 * <li>-1 if p1 has a lower optimized performance, or a lower constrained performance (only for "both" optimization), or has a bigger size</li>
 * <li>1 if p1 has a higher optimized performance, or a higher constrained performance (only for "both" optimization), or has a smaller size</li>
 *
 */
public class PanelComparator {
	static int compare(Panel p1, Panel p2) {
		PanelOptimizeOptions oo1 = p1.getOptimizeOptions();
		PanelOptimizeOptions oo2 = p2.getOptimizeOptions();
		if (oo1.equals(oo2)) {
			if (oo1.isFocusAcc()) {
				return new Float(p1.getOptimizedPerf()).compareTo(p2.getOptimizedPerf());
			}
			else {
				int compareOptimizedPerf = new Float(p1.getOptimizedPerf()).compareTo(p2.getOptimizedPerf());
				if (compareOptimizedPerf == 0) {
					int compareConstrainedPerf = new Float(p1.getConstrainedPerf()).compareTo(p2.getConstrainedPerf());
					if (compareConstrainedPerf == 0) {
						return new Integer(p2.size()).compareTo(p1.size());
					}
					return compareConstrainedPerf;
				}
				return compareOptimizedPerf;
			}
		}
		else {
			throw new IncomparablePanelsException("The panels have different optimizeOptions and cannot be compared.");
		}
	}
}
