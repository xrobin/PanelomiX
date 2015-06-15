package ch.unige.bprg.panelomix;

import java.util.*;

public class Panel implements Comparable<Panel> {
	/* Core panel variables */
	private int positiveAt;
	private List<Threshold> thresholds = new ArrayList<Threshold>();
	/* Panel properties and options */
	private float sensitivity;
	private float specificity;
	private int size;
	private PanelOptimizeOptions optimizeOptions;
	
	public Panel(PanelomiX p, int[] panel, int[] thresholdIdx, int scorePosAt, float sensitivity, float specificity) {
		this.sensitivity = sensitivity;
		this.specificity = specificity;
		for (int mol = 0; mol < panel.length; mol++) {
			/* If controls < stroke then we must have mol > threshold for a positive */
			String direction = p.directions.get(p.allPredictors[panel[mol]]).equals(">") ? "<" : ">";
			thresholds.add(new Threshold(p.allPredictors[panel[mol]], direction, p.thresholds.get(p.allPredictors[panel[mol]])[thresholdIdx[mol]]));
			// print(p.allPredictors[panel[mol]] + " " + direct + " " + p.thresholds.get(p.allPredictors[panel[mol]])[thresholdIdx[mol]] + "; ");
		}
		size = thresholds.size();
		positiveAt = scorePosAt;
		optimizeOptions = p.getOptimizeOptions();
	}
	
	public float getSensitivity() {
		return(sensitivity);
	}
	
	public float getSpecificity() {
		return(specificity);
	}
	
	public int getPositiveAt() {
		return positiveAt;
	}
	
	public int size() {
		return size;
	}
	
	public List<Threshold> getThresholds() {
		return(thresholds);
	}
	
	public List<String> getMarkers() {
		List<String> l = new ArrayList<String>();
		for (Threshold t : thresholds) {
			l.add(t.getMarker());
		}
		return l;
	}
	
	/** Returns the optimized performance */
	public float getOptimizedPerf() {
		if (optimizeOptions.isFocusSe()) {
			return specificity;
		}
		else if (optimizeOptions.isFocusSp()) {
			return sensitivity;
		}
		return specificity + sensitivity;
	}
	
	/** Returns the performance of the constrained value or -1 if "both" */
	public float getConstrainedPerf() {
		if (optimizeOptions.isFocusSe()) {
			return sensitivity;
		}
		else if (optimizeOptions.isFocusSp()) {
			return specificity;
		}
		return -1;
	}
	
	public PanelOptimizeOptions getOptimizeOptions() {
		return optimizeOptions;
	}
	
	public String toString() {
		String s = "";
		for (Threshold threshold : thresholds) {
			s += threshold.toString();
		}
		s += "positive at: " + positiveAt;
		return s;
	}

	@Override
	/** Compares the performance of two panels.
	 * @return a negative integer, zero, or a positive integer as the specified Panel is better than, equal to, or worse than this Panel.
	 */
	public int compareTo(Panel p) {
		return PanelComparator.compare(this, p);
	}
}
