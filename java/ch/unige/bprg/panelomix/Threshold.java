package ch.unige.bprg.panelomix;


public class Threshold {
	private String biomarker;
	private String direction;
	private float threshold;
	
	public Threshold(String biomarker, String direction, float threshold) {
		this.biomarker = biomarker;
		this.direction = direction;
		this.threshold = threshold;
	}
	
	public String toString() {
		return biomarker + " " + direction + " " + threshold;
	}

	public String getMarker() {
		return biomarker;
	}
	
	public String getDirection() {
		return direction;
	}
	
	public float getThreshold() {
		return threshold;
	}

	/** Tests equality if two Thresholds
	 * Returns true only if two Thresholds are exactly identical for biomarker name, direction and threshold.
	 */
	public boolean equals(Threshold t) {
		return t.getMarker().equals(biomarker) 
			&& t.getDirection().equals(direction) 
			&& t.getThreshold() ==  threshold;
	}
}
