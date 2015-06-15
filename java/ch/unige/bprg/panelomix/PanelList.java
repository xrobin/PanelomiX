package ch.unige.bprg.panelomix;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;

/** A list of panels with the same performance (sensitivity and specificity, depending on the constraint) and size */
public class PanelList extends ArrayList<Panel> {
	private static final long serialVersionUID = 1L;
	private float sensitivity;
	private float specificity;
	private int minLength;
	private PanelOptimizeOptions optimizeOptions;
	
	public PanelList(PanelOptimizeOptions optimizeOptions) {
		this.optimizeOptions = optimizeOptions;
	}
	
	public PanelList() {}
	
	protected void foundBetter(float sensitivity, float specificity, int length) {
		setSensitivity(sensitivity);
		setSpecificity(specificity);
		setMinLength(length);
		this.clear();
	}

	private void setSensitivity(float sensitivity) {
		this.sensitivity = sensitivity;
	}

	public float getSensitivity() {
		return(sensitivity);
	}

	private void setSpecificity(float specificity) {
		this.specificity = specificity;
	}

	public int getMinLength() {
		return(minLength);
	}

	private void setMinLength(int length) {
		this.minLength = length;
	}

	public PanelOptimizeOptions getOptimizeOptions() {
		if (optimizeOptions == null) {
			optimizeOptions = this.get(0).getOptimizeOptions();
		}
		return optimizeOptions;
	}

	public float getSpecificity() {
		return(specificity);
	}

	/** Adds a panel with the given specifications to the list.
	 * @return true if the list was modified, false otherwise */
	protected boolean add(PanelomiX p, int[] panel, int[] thresholdIdx, int scorePosAt) {
		return this.add(new Panel(p, panel, thresholdIdx, scorePosAt, sensitivity, specificity));
	}

	/** Add a panel to the list. First empty the list if the new panel is better. Don't add it if it is worse.
	 * @return true if the list was modified, false otherwise */
	public boolean add(Panel panel) {
		int comparator = this.size() > 0 ? this.get(0).compareTo(panel) : 1;
		if (comparator == 0) {
			return super.add(panel);
		}
		else if (comparator < 0) {
			return false;
		}
		else {
			foundBetter(panel.getSensitivity(), panel.getSpecificity(), panel.size());
			super.add(panel);
			return true;
		}
	}

	/** Add a panel to the list at the given position. First empty the list if the new panel is better. Don't add it if it is worse. */
	public void add(int index, Panel panel) {
		int comparator = this.size() > 0 ? this.get(0).compareTo(panel) : 1;
		if (comparator == 0) {
			super.add(index, panel);
		}
		else if (comparator > 0) {
			foundBetter(panel.getSensitivity(), panel.getSpecificity(), panel.size());
			super.add(index, panel);
		}
	}

	/** Add a collection of panel to the list. First empty the list if the new panel is better. Don't add it if it is worse.
	 * @return true if the list was modified, false otherwise */
	public boolean addAll(Collection<? extends Panel> panels) {
		boolean changed = false;
		for (Panel panel : panels) {
			changed = add(panel) || changed;
		}
		return changed;
	}

	/** Add a collection of panel to the list at the given position. First empty the list if the new panel is better. Don't add it if it is worse.
	 * @return true if the list was modified, false otherwise */
	public boolean addAll(int index, Collection<? extends Panel> panels) {
		PanelList pl = new PanelList();
		for (Panel panel : panels) {
			pl.add(panel);
		}
		if (pl.size() > 0) {
			Panel panel = pl.get(0);
			int comparator = this.size() > 0 ? this.get(0).compareTo(panel) : 1;
			if (comparator == 0) {
				return super.addAll(index, pl);
			}
			else if (comparator < 0) {
				return false;
			}
			else {
				foundBetter(panel.getSensitivity(), panel.getSpecificity(), panel.size());
				super.add(panel);
				return true;
			}
		}
		return false;
	}

	public boolean addAll(int index, PanelList panels) {
		int compare = compareTo(panels);
		if (compare == 0) {
			return addAll(index, panels);
		}
		else if (compare < 0) {
			return false;
		}
		else {
			Panel panel = panels.get(0);
			foundBetter(panel.getSensitivity(), panel.getSpecificity(), panel.size());
			super.add(panel);
			return true;
		}
	}

	public int compareTo(PanelList pl) {
		if (this.size() > 0) {
			if (pl.size() > 0) {
				return this.get(0).compareTo(pl.get(0));
			}
			return -1;
		}
		return 1;
	}

	public String toString() {
		String s = new Formatter().format("Best panel found: se = %.1f; sp = %.1f", sensitivity*100, specificity*100).toString();
		s += System.getProperty("line.separator");
		for (Panel panel : this) {
			s += panel.toString();
			s += System.getProperty("line.separator");
		}
		return s;
	}
}
