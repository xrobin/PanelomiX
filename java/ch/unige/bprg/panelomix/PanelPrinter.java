package ch.unige.bprg.panelomix;
import java.io.FileNotFoundException;
import java.io.PrintStream;

/** 
 * Prints a panel
 * @author Xavier Robin
 */
public class PanelPrinter extends PrintStream {
	private PrintStream ps1;
	private PrintStream ps2;
	private PanelomiX p;

	/**
	 * Constructor for 0 output stream
	 * @param PrintStream out: the output stream
	 * @throws FileNotFoundException 
	 */
	public PanelPrinter(PanelomiX panelomix) throws FileNotFoundException {
		super(OSCall.NulDir());
		this.ps1 = new PrintStream(OSCall.NulDir());
		this.p = panelomix;
	}

	/**
	 * Constructor for 1 output stream: PrintStream
	 * @param PrintStream out: the output stream
	 */
	public PanelPrinter(PanelomiX panelomix, PrintStream out) {
		super(out);
		this.p = panelomix;
		this.ps1 = out;
	}

	/**
	 * Constructor for 2 output streams: PrintStream and String (file name)
	 * @param PrintStream out1: the output stream
	 * @param String filename2: the output file name
	 * @throws FileNotFoundException
	 */
	public PanelPrinter(PanelomiX panelomix, PrintStream out1, String filename2) throws FileNotFoundException {
		super(out1);
		this.p = panelomix;
		this.ps1 = out1;
		this.ps2 = new PrintStream(filename2);
	}

	/**
	 * Constructor for 1 output streams: String (file name)
	 * @param String filename: the output file name
	 * @throws FileNotFoundException
	 */
	public PanelPrinter(PanelomiX panelomix, String filename) throws FileNotFoundException {
		super(filename);
		this.p = panelomix;
		this.ps1 = new PrintStream(filename);
	}

	/**
	 * Prints a string followed by a new line on any outputs
	 * @param String s: the line to print
	 */
	public void println(String s) {
		print(s);
		println();
	}
	
	/** 
	 * Flushes the streams
	 */
	public void flush() {
		ps1.flush();
		if (this.ps2 != null)
			ps2.flush();
	}

	/**
	 * Prints a new line on any outputs
	 */
	public void println() {
		print(System.getProperty("line.separator"));
	}

	/**
	 * Prints a string on any outputs
	 * @param String s: the line to print
	 */
	public void print(String s) {
		this.ps1.print(s);
		if (this.ps2 != null)
			this.ps2.print(s);
	}

	/** Prints a format on any outputs 
	 * @param String format
	 * @param Object ... args
	 */
	public PrintStream printf(String format, Object ... args) {
		this.ps1.printf(format, args);
		if (this.ps2 != null)
			this.ps2.printf(format, args);
		return this;
		
	}

	/** We found a better panel: print its performance 
	 * @param float se: the new sensitivity
	 * @param float sp: the new specificity
	 */
	public void printFoundBetter(float se, float sp) {
		printf("Better panel found: se = %.1f; sp = %.1f", se*100, sp*100);
		println();
	}

	/** 
	 * Prints a panel
	 * @param sp 
	 * @param se 
	 * @param int[] panel: the panel
	 * @param int[] thresholdIdx: the indices of the thresholds
	 * @param int scorePosAt: when the panel is positive
	 */
	public void printPanel(int[] panel, int[] thresholdIdx, int scorePosAt) {
		for (int mol = 0; mol < panel.length; mol++) {
			/* If controls < stroke then we must have mol > threshold for a positive */
			String direct = p.directions.get(p.allPredictors[panel[mol]]).equals(">") ? "<" : ">";
			print(p.allPredictors[panel[mol]] + " " + direct + " " + p.thresholds.get(p.allPredictors[panel[mol]])[thresholdIdx[mol]] + "; ");
		}
		println("positive at: " + scorePosAt);
	}
}
