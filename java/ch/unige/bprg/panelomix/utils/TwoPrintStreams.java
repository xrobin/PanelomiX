package ch.unige.bprg.panelomix.utils;
import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.UnsupportedOperationException;
import java.util.Locale;
import java.util.Scanner;

public class TwoPrintStreams extends PrintStream implements Appendable, Closeable {
	private PrintStream ps1;
	private PrintStream ps2;
	public TwoPrintStreams(PrintStream out1, PrintStream out2) {
		super(out1);
		this.ps1 = out1;
		this.ps2 = out2;
	}
	
	/** Creates a new print stream with the specified files. 
	 * @param File fileName1: first file
	 * @param File fileName2: second file
	 * @throws FileNotFoundException */
	public TwoPrintStreams(File file1, File file2) throws FileNotFoundException {
		super(file1);
		this.ps1 = new PrintStream(file1);
		this.ps2 = new PrintStream(file2);
	}
	/** Creates a new print stream with the specified file names. 
	 * @param String fileName1: name of the first file
	 * @param String fileName2: name of the second file
	 * @throws FileNotFoundException
	 */
	public TwoPrintStreams(String fileName1, String fileName2) throws FileNotFoundException {
		super(fileName1);
		this.ps1 = new PrintStream(fileName1);
		this.ps2 = new PrintStream(fileName2);
	}
//    Creates a new print stream, without automatic line flushing, with the specified file name and charset.
    
    
	/** Flushes both streams */
	public void flush() {
		this.ps1.flush();
		this.ps2.flush();
	}
	/** 
	 * Closes both stream.
	 */
	public void close() {
		this.ps1.close();
		this.ps2.close();
	}
	/**
	 * checkError is not implemented for two streams.
	 * @throws UnsupportedOperationException;
	 * @return never returns
	 */
	public boolean checkError() {
		throw new UnsupportedOperationException("checkError is not implemented in TwoPrintStreams");
	}
	/**
	 * setError is not implemented for two streams.
	 * @throws UnsupportedOperationException;
	 * @return never returns
	 */
	protected void setError() {
		throw new UnsupportedOperationException("setError is not implemented in TwoPrintStreams");
	}
	/**
	 * clearError is not implemented for two streams.
	 * @throws UnsupportedOperationException;
	 * @return never returns
	 */
	protected void clearError() {
		throw new UnsupportedOperationException("clearError is not implemented in TwoPrintStreams");
	}
	/** Writes the specified byte to this stream */
	public void write(int b) {
		this.ps1.write(b);
		this.ps2.write(b);
	}
	/** Writes len bytes from the specified byte array starting at offset off to both streams. */
	public void write(byte[] buf, int off, int len) {
		this.ps1.write(buf, off, len);
		this.ps2.write(buf, off, len);		
	}
	/** Prints a boolean value. */
	public void print(boolean b) {
		this.ps1.print(b);
		this.ps2.print(b);		
	}
	/** Prints a character. */
	public void print(char c) {
		this.ps1.print(c);
		this.ps2.print(c);		
	}
	/** Prints an integer. */
	public void print(int i) {
		this.ps1.print(i);
		this.ps2.print(i);		
	}
	/** Prints a long integer. */
	public void print(long l) {
		this.ps1.print(l);
		this.ps2.print(l);		
	}
	/** Prints a floating point number. */
	public void print(float f) {
		this.ps1.print(f);
		this.ps2.print(f);		
	}
	/** Prints a double-precision floating-point number. */
	public void print(double d) {
		this.ps1.print(d);
		this.ps2.print(d);		
	}
	/** Prints an array of characters. */
	public void print(char[] s) {
		this.ps1.print(s);
		this.ps2.print(s);		
	}
	/** Prints a String on both output streams. */
	public void print(String s) {
		this.ps1.print(s);
		this.ps2.print(s);		
	}
	/** Prints an object. */
	public void print(Object obj) {
		this.ps1.print(obj);
		this.ps2.print(obj);		
	}
	/** Terminates the current line by writing the line separator string. */
	public void println() {
		this.ps1.println();
		this.ps2.println();
	}
	public TwoPrintStreams printf(String format, Object... args) {
		this.ps1 = this.ps1.printf(format, args);
		this.ps2 = this.ps2.printf(format, args);		
		return this;
	}
	public TwoPrintStreams format(String format, Object ... args) {
		this.ps1 = this.ps1.format(format, args);
		this.ps2 = this.ps2.format(format, args);		
		return this;
	}
	public TwoPrintStreams printf(Locale l, String format, Object... args) {
		this.ps1 = this.ps1.format(l, format, args);
		this.ps2 = this.ps2.format(l, format, args);
		return this;
	}
	public TwoPrintStreams append(CharSequence csq) {
		this.ps1 = this.ps1.append(csq);
		this.ps2 = this.ps2.append(csq);
		return this;
	}
	public TwoPrintStreams append(CharSequence csq, int start, int end) {
		this.ps1 = this.ps1.append(csq, start, end);
		this.ps2 = this.ps2.append(csq, start, end);
		return this;
	}
	public TwoPrintStreams append(char c)  {
		this.ps1 = this.ps1.append(c);
		this.ps2 = this.ps2.append(c);
		return this;
	}
	/** Access one of the streams individually 
	 * @param int i: either 1 or 2
	 * @return PrintStream: the requested print stream if i equals 1 or 2
	 * @throws IllegalArgumentException if i is different from 1 or 2
	 */
	public PrintStream getStream(int i) {
		if (i == 1) {
			return this.ps1;
		}
		else if (i == 2) {
			return this.ps2;
		}
		else {
			throw new IllegalArgumentException("i must be either 1 or 2.");
		}
	}
	/** Function test. Check that both the console and the file test.out now contains exactly:
	 * 
	 * Hello World!
	 */
	public static void main(String[] args) throws FileNotFoundException {
		try {
			// Setup both streams
			File tempFile = File.createTempFile("testTwoPrintStreams.java", ".tmp" );
			TwoPrintStreams outputStreams = new TwoPrintStreams(System.out, new PrintStream(tempFile));
			// Print in streams
			System.err.println("The console should display 'Hello World!' to pass the test.");
			TwoPrintStreams outputFile2 = outputStreams.format("%s", "Hello World");
			outputFile2.getStream(1);
			outputFile2.append("!\n");
			outputStreams.getStream(2).println("!");
			// Check the file was written properly
			Scanner scanFile = new Scanner(tempFile);
			if (! scanFile.hasNextLine() || ! scanFile.nextLine().equals("Hello World!")) {
				throw new IllegalStateException("Could not read from output file!");
			}
		} catch (IOException e) {
			System.err.println("Cannot create a temporary file.");
			e.printStackTrace();
		}
	}
}
