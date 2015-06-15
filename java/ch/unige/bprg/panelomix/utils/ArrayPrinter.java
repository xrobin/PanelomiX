package ch.unige.bprg.panelomix.utils;
/**
 * Prints the content of an array
 * @author Xavier Robin
 *
 */
public class ArrayPrinter {
	private String s;
	private int[] theInteger = new int[0];
	private float[] theFloat = new float[0];
	private boolean[] theBoolean = new boolean[0];
	private String[] theString = new String[0];
	private Object[] theObject = new Object[0];
	private int[][] theIntegerM = new int[0][0];
	private float[][] theFloatM = new float[0][0];
	private boolean[][] theBooleanM = new boolean[0][0];
	private String[][] theStringM = new String[0][0];
	private Object[][] theObjectM = new Object[0][0];
	private String colSeparator = ", ";
	private String lineSeparator = System.getProperty("line.separator");
	private final String type;
	
	
	/**
	 * Class constructors (2D)
	 * @param array (mandatory): an array of array of integers (int[]), floats (float[]), booleans (boolean[]) or strings (String[]).
	 * @param colSeparator (optional, mandatory if lineSeparator is used): the columns separator wanted, if different from ", ".
	 * @param lineSeparator (optional): the lines separator wanted, if different from File.separator.
	 */
	public ArrayPrinter(int[][] array) {
		this.type = "integerM";
		this.theIntegerM = array;
		this.Initialize();
	}	
	public ArrayPrinter(int[][] array, String colSeparator) {
		this.type = "integerM";
		this.colSeparator = colSeparator;
		this.theIntegerM = array;
		this.Initialize();
	}
	public ArrayPrinter(int[][] array, String colSeparator, String lineSeparator) {
			this.type = "integerM";
			this.colSeparator = colSeparator;
			this.lineSeparator = lineSeparator;
			this.theIntegerM = array;
			this.Initialize();
	}
	public ArrayPrinter(float[][] array) {
		this.type = "floatM";
		this.theFloatM = array;
		this.Initialize();
	}	
	public ArrayPrinter(float[][] array, String colSeparator) {
		this.type = "floatM";
		this.colSeparator = colSeparator;
		this.theFloatM = array;
		this.Initialize();
	}
	public ArrayPrinter(float[][] array, String colSeparator, String lineSeparator) {
		this.type = "floatM";
		this.colSeparator = colSeparator;
		this.lineSeparator = lineSeparator;
		this.theFloatM = array;
		this.Initialize();
	}
	
	public ArrayPrinter(boolean[][] array) {
		this.type = "booleanM";
		this.theBooleanM = array;
		this.Initialize();
	}
	public ArrayPrinter(boolean[][] array, String colSeparator) {
		this.type = "booleanM";
		this.colSeparator = colSeparator;
		this.theBooleanM = array;
		this.Initialize();
	}
	public ArrayPrinter(boolean[][] array, String colSeparator, String lineSeparator) {
		this.type = "booleanM";
		this.colSeparator = colSeparator;
		this.lineSeparator = lineSeparator;
		this.theBooleanM = array;
		this.Initialize();
	}
	
	public ArrayPrinter(String[][] array) {
		this.type = "StringM";
		this.theStringM = array;
		this.Initialize();
	}
	public ArrayPrinter(String[][] arg, String colSeparator) {
		this.type = "StringM";
		this.colSeparator = colSeparator;
		this.theStringM = arg;
		this.Initialize();
	}
	public ArrayPrinter(String[][] arg, String colSeparator, String lineSeparator) {
		this.type = "StringM";
		this.colSeparator = colSeparator;
		this.lineSeparator = lineSeparator;
		this.theStringM = arg;
		this.Initialize();
	}
	
	public ArrayPrinter(Object[][] array) {
		this.type = "ObjectM";
		this.theObjectM = array;
		this.Initialize();
	}
	public ArrayPrinter(Object[][] arg, String colSeparator) {
		this.type = "ObjectM";
		this.colSeparator = colSeparator;
		this.theObjectM = arg;
		this.Initialize();
	}
	public ArrayPrinter(Object[][] arg, String colSeparator, String lineSeparator) {
		this.type = "ObjectM";
		this.colSeparator = colSeparator;
		this.lineSeparator = lineSeparator;
		this.theObjectM = arg;
		this.Initialize();
	}

	/**
	 * Class constructors (1D)
	 * @param array (mandatory): an array of integers (int[]), floats (float[]), booleans (boolean[]) or strings (String[]).
	 * @param colSeparator (optional): the separator wanted, if different from ", ".
	 */
	public ArrayPrinter(int[] array) {
		this.type = "integer";
		this.theInteger = array;
		this.Initialize();
	}	
	public ArrayPrinter(int[] array, String colSeparator) {
		this.type = "integer";
		this.colSeparator = colSeparator;
		this.theInteger = array;
		this.Initialize();
	}
	public ArrayPrinter(float[] array) {
		this.type = "float";
		this.theFloat = array;
		this.Initialize();
	}	
	public ArrayPrinter(float[] array, String colSeparator) {
		this.type = "float";
		this.colSeparator = colSeparator;
		this.theFloat = array;
		this.Initialize();
	}
	public ArrayPrinter(boolean[] array) {
		this.type = "boolean";
		this.theBoolean = array;
		this.Initialize();
	}
	public ArrayPrinter(boolean[] array, String colSeparator) {
		this.type = "boolean";
		this.colSeparator = colSeparator;
		this.theBoolean = array;
		this.Initialize();
	}
	public ArrayPrinter(String[] array) {
		this.type = "String";
		this.theString = array;
		this.Initialize();
	}
	public ArrayPrinter(String[] arg, String colSeparator) {
		this.type = "String";
		this.colSeparator = colSeparator;
		this.theString = arg;
		this.Initialize();
	}
	public ArrayPrinter(Object[] array) {
		this.type = "Object";
		this.theObject = array;
		this.Initialize();
	}
	public ArrayPrinter(Object[] arg, String colSeparator) {
		this.type = "Object";
		this.colSeparator = colSeparator;
		this.theObject = arg;
		this.Initialize();
	}
	
	/** 
	 * Returns the column separator string to place between the elements
	 * @return String. Default: ", "
	 */
	public String getSeparator() {
		return this.colSeparator;
	}
	
	/** 
	 * Changes the column separator string to place between the elements
	 * @param String newSeparator: the new desired separator. Default: ", "
	 */
	public void setSeparator(String newSeparator) {
		this.colSeparator = newSeparator;
		this.Initialize();
	}	
	/** 
	 * Returns the column separator string to place between the elements
	 * @return String. Default: ", "
	 */
	public String getColSeparator() {
		return this.colSeparator;
	}
	
	/** 
	 * Changes the column separator string to place between the elements
	 * @param String newSeparator: the new desired separator. Default: ", "
	 */
	public void setColSeparator(String newSeparator) {
		this.colSeparator = newSeparator;
		this.Initialize();
	}
	
	/** 
	 * Returns the line separator string to place between the elements
	 * @return String. Default: ", "
	 */
	public String getLineSeparator() {
		return this.lineSeparator;
	}
	
	/** 
	 * Changes the separator string to place between the elements
	 * @param String newSeparator: the new desired separator. Default: ", "
	 */
	public void setLineSeparator(String newLineSeparator) {
		this.lineSeparator = newLineSeparator;
		this.Initialize();
	}
	
	/**
	 * Computes the string using the Array argument given and the separator
	 */
	private void Initialize() {
		this.s = "";
		/* 1D */
		if (this.type.equals("integer")) {
			for (int i = 0; i < this.theInteger.length; i++) {
				s += this.theInteger[i] + this.colSeparator;
			}
			s = s.substring(0, s.length() - this.colSeparator.length());
		}
		if (this.type.equals("float")) {
			for (int i = 0; i < this.theFloat.length; i++) {
				s += this.theFloat[i] + this.colSeparator;
			}
			s = s.substring(0, s.length() - this.colSeparator.length());
		}
		if (this.type.equals("boolean")) {
			for (int i = 0; i < this.theBoolean.length; i++) {
				s += this.theBoolean[i] + this.colSeparator;
			}
			s = s.substring(0, s.length() - this.colSeparator.length());
		}
		if (this.type.equals("String")) {
			for (int i = 0; i < this.theString.length; i++) {
				s += this.theString[i] + this.colSeparator;
			}
			s = s.substring(0, s.length() - this.colSeparator.length());
		}
		if (this.type.equals("Object")) {
			for (int i = 0; i < this.theObject.length; i++) {
				s += this.theObject[i] + this.colSeparator;
			}
			s = s.substring(0, s.length() - this.colSeparator.length());
		}
		/* 2D */
		if (this.type.equals("integerM")) {
			for (int i = 0; i < this.theIntegerM.length; i++) {
				s += new ArrayPrinter(this.theIntegerM[i], this.colSeparator) + this.lineSeparator;
			}
			s = s.substring(0, s.length() - this.lineSeparator.length());
		}
		if (this.type.equals("floatM")) {
			for (int i = 0; i < this.theFloatM.length; i++) {
				s += new ArrayPrinter(this.theFloatM[i], this.colSeparator) + this.lineSeparator;
			}
			s = s.substring(0, s.length() - this.lineSeparator.length());
		}
		if (this.type.equals("booleanM")) {
			for (int i = 0; i < this.theBooleanM.length; i++) {
				s += new ArrayPrinter(this.theBooleanM[i], this.colSeparator) + this.lineSeparator;
			}
			s = s.substring(0, s.length() - this.lineSeparator.length());
		}
		if (this.type.equals("StringM")) {
			for (int i = 0; i < this.theStringM.length; i++) {
				s += new ArrayPrinter(this.theStringM[i], this.colSeparator) + this.lineSeparator;
			}
			s = s.substring(0, s.length() - this.lineSeparator.length());
		}
		if (this.type.equals("ObjectM")) {
			for (int i = 0; i < this.theObjectM.length; i++) {
				s += new ArrayPrinter(this.theObjectM[i], this.colSeparator) + this.lineSeparator;
			}
			s = s.substring(0, s.length() - this.lineSeparator.length());
		}
			
	}

	/**
	 * Transpose the matrix. Only for 2D matrices (displays a warning on standard output if one attempts to transpose a 1D array).
	 * @param none
	 * @return itself
	 */
	public ArrayPrinter transpose() {
		if(this.type.equals("integer") || this.type.equals("float")  || this.type.equals("boolean") || this.type.equals("String")) {
			System.err.println("Warning: 1D " + this.type + " cannot be transposed. Ignoring command.");
		}
		else {
			if (this.type.equals("integerM")) {
				int[][] theNewIntegerM = new int[theIntegerM[1].length][theIntegerM.length];
				for (int i = 0; i < theNewIntegerM.length; i++) {
					for (int j = 0; j < theNewIntegerM[i].length; j++) {
						theNewIntegerM[i][j] = this.theIntegerM[j][i];
					}
				}
				this.theIntegerM = theNewIntegerM;
				this.Initialize();
			}
			if (this.type.equals("floatM")) {
				float[][] theNewFloatM = new float[theFloatM[1].length][theFloatM.length];
				for (int i = 0; i < theNewFloatM.length; i++) {
					for (int j = 0; j < theNewFloatM[i].length; j++) {
						theNewFloatM[i][j] = this.theFloatM[j][i];
					}
				}
				this.theFloatM = theNewFloatM;
				this.Initialize();
			}
			if (this.type.equals("booleanM")) {
				boolean[][] theNewBooleanM = new boolean[theBooleanM[1].length][theBooleanM.length];
				for (int i = 0; i < theNewBooleanM.length; i++) {
					for (int j = 0; j < theNewBooleanM[i].length; j++) {
						theNewBooleanM[i][j] = this.theBooleanM[j][i];
					}
				}
				this.theBooleanM = theNewBooleanM;
				this.Initialize();
			}
			if (this.type.equals("StringM")) {
				String[][] theNewStringM = new String[theStringM[1].length][theStringM.length];
				for (int i = 0; i < theNewStringM.length; i++) {
					for (int j = 0; j < theNewStringM[i].length; j++) {
						theNewStringM[i][j] = this.theStringM[j][i];
					}
				}
				this.theStringM = theNewStringM;
				this.Initialize();
			}
			if (this.type.equals("ObjectM")) {
				Object[][] theNewObjectM = new Object[theObjectM[1].length][theObjectM.length];
				for (int i = 0; i < theNewObjectM.length; i++) {
					for (int j = 0; j < theNewObjectM[i].length; j++) {
						theNewObjectM[i][j] = this.theObjectM[j][i];
					}
				}
				this.theObjectM = theNewObjectM;
				this.Initialize();
			}
		}
		return this;
	}

	/**
	 * Returns the nicely formatted string containing the array elements separated by the separator.
	 * @param none
	 * @return String
	 */
	public String toString() {
		return s;
	}

	/**
	 * Tests the class.
	 * @param args
	 * @throws IllegalStateException in case of unexpected results
	 */
	public static void main(String[] args) {
		ArrayPrinter ap;
		// Test integer 1D
		ap = new ArrayPrinter(new int[] {1, 2, 3, 5});
		if (! ap.toString().equals("1, 2, 3, 5"))
			throw new IllegalStateException("New integer concatenation broken");
		System.out.println("The following command should raise the warning: 'Warning: 1D integer cannot be transposed. Ignoring command.'");
		ap.transpose();
		ap.setSeparator("...");
		if (! ap.toString().equals("1...2...3...5"))
			throw new IllegalStateException("Integer concatenation with a custom separator probably broken");

		// Test integer 2D
		ap = new ArrayPrinter(new int[][] {{1, 2, 3}, {4, 5, 6}, {-10, 300, 12}});
		if (! ap.toString().equals("1, 2, 3" + System.getProperty("line.separator") + "4, 5, 6" + System.getProperty("line.separator") + "-10, 300, 12"))
			throw new IllegalStateException("New integerM concatenation broken");
		ap.setLineSeparator("...");
		ap.setColSeparator("+");
		if (! ap.toString().equals("1+2+3...4+5+6...-10+300+12"))
			throw new IllegalStateException("IntegerM concatenation with custom separators probably broken");
		ap.transpose();
		if (! ap.toString().equals("1+4+-10...2+5+300...3+6+12"))
			throw new IllegalStateException("IntegerM transposition probably broken");
	
		// Test float 1D
		ap = new ArrayPrinter(new float[] {2f, 594.5f, -8.49834f, 5});
		if (! ap.toString().equals("2.0, 594.5, -8.49834, 5.0"))
			throw new IllegalStateException("New float concatenation broken");
		ap.setSeparator("...");
		if (! ap.toString().equals("2.0...594.5...-8.49834...5.0"))
			throw new IllegalStateException("Float concatenation with a custom separator probably broken");

		// Test float 2D
		ap = new ArrayPrinter(new float[][] {{2f, 594.5f, -8.49834f}, {5f, +23e4f, 49.4f}});
		if (! ap.toString().equals("2.0, 594.5, -8.49834" + System.getProperty("line.separator") + "5.0, 230000.0, 49.4"))
			throw new IllegalStateException("New floatM concatenation broken");
		ap.setLineSeparator("...");
		ap.setColSeparator("+");
		if (! ap.toString().equals("2.0+594.5+-8.49834...5.0+230000.0+49.4"))
			throw new IllegalStateException("FloatM concatenation with custom separators probably broken");
		ap.transpose();
		if (! ap.toString().equals("2.0+5.0...594.5+230000.0...-8.49834+49.4"))
			throw new IllegalStateException("FloatM transposition probably broken");
	
		// Test boolean 1D
		ap = new ArrayPrinter(new boolean[] {true, false, false, true});
		if (! ap.toString().equals("true, false, false, true"))
			throw new IllegalStateException("New boolean concatenation broken");
		ap.setSeparator("...");
		if (! ap.toString().equals("true...false...false...true"))
			throw new IllegalStateException("Boolean concatenation with a custom separator probably broken");

		// Test boolean 2D
		ap = new ArrayPrinter(new boolean[][] {{false, true}, {false, false}});
		if (! ap.toString().equals("false, true" + System.getProperty("line.separator") + "false, false"))
			throw new IllegalStateException("New booleanM concatenation broken");
		ap.setLineSeparator("...");
		ap.setColSeparator("+");
		if (! ap.toString().equals("false+true...false+false"))
			throw new IllegalStateException("BooleanM concatenation with custom separators probably broken");
		ap.transpose();
		if (! ap.toString().equals("false+false...true+false"))
			throw new IllegalStateException("BooleanM transposition probably broken");
	
		// Test string 1D
		ap = new ArrayPrinter(new String[] {"A", "B", "c", "test"});
		if (! ap.toString().equals("A, B, c, test"))
			throw new IllegalStateException("New string concatenation broken");
		ap.setSeparator("...");
		if (! ap.toString().equals("A...B...c...test"))
			throw new IllegalStateException("String concatenation with a custom separator probably broken");

		// Test string 2D
		ap = new ArrayPrinter(new String[][] {{"A", "B"}, {"c", "test"}});
		if (! ap.toString().equals("A, B" + System.getProperty("line.separator") + "c, test"))
			throw new IllegalStateException("New StringM concatenation broken");
		ap.setLineSeparator("...");
		ap.setColSeparator("+");
		if (! ap.toString().equals("A+B...c+test"))
			throw new IllegalStateException("StringM concatenation with custom separators probably broken");
		ap.transpose();
		if (! ap.toString().equals("A+c...B+test"))
			throw new IllegalStateException("StringM transposition probably broken");
		
		// Test Object 1D
		ap = new ArrayPrinter(new Object[] {"A", "B", "c", "test"});
		if (! ap.toString().equals("A, B, c, test"))
			throw new IllegalStateException("New Object concatenation broken");
		ap.setSeparator("...");
		if (! ap.toString().equals("A...B...c...test"))
			throw new IllegalStateException("Object concatenation with a custom separator probably broken");

		// Test string 2D
		ap = new ArrayPrinter(new Object[][] {{"A", "B"}, {"c", "test"}});
		if (! ap.toString().equals("A, B" + System.getProperty("line.separator") + "c, test"))
			throw new IllegalStateException("New ObjectM concatenation broken");
		ap.setLineSeparator("...");
		ap.setColSeparator("+");
		if (! ap.toString().equals("A+B...c+test"))
			throw new IllegalStateException("ObjectM concatenation with custom separators probably broken");
		ap.transpose();
		if (! ap.toString().equals("A+c...B+test"))
			throw new IllegalStateException("ObjectM transposition probably broken");
		
		/* If no Exception were encountered, display a short success message */
		System.out.println("ArrayPrinter looks good");
	}

}
