package ch.unige.bprg.panelomix.utils;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;


/** Read a CSV file
 * @author Xavier Robin */

public final class PanelCSVReader {

	/* final: Once the data has been read, do not change anything */
	private final String[] titles;
	private final float[][] predictorsData;
	private final String response;
	private final String[] responseLevels;
	private final String[] responseData;
	private final String[] predictors;
	private final int cols;
	private final int rows;

	/**
	 * Read a CSV file
	 * @param String filenameCSV: the name of the file to convert
	 * @param predictors
	 * @param response
	 * @param levels: levels of the response
	 * @throws IOException
	 */
	public PanelCSVReader(String filenameCSV, String[] predictors, String response, String[] levels) throws IOException {
		// Open IO stream
		File inputCSV = new File(filenameCSV);
		BufferedReader bufferedCSV  = new BufferedReader(new FileReader(inputCSV));
		int rows = 0; // number of patients in the CSV file
		String line = null; // temp. variable to contain the read lines
		// read the first line
		String titleLineFile = bufferedCSV.readLine();
		// split it on ","
		String[] titles = titleLineFile.split(",");
		// remove the quotes
		for (int i = 0; i< titles.length; i++) {
//			cols++; // count the number of columns here
			titles[i] = titles[i].replaceAll("\"", "");
			titles[i] = titles[i].trim();
		}
		
		// Find the position of the predictors
		int[] moleculePositionsInTitle = new int[predictors.length];
		for (int i = 0; i < predictors.length; i++) {
			boolean moleculePositionFound = false;
			for (int j = 0; j < titles.length; j++) {
				if (predictors[i].equals(titles[j])) {
					moleculePositionsInTitle[i] =  j;
					moleculePositionFound = true;
				}
			}
			// Double-check that the molecule had its positions found. Otherwise, throw an error.
			if (! (moleculePositionFound ))
				throw new Error("The position of "+ predictors[i] + " was not found in the CSV file.");
		}
		// Find the position of the response
		int responsePositionInTitle = -1;
		for (int j = 0; j < titles.length; j++) {
			if (response.equals(titles[j])) {
				responsePositionInTitle = j;
			}
		}
		/* Double-check that the response had its positions found. Otherwise, throw an error. */
		if (responsePositionInTitle < 0)
			throw new Error("The position of "+ response + " was not found in the CSV file.");
		
		// Now actually read the data into readDataAL
		ArrayList<Float[]> predictorsArrayList = new ArrayList<Float[]>();
		ArrayList<String> responses = new ArrayList<String>();
		while ((line = bufferedCSV.readLine()) != null) { // content lines
			Float[] predictorsLine = new Float[predictors.length];
			rows++; // count the number of data rows
			String[] lineContent = line.split(",");
			// Check that lineContent has the same length as the titles
			if (lineContent.length != titles.length) {
				throw new Error("Invalid data line " + line + ": length differ from the titles");
			}
			for (int i = 0; i < lineContent.length; i++) {
				if (i == responsePositionInTitle) {
					responses.add(stripQuotes(lineContent[i].trim()));
				}
				else if (contains(predictors, titles[i])) {
					predictorsLine[index(predictors, titles[i])] = Float.valueOf(lineContent[i]);
				}
			}
			predictorsArrayList.add(predictorsLine);
		}
		
		// Now create a simple array table out of the predictorsArrayList variable
		float[][] predictorsData = new float[rows][predictors.length];
		for (int i = 0; i < predictors.length; i++) {
			for (int j = 0; j < rows; j++) {
				predictorsData[j][i] = predictorsArrayList.get(j)[i];
			}
		}
		
		// close the connection
		bufferedCSV.close();
		
		
		// finally, assign everything to the object
		this.cols = predictors.length;
		this.rows = rows;
		this.predictors = predictors;
		this.titles = titles;
		this.predictorsData = predictorsData;
		this.responseData = responses.toArray(new String[responses.size()]);
		this.response = response;

		// Generate the levels if required, and hope it is in the correct order!
		if (levels == null) {
			ArrayList<String> uniqueResponse = new ArrayList<String>();
			 
			for (String x : this.getResponseData())
			    if (!uniqueResponse.contains(x))
			    	uniqueResponse.add(x);
			levels = new String[2];
			levels[0] = uniqueResponse.get(0);
			levels[1] = uniqueResponse.get(1);
		}
		this.responseLevels = levels;
	}

	/** Does the array of Strings list contain the String search
	 * @param String[] list: the list in which to search
	 * @param String search: the searched element
	 * @return true if search is contained in list, false otherwise
	 */
	public boolean contains(String[] list, String search) {
		for (int i = 0; i < list.length; i++) {
			if (list[i].equals(search)) {
				return true;
			}
		}
		return false;
	}

	/** At which index of the array of Strings list is the String search located?
	 * @param String[] list: the list in which to search
	 * @param String search: the searched element
	 * @return the index of search in list if search is contained in list, -1 otherwise
	 */
	public int index(String[] list, String search) {
		for (int i = 0; i < list.length; i++) {
			if (list[i].equals(search)) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * Get the original titles of the data. Does not correspond to the actual data
	 * @param none
	 * @return String[]
	 */
	public String[] getTitles() {
		return this.titles;
	}

	/**
	 * Get the titles of the data. Does not correspond to the actual data
	 * @param int index: the index of the title to get
	 * @return String[]
	 */
	public String getTitle(int index) {
		return this.titles[index];
	}

	/**
	 * Get the predictors selected in the data
	 * @param none
	 * @return String[]
	 */
	public String[] getPredictors() {
		return this.predictors;
	}

	/**
	 * Get the predictor selected in the data at the index
	 * @param int index: the requested index
	 * @return String
	 */
	public String getPredictor(int index) {
		return this.predictors[index];
	}

	/**
	 * Get the whole computed predictors data in an array of float[predictor][patient]
	 * @param none
	 * @return float[][]: the values of all the predictors for all the patients
	 */
	public float[][] getPredictorsData() {
		float[][] ret = new float[getCols()][getRows()];
		for (int i = 0; i < ret.length; i++) {
			for (int j = 0; j < ret[i].length; j++) {
				ret[i][j] = this.predictorsData[j][i];
			}
		}
		return ret;
	}

	/**
	 * Get the whole computed predictors data in an array of float[patient][predictors]
	 * @param none
	 * @return float[][]: the values of all the predictors for all the patients
	 */
	public float[][] getPatientsData() {
		return this.predictorsData;
	}

	/**
	 * Get the computed predictors data for the predictor / patient
	 * @param int predictor: the index of the predictor to get
	 * @param int patient: the index of the patient
	 * @return float
	 */
	public float getPredictorData(int predictor, int patient) {
		return this.predictorsData[predictor][patient];
	}

	/**
	 * Get the computed patients data for the patient / predictor
	 * @param int patient: the index of the patient
	 * @param int predictor: the index of the predictor to get
	 * @return float
	 */
	public float getPatientData(int patient, int predictor) {
		return getPatientsData()[patient][predictor];
	}

	/**
	 * Get the computed predictors data for the predictor / patient
	 * @param String predictor: the name of the predictor to get
	 * @param int patient: the index of the patient
	 * @return float: the value of the predictor for the patient
	 * @throws Error if the name of the predictor cannot be found
	 */
	public float getPredictorData(String predictor, int patient) throws Error {
		return getPredictorData(predictor)[patient];
	}
	
	/**
	 * Get the computed predictors data for the predictor and all patients
	 * @param String predictor: the name of the predictor to get
	 * @return float: the values of the patient for the predictor
	 * @throws Error if the name of the predictor cannot be found
	 */
	public float[] getPredictorData(String predictor) throws Error {
		int i = index(this.predictors, predictor);
		if (i == -1) 
			throw new Error("Predictor not found in the predictor list");
		else
			return getPredictorData(i);
	}

	
	/**
	 * Get the computed data for the predictor and all the patients
	 * @param int predictor: the index of the patient
	 * @return float: the value of the patients for the predictor
	 */
	public float[] getPredictorData(int predictor) {
		return getPredictorsData()[predictor];
	}

	/**
	 * Get the computed data for the patient and all predictors
	 * @param int patient: the index of the patient
	 * @return float: the value of the predictor for the patient
	 */
	public float[] getPatientData(int patient) {
		return getPatientsData()[patient];
	}

	/**
	 * Get the responses in whole
	 * @param none
	 * @return String[]
	 */
	public String[] getResponseData() {
		return this.responseData;
	}
	
	/**
	 * Get the response for patient row
	 * @param row: the patient
	 * @return String
	 */
	public String getResponseData(int row) {
		return this.responseData[row];
	}
	
	/**
	 * Get the response for patient row
	 * @param none
	 * @return String
	 */
	public String[] getResponseLevels() {
		return this.responseLevels;
	}

	/**
	 * Get the number of columns in the data
	 * @param none
	 * @return int
	 */
	public int getCols() {
		return this.cols;
	}

	/**
	 * Get the response in the data
	 * @return String
	 * @param none
	 */
	public String getResponse() {
		return this.response;
	}

	/**
	 * Get the number of rows in the data
	 * @return int
	 * @param none
	 */
	public int getRows() {
		return this.rows;
	}

	/**
	 * Removes the quotes surrounding a string if there are quotes. Otherwise returns the intact string
	 * @param string
	 * @return
	 */
	static String stripQuotes(String string) {
		if (string.matches("\".+\"")) 
			string = string.substring(1, string.length()-1);
		return string;
	} // stripQuotes

	/**
	 * Tests the class
	 * @param args
	 * @throws IOException if it is not possible to create the temp file
	 * @throws IllegalStateException in case of unexpected results
	 */
	public static void main(String[] args) throws IOException {
		// Define the variables we'll use for the test
		File tmp;
		String[][] dataToWrite = new String[][] {new String[] {"1", "2.3", "4.4", "Good"}, new String[] {"2", "-5", "5933.4", "Bad"}, new String[] {"3", "0", "4.1", "Intermediate"}};
		String[] expectedTitles = new String[] {"ID", "par1", "par.2", "res"};
		float[][] expectedData = new float[][] {new float[] {4.4f, 2.3f}, new float[] {5933.4f, -5f}, new float[] {4.1f, 0f}};
		String[] expectedResponses = new String[] {"Good", "Bad", "Intermediate"};
		// Define a temporary file
		tmp = File.createTempFile("testReadCSV", ".csv");
		// Request that it be deleted at the end
		tmp.deleteOnExit();
		// Write some data in it
		FileWriter fw = new FileWriter(tmp.getCanonicalPath().toString());
		fw.write(new ArrayPrinter(new String[] {new ArrayPrinter(expectedTitles).toString(), new ArrayPrinter(dataToWrite).toString()}, "\n").toString());
		fw.flush();
		fw.close();
		// read it with ReadCSV
		PanelCSVReader rcsv = new PanelCSVReader(tmp.getCanonicalPath(), new String[] {"par.2", "par1"}, "res", new String[] {"Good", "Bad", "Intermediate"});
		// Check the number of columns
		if (rcsv.getCols()!=2) {
			throw new IllegalStateException("Wrong number of columns");
		}
		// Check the number of rows
		if (rcsv.getRows()!=3) {
			throw new IllegalStateException("Wrong number of rows");
		}
		// Check the content
		for (int col = 0; col < 2; col++) {
			// title
			if (! rcsv.getTitles()[col].equals(expectedTitles[col])) {
				throw new IllegalStateException("Illegal title " +  rcsv.getTitles()[col] + " in position " + col + ": expected " + expectedTitles[col]);
			}
			// title
			if (! rcsv.getTitle(col).equals(expectedTitles[col])) {
				throw new IllegalStateException("Illegal title " +  rcsv.getTitle(col) + " in position " + col + ": expected " + expectedTitles[col]);
			}
			// data with getPatientsData()[row][col]
			for (int row = 0; row < 3; row++) {
				if (Float.compare(rcsv.getPatientsData()[row][col], expectedData[row][col])!=0) {
					System.out.println(row);
					System.out.println(col);
					System.out.println(expectedData[row][col]);
					System.out.println(rcsv.getPatientsData()[row][col]);
					System.out.println(new ArrayPrinter(expectedData));
					System.out.println(new ArrayPrinter(rcsv.getPatientsData()));
					throw new IllegalStateException("Illegal data " +  rcsv.getPatientsData()[row][col] + " in position " + col + ", " + row + ": expected " + expectedData[row][col]);
				}
				// data with getPatientData(row, col)
				if (Float.compare(rcsv.getPatientData(row, col), expectedData[row][col])!=0)
					throw new IllegalStateException("Illegal data " +  rcsv.getPatientData(row, col) + " in position " + col + ", " + row + ": expected " + expectedData[row][col]);
			
				// data with getPatientsData()[col][row]
				if (Float.compare(rcsv.getPatientsData()[row][col], expectedData[row][col])!=0)
					throw new IllegalStateException("Illegal data " +  rcsv.getPatientsData()[row][col] + " in position " + col + ", " + row + ": expected " + expectedData[row][col]);
	
				// data with getPatientData(col, row)
				if (Float.compare(rcsv.getPatientData(row, col), expectedData[row][col])!=0)
					throw new IllegalStateException("Illegal data " +  rcsv.getPatientData(row, col) + " in position " + col + ", " + row + ": expected " + expectedData[row][col]);
			}
		}	
		for (int row = 0; row < 3; row++) {
			// responses with getResponseData()[row]
			if (! expectedResponses[row].equals(rcsv.getResponseData()[row]))
				throw new IllegalStateException("Illegal response " +  rcsv.getResponseData()[row] + " in patient " + row + ": expected " + expectedResponses[row]);
			// responses with getResponseData(row)
			if (! expectedResponses[row].equals(rcsv.getResponseData(row)))
				throw new IllegalStateException("Illegal response " +  rcsv.getResponseData(row) + " in patient " + row + ": expected " + expectedResponses[row]);
			// par.2
			if (Float.compare(rcsv.getPredictorData("par.2")[row], expectedData[row][0])!=0) {
				throw new IllegalStateException("Illegal data " +  rcsv.getPredictorData("par.2")[row] + " in position " + row + ": expected " +  expectedData[row][0]);
			}
			if (Float.compare(rcsv.getPredictorData(0)[row], expectedData[row][0])!=0) {
				throw new IllegalStateException("Illegal data " +  rcsv.getPredictorData(0)[row] + " in position " + row + ": expected " +  expectedData[row][0]);
			}
			// par1
			if (Float.compare(rcsv.getPredictorData("par1")[row], expectedData[row][1])!=0) {
				throw new IllegalStateException("Illegal data " +  rcsv.getPredictorData("par1")[row] + " in position " + row + ": expected " +  expectedData[row][1]);
			}
			if (Float.compare(rcsv.getPredictorData(1)[row], expectedData[row][1])!=0) {
				throw new IllegalStateException("Illegal data " +  rcsv.getPredictorData(1)[row] + " in position " + row + ": expected " +  expectedData[row][1]);
			}
			// par.2
		}
		/* If no Exception were encountered, display a short success message */
		System.out.println("ReadCSV looks good");
	}
	
}

