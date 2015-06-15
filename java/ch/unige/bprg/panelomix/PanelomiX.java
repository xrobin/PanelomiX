package ch.unige.bprg.panelomix;

import ch.unige.bprg.panelomix.utils.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.regex.*;


/** 
 * Choose best biomarkers panels.
 * @author Xavier Robin
 * @version 0.5
 * @notes
 * 
 */
public class PanelomiX {
	/* Program state variable */
	/** Are we in debug mode? 
	 * Default: false
	 * Use -d command switch to turn on.*/
	private boolean debug = false;
	/** Are we in silent mode? 
	 * Default: false
	 * Use -s command switch to turn on.*/
	private boolean silent = false;
	
	/** What will we optimize? */
	protected PanelOptimizeOptions optimizeOptions;

	/** The best sensitivity achieved yet */
	protected float maxSe = -1;
	protected float maxSp = -1;
	protected float maxAcc = -1;
	/** The number of iterations that have been tested yet */
	private long iterations = 0; // stores the total number of iterations for information
	/** The time the panel has been running */
	private long timeStart;
	private double execTime;

	/* Configuration options with default values */
	/** Number of processors on which the program will be run */
	private int nProc = Runtime.getRuntime().availableProcessors();
	private int monitor = 100;

	/* Configuration options without default values */
	/** The name of the predictors. Syntax in config file:<br>
	 * predictors:"predictor1","predictor2",[…]
	 */
	/** The levels of the response. Array of length 2. level[0] = negative level, level[1] = positive.
	 * Default: will be taken as the unique values of the response data. Beware of inversions if the positive patients come before in the data */
	private String[] levels;
	/** The predictors to be tested for insertion */
	private String[] predictors = new String[0];
	/** The predictors that must be forced into the panel */
	private String[] fixedPredictors = new String[0];
	/** All the predictors, both fixed and optional */
	protected String[] allPredictors;
	/** The response column. Syntax in config file:<br>
	 * response:"response" */
	private String response;
	/** HashMap<String, float[]>: the thresholds to test. Syntax in config file:<br>
	 * thresholds:<br>
	 * -"predictor1":threshold1,threshold2,[…]<br>
	 * -"predictor2":threshold1,threshold2,[…]<br>
	 * […]<br>
	 * with threshold1, threshold2: floats;
	 */
	protected HashMap<String, float[]> thresholds = new HashMap<String, float[]>();
	/** HashMap<String, String>: the directions of the thresholds to test. Syntax in config file:<br>
	 * directions:<br>
	 * -"predictor1":direction1<br>
	 * -"predictor2":direction2<br>
	 * […]<br>
	 * with direction1, direction2: one of "<" or ">" (without quotes).<br>
	 * The direction is 'controls direction cases' so that "<" means controls lower than cases;
	 */
	protected HashMap<String, String> directions = new HashMap<String, String>();
	/** The sizes of the panels to check . Syntax in config file:<br>
	 * panels.of.num:Size1,Size2,[…]<br>
	 * with Size1, Size2: integers
	 * */
	private int[] panelSizes;
	/** The name of the data file, in CSV format. Syntax in config file:<br>
	 * datafile:"data.csv"
	 */
	private String filenameCSV;
	/** The name of the output file. Syntax in config file:<br>
	 * outputfile:"result"
	 */
	private String filenameOutput;
	/** The name of the progress file. Syntax in config file:<br>
	 * progressfile:"progress"
	 */
	private String filenameProgress;
	/* Variables that will be configured using the configuration options */
	/** The actual data, read from filenameCSV. */
	protected PanelCSVReader dataCSV;
	/** The output object, similar to a PrintStream. Data is either written to System.out or filenameOutput or both (in debug mode)*/
	protected PanelPrinter outputStream;
	/** The text progress object.*/
	protected PrintStream progressStream;
//	/** The precomputed array of data/thresholds comparisons, used to build the scores 
//	 * Dimensions:<br>
//	 * 1. predictors<br>
//	 * 2. predictor thresholds (the longest length of predictor thresholds is used)<br>
//	 * 3. patients*/
//	static int[][][] arrayPrecomputed;
	/** The list of panels */
	private PanelList panelList;
	/** The minimal number of true negatives that keep specificity > minConstr */
	protected int minTN;
	/** The minimal number of true positives that keep sensitivity > minConstr */
	protected int minTP;
	/** Number of positive patients (with response as levels[0]) */
	protected int numPatNeg = 0;
	/** Number of positive patients (with response as levels[1]) */
	protected int numPatPos = 0;

	/** The list of panels to test */
	protected MultisizeCombinationGenerator panels;
	/** The list of molecules index that are fixed */
	protected int[] fixedPanel;
	/** the response data */
	protected boolean[] responseData;

	/** the precomputed array of scores 
	 * as int[predictor][threshold][patient]
	 */
	protected byte[][][] arrayPrecomputed;

	/** the working threads */
	private ArrayList<PanelomiXThreads> threads;
	private PanelProgressMonitor threadMonitor;

	private int bestLength;
	private boolean configured;
	private int missingthresholds = 0; // some molecules may not have thresholds: keep it here
	
	public PanelomiX(String[] args) throws Exception {
		/* Configure the program */
		parseArgs(args);
		panelList = new PanelList();
	}

	/**
	 * Constructor: PanelomiX
	 * Just constructs a new instance of PanelomiX.
	 * Does not configure anything: use the defaultConfigure or parseConfFile methods,
	 * or set the configuration elements individually with the accessors (setResponse, setDataFile, putPredictor, …)
	 */
	public PanelomiX() {
		/* Do nothing here, just create the instance */
		panelList = new PanelList();
	}
	
	/**
	 * Prepares the panel. Will read the data, prepare the output
	 * @throws IOException
	 */

	private void prepareStart() throws IOException {
		if (missingthresholds > 0) {
			String[] newPredictors = new String[predictors.length - missingthresholds];
			int newI = 0;
			for (int i = 0; i < predictors.length; i++) {
				float[] thres = thresholds.get(predictors[i]);
				if (thres != null) {
					newPredictors[newI] = predictors[i];
					newI++;
				}
			}
			predictors = newPredictors;
		}
		/* Array with all predictors */
		allPredictors = ArrayConcat.c(predictors, fixedPredictors);

		/* OPEN THE OUTPUT */
		if (filenameOutput == null) { // If output file wasn't defined, use System.out
			if (debug) 
				outputStream = new PanelPrinter(this, System.out);
			else
				outputStream = new PanelPrinter(this);
		}
		else if (debug) // in case of debug, we always print to System.out
			outputStream = new PanelPrinter(this, System.out, filenameOutput);
		else // if not debug and filenameOutput was defined, use only this stream
			outputStream = new PanelPrinter(this, filenameOutput);
		if (filenameProgress != null)
			progressStream = new PrintStream(filenameProgress);

		/* READ THE DATA */
		dataCSV = new PanelCSVReader(filenameCSV, allPredictors, response, levels);

		/* GENERATE THE BIG ARRAY */
		/* Generate an integer 3D array storing the amount that must be incremented on each patient
		 * and for each molecule/threshold combination
		 * D1: the molecule 
		 * D2: the thresholds
		 * D3: the patients
		 */
		/* Define the second dimension. Corresponds to the maximum number of thresholds for 1 mol. */
		int d2 = 0;
		for (int i = 0; i < allPredictors.length; i++) {
			if(thresholds.get(allPredictors[i]).length > d2)
				d2 = thresholds.get(allPredictors[i]).length;
		}
		/* Declare the array */
		arrayPrecomputed = new byte[allPredictors.length][d2][dataCSV.getRows()];
		// Fill it
		for (int i = 0; i < allPredictors.length; i++) { // predictor
			for (int col = 0; col < thresholds.get(allPredictors[i]).length; col++) { // threshold
				for (int row = 0; row < dataCSV.getRows(); row++) { // patient
					if (directions.get(allPredictors[i]).equals(">")) {
						if (dataCSV.getPatientData(row, i) < thresholds.get(allPredictors[i])[col])
							arrayPrecomputed[i][col][row] = 1;
						else
							arrayPrecomputed[i][col][row] = 0;
					}
					else {
						if (dataCSV.getPatientData(row, i) > thresholds.get(allPredictors[i])[col])
							arrayPrecomputed[i][col][row] = 1;
						else
							arrayPrecomputed[i][col][row] = 0;
					}
				}
			}
		}
		
		/* FIND OUT THE NUMBER OF POSITIVE OR NEGATIVE PATIENTS */
		responseData = new boolean[dataCSV.getRows()];
		for (int i = 0; i< dataCSV.getRows(); i++) {
			if (dataCSV.getResponseData(i).equals(dataCSV.getResponseLevels()[0])) {
				numPatNeg++;
				responseData[i] = false;
			}
			else if (dataCSV.getResponseData(i).equals(dataCSV.getResponseLevels()[1])) {
				numPatPos++;
				responseData[i] = true;
			}
			else
				throw new Error("Unexpected patient class. Must be one of " + new ArrayPrinter(dataCSV.getResponseLevels()) + ", not " + dataCSV.getResponseData(i) + ".");
		}
		/* MINIMUM ALLOWED NUMBER OF TN */
		/* This avoids computing sp each time. Seemingly only a very small gain, not sure if significant */
		int tn = numPatNeg;
		int tp = numPatPos;
		if (optimizeOptions.isFocusSp()) {
			while ((float)tn/numPatNeg >= optimizeOptions.getMinConstr()) { // compute the specificity and check if it is sufficient
				minTN = tn;
				tn--;
			}
		}
		else if (optimizeOptions.isFocusSe()) {
			while ((float)tp/numPatPos >= optimizeOptions.getMinConstr()) { // compute the sensitivity and check if it is sufficient
				minTP = tp;
				tp--;
			}
		}

		/* DEFINE AND TEST THE COMBINATIONS */
		panels = new MultisizeCombinationGenerator(predictors.length, panelSizes);
		// Also define the indices of the fixedPanel
		fixedPanel = new int[fixedPredictors.length];
		// No range operator (like a:b in R or a..b in perl), so use a loop
		for (int i = 0; i < fixedPredictors.length; i++) {
			fixedPanel[i] = predictors.length + i;
		}
/*		// Panel for SAH
 		panel = new int[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
 		iteratePanel(panel);
 		System.exit(1);*/

/*		// Panel for SMS
		panel = new int[] {0, 1, 2};
		iteratePanel(panel);
		System.exit(1);*/
		
		/* Start the progress bar */
		threadMonitor = null;
		if (this.monitor > 0)
			threadMonitor = new PanelProgressMonitor(this);
		/* Text progress */
		if (filenameProgress != null) {
			/* If we have only fixed predictors, getTotal() will compute 0. But this is
			 * wrong because we still have 1 panel to test. */
			if (panels.getTotal() == 0 && fixedPredictors.length > 0)
				progressStream.println("Total panels: 1");
			else
				progressStream.println("Total panels: " + panels.getTotal());
		}

		threads = new ArrayList<PanelomiXThreads>();
		if (panels.getTotal() < nProc)
			nProc = (int) panels.getTotal();
		/* If we have only fixed predictors, getTotal() will compute 0. But this is
		 * wrong because we still have 1 panel to test. */
		if (nProc == 0 && fixedPredictors.length > 0)
			nProc = 1;
		for (int proc = 0; proc < nProc; proc++) {
			if (debug)
				System.out.println("Starting thread " + proc);
			PanelomiXThreads newPanelomixThreads = new PanelomiXThreads(this);
			threads.add(newPanelomixThreads);
		}
	}

	/**
	 * Actually start running the panel. First checks that the required options are ok (checkRequiredOptions()).
	 * @param void
	 * @throws IOException
	 * @value void
	 */
	private void start() throws IOException {
		String[] errors = this.checkRequiredOptions();

		/* Start time tracking (benchmark) */
		this.timeStart = System.nanoTime();

		if (errors == null) {
			prepareStart();
			for (PanelomiXThreads panelThread : threads) {
				panelThread.getThread().start();
				if (debug)
					System.out.println("Started thread " + panelThread.getThread().getName());
			}
			if (threadMonitor != null) {
				threadMonitor.getThread().start();
				if (debug)
					System.out.println("Started thread monitor " + threadMonitor.getThread().getName());
			}
		}
		else {
			throw new IncompleteConfigurationException(errors);
		}
	}

	public void join() throws InterruptedException {
		join(false, false);
	}
	/**
	 * Join the panels to end the search
	 * @param void
	 * @throws InterruptedException 
	 * @value void
	 */
	private void join(boolean showDoneButton, boolean exitWhenDone) throws InterruptedException {
		/* WAIT FOR THE PROCESS TO TERMINATE */
		for (PanelomiXThreads panelThread : threads) {
			Thread thread = panelThread.getThread();
			if (debug)
				System.out.println("Joining thread " + thread.getName() + " state: " + thread.getState() + " iter: " + panelThread.iterations);
			thread.join();
			if (debug)
				System.out.println("Joined thread " + thread.getName() + " state: " + thread.getState() + " iter: " + panelThread.iterations);
			this.iterations += panelThread.iterations;
		}

		if (threadMonitor != null && showDoneButton) {
			threadMonitor.setDone(true); // signal to monitor thread that it is done
			threadMonitor.showDone(exitWhenDone);
			threadMonitor.getThread().join();
		}
		else if (threadMonitor != null) {
			threadMonitor.setDone(true); // signal to monitor thread that it is done
			threadMonitor.getThread().join();
		}

		/* END WITH STATISTICS */
		long timeEnd = System.nanoTime();
		this.execTime = (timeEnd - timeStart) * 1E-9;
	}
	
	/**
	 * Run the panel search. Executes start() and join()
	 * @param void
	 * @throws InterruptedException 
	 * @throws IOException 
	 * @value void
	 */
	public PanelList run() throws InterruptedException, IOException {
		start();
		join(false, false);
		return panelList;
	}

	/**
	 * @param the parameters are currently defined directly in the body of the main function
	 * @param predictors (String[]): the molecules to test
	 * @param response (String): response
	 * @throws IOException
	 * @throws NumberFormatException
	 * @throws Error
	 * @throws InterruptedException 
	 */
	public static void main(String[] args) throws Exception, NumberFormatException, InterruptedException {
		
		/* Initialize and start the search */
		PanelomiX p = new PanelomiX();

		//if (! p.configured) 
		p.parseArgs(args);
		if (!p.configured) {
			p.printHelpAndExit(1);
		}
		
		p.setProgressBar(false);
		/* Start the search */
		p.start();
		/* Wait until end */
		p.join(true, true);
		
		/* Print the PanelList */
		//p.outputStream.print(p.panelList.toString());
		
		/* Print the statistics */
		p.outputStream.println("Iterations: " + p.iterations);
		p.outputStream.printf("Time %.3fs", p.execTime);

	} // main

	
	/**
	 * Parses the command-line arguments
	 * The syntax is the following:
	 * Call: java PanelomiX [-h] -c file
	     -h: prints this help message
	     -d: debug mode. Debug output is printed to the console. Note that if -d is defined after -c, configuration options cannot be debugged.
	     -c ConfigFile: the filename of a configuration file.
	     -SAH: use a standard dataset (SAH) and sensible options. Should not be used with -c.
	 * @param String[] args: arguments from main()
	 * @return void 
	 * @throws IOException 
	 */
	public void parseArgs(String[] args) throws Exception {
		for (int i = 0; i < args.length; i++) {
			Matcher Matcher;
			if (args[i].matches("^-?-h$")) {
				printHelpAndExit(0);
			}
			else if (args[i].matches("^-?-d$")) {
				System.out.println("Entering debug mode.");
				debug = true;
			}
			else if ((Matcher = Pattern.compile("^-?-p=(.+)$").matcher(args[i])).matches()) {
				if (Matcher.group(1).compareTo("0") != 0) {
					filenameProgress = Matcher.group(1);
				}
			}
			else if (args[i].matches("^-?-c$")) {
				i++;
				parseConfFile(args[i]);
			}
			else if ((Matcher = Pattern.compile("^-?-c=(.+)$").matcher(args[i])).matches()) {
				parseConfFile(Matcher.group(1));
			}
			else if ((Matcher = Pattern.compile("^-?-m=(.+)$").matcher(args[i])).matches()) {
				monitor = new Integer(Matcher.group(1));
			}
			else if (args[i].matches("^-?-SAH$")) {
				defaultConfigure();
			}
			else 
				throw new IllegalArgumentException("Unrecognized command line argument: " + args[i]);
		}
	}

	/** Define sensible configuration values for SAH.
	 * Should be used only if the configuration is not done.
	 */	
	public void defaultConfigure() throws AlreadyConfiguredException {
		if (configured == true)
			throw new AlreadyConfiguredException("Panel already configured");
		//predictors = new String[] {"ndka", "fabp", "s100", "troponin", "ufd1", "wfns", "vasospasme", "age", "fisher", "dj1"};		
		response = "outcome";
		this.putFixedPredictor("ndka", "<", new float[] {8.125f, 11.08f, 12.73f, 13.14f, 20.99f, 30.43f} );
		this.putFixedPredictor("s100", "<", new float[] {0.115f, 0.205f, 0.345f, 0.435f, 0.475f, 0.51f} );
		this.putFixedPredictor("fabp", "<", new float[] {1.105f, 1.24f, 1.87f, 2.965f, 5.88f});
		this.putFixedPredictor("troponin", "<", new float[] {0.045f, 0.315f, 1.385f, 1.56f, 2.325f, 3.44f, 4.625f});
		this.putFixedPredictor("ufd1", "<", new float[] {185f, 271.5f, 319.9f});
		this.putFixedPredictor("wfns", "<", new float[] {1.5f, 2.5f, 3.5f, 4.5f});
		this.putFixedPredictor("vasospasme", "<", new float[] {0.5f});
		this.putFixedPredictor("age", "<", new float[] {36.5f, 53.5f, 61.5f, 67.5f});
		this.putFixedPredictor("fisher", "<", new float[] {2.5f, 3.5f, 4.5f});
		this.putFixedPredictor("dj1", "<", new float[] {95.97f, 154.3f});
		panelSizes = new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9};
		//panelSizes = new int[] {5};
        filenameCSV = "/home/xrobin/Java/perl/data_withtitle.csv";
        this.setOutputFile("/home/xrobin/Bureau/sah.txt");
        this.setProgressFile("/home/xrobin/Bureau/sah_progress.txt");
        levels = new String[] {"0", "1"};
        setOptimizeOptions("specificity", 1);
        //setOptimizeOptions("accuracy");
        configured = true;
        
	} // defaultConfigure

	/**
	 * Reads the configuration file and configures the search.
	 * Defaults to focus on 100% specificity.
	 * @param configFileName
	 * @throws IOException 
	 */
	public void parseConfFile(String configFileName) throws Exception, AlreadyConfiguredException {
		File configFileObj = new File(configFileName);
		BufferedReader bufferedConfigFile  = new BufferedReader(new FileReader(configFileObj));
		String line;
		
		float minConstr = 1;
		String focusOn = "specificity";
		while ((line = bufferedConfigFile.readLine()) != null) { // read the lines
			Matcher m;
			// Comment
			if (line.startsWith("#")) {
				if (debug)
					System.out.println("Comment line: " + line);
			}
			// Response
			else if (line.startsWith("response:")) {
				response = line.substring(9);
				setResponse(stripQuotes(response));
				if (debug)
					System.out.println("Response: " + response);
			}
			// Response levels
			else if (line.startsWith("levels:")) {
				String[] levelConf = line.substring(7).split(",");
				if (levelConf.length != 2) {
					throw new InvalidConfigurationException("\"levels:\" must be of length 2");
				}
				levelConf[0] = stripQuotes(levelConf[0]);
				levelConf[1] = stripQuotes(levelConf[1]);
				setLevels(levelConf);
				if (debug)
					System.out.println("Levels: " + new ArrayPrinter(levelConf));
			}
			// Predictors
			else if (line.startsWith("predictors:")) {
				predictors = line.substring(11).split(",");
				for (int i = 0; i < predictors.length; i++) {
					predictors[i] = stripQuotes(predictors[i]);
				}
				if (debug)
					System.out.println("Predictors: " + new ArrayPrinter(predictors));
			}
			// Fixed predictors
			else if (line.startsWith("fixed.predictors:")) {
				fixedPredictors = line.substring(17).split(",");
				for (int i = 0; i < fixedPredictors.length; i++) {
					fixedPredictors[i] = stripQuotes(fixedPredictors[i]);
				}
				if (debug)
					System.out.println("Fixed predictors: " + new ArrayPrinter(fixedPredictors));
			}
			// Data file
			else if (line.startsWith("datafile:")) {
				filenameCSV = line.substring(9);
				filenameCSV = stripQuotes(filenameCSV);
				if (configFileObj.getParent() != null)
					filenameCSV = configFileObj.getParent() + File.separator + filenameCSV;
				if (debug)
					System.out.println("Data file: " + filenameCSV);
			}
			// Maximum size
			else if (line.startsWith("panels.of.num:")) {
				panelSizes = stringsToInt(line.substring(14).split(","));
				if (debug)
					System.out.println("Panel lengthes: " + new ArrayPrinter(panelSizes));
			}
			else if (line.startsWith("maxsize:")) {
				panelSizes = arrayOneTo(Integer.valueOf(line.substring(8)));
				if (debug)
					System.out.println("Computed panel lengthes: " + panelSizes);
			}
			// Thresholds
			else if (line.startsWith("thresholds:")) {
				while ((line = bufferedConfigFile.readLine()).startsWith("-")) {
					/* we need to be able to come back here after the next line = bufferedConfigFile.readLine()
					 * if it doesn't start with "-". mark() with a large number allows this using reset().
					 */
					bufferedConfigFile.mark(10000);
					int colonIndex = line.indexOf(":");
					String molName = line.substring(1, colonIndex);
					molName = stripQuotes(molName);
					String[] molThresholds = line.substring(colonIndex+1).split(",");
					if (! molThresholds[0].equals("")) {
						try {
							float[] molThresholdsFloat = stringsToFloat(molThresholds);
							thresholds.put(molName, molThresholdsFloat);
							if (debug)
								System.out.println("Threshold for " + molName + ": " + new ArrayPrinter(molThresholdsFloat));
						} catch(NumberFormatException e) {
							throw new Exception("Number format exception for " + molName + ": " + new ArrayPrinter(molThresholds));
						}
					}
					else {
						missingthresholds += 1;
						if (debug)
							System.out.println("Threshold missing for " + molName);
					}
				}
				bufferedConfigFile.reset();
			}
			// Directions
			else if (line.startsWith("directions:")) {
				while ((line = bufferedConfigFile.readLine()).startsWith("-")) {
					/* we need to be able to come back here after the next line = bufferedConfigFile.readLine()
					 * if it doesn't start with "-". mark() with a large number allows this using reset().
					 */
					bufferedConfigFile.mark(10000);
					int colonIndex = line.indexOf(":");
					String molName = line.substring(1, colonIndex);
					molName = stripQuotes(molName);
					String molDirection = line.substring(colonIndex+1);
					directions.put(molName, molDirection);
					if (debug)
						System.out.println("Direction for " + molName + ": " + molDirection);
				}
				bufferedConfigFile.reset();
			}
			// Minimal value of the specificity/sensitivity
			else if (line.startsWith("min.constr:")) {
				minConstr = Float.valueOf(line.substring(11));
				if (debug)
					System.out.println("Minimum constraint: " + minConstr);
			}
			else if ((m = Pattern.compile("^constrain\\.on:(sensitivity|specificity|accuracy)").matcher(line)).find()) {
				
				focusOn = m.group(1);
				if (debug)
					System.out.println("Focus on: " + focusOn);
			}
			// Output file
			else if (line.startsWith("outputfile:")) {
				filenameOutput =  line.substring(11);
				filenameOutput = stripQuotes(filenameOutput);
				if (configFileObj.getParent() != null)
					filenameOutput = configFileObj.getParent() + File.separator + filenameOutput;
				if (debug)
					System.out.println("Output file: " + filenameOutput);
			}
			// Progress file
			else if (line.startsWith("progressfile:")) {
				filenameProgress =  line.substring(13);
				filenameProgress = stripQuotes(filenameProgress);
				if (configFileObj.getParent() != null)
					filenameProgress = configFileObj.getParent() + File.separator + filenameProgress;
				if (debug)
					System.out.println("Progress file: " + filenameProgress);
			}
			// Number of processors to use.
			else if (line.startsWith("num.proc:")) {
				nProc = Integer.valueOf(line.substring(9));
				if (debug)
					System.out.println("Number of processors to use: " + nProc);
			}
			else if (line.startsWith("id:")) {/* ignore */}
			else
				throw new Error("Unrecognized configuration line: " + line);
		}
		
		setOptimizeOptions(focusOn, minConstr);
		
		/* close the connection */
		bufferedConfigFile.close();
		// panelSizes might not be defined in the conf file. If so, use a sensible default value
		if (panelSizes == null) {
			panelSizes = arrayOneTo(predictors.length);
		}
		/* Signal that the panel is now configured (we'll check it is really valid just before we start) */
		configured = true;
	}
	
	/** Checks the options required for the panel to run were entered correctly
	 * Namely:
	 * - response missing
	 * - predictors missing
	 * - thresholds missing
	 * - consistent number of predictors and thresholds (make sure you didn't define a predictor twice)
	 * - data missing
	 * @return String[]: the errors encountered. Null means no error.
	 * Side effects: set the "configured" flag on true upon success
	 */
	public String[] checkRequiredOptions() {
		/* Check that the required options were defined */
		ArrayList<String> errors = new ArrayList<String>();
		if (response == null)
			errors.add("missing response");
		if (predictors == null && fixedPredictors == null)
			errors.add("missing predictors");
		else if (thresholds.isEmpty()) {
			System.out.println(thresholds.size());
			errors.add("missing thresholds");
		}
		else if (thresholds.size() + missingthresholds != predictors.length + fixedPredictors.length) {
			System.out.println(thresholds.size());
			System.out.println(allPredictors.length);
			System.out.println(new ArrayPrinter(allPredictors));
			errors.add("Inconsistent number of predictors or thresholds");
		}
		if (filenameCSV == null)
			errors.add("missing data");
		if (errors.isEmpty()) {
			configured = true;
			return null;
		}
		else return errors.toArray(new String[errors.size()]);

	} // parseConfFile

	/**
	 * Converts an array of type string to the corresponding array of int
	 * @param String[] strings: the array of strings to convert
	 * @return int[]: the converted array of ints.
	 * @throws NumberFormatException
	 */
	private static int[] stringsToInt(String[] strings) throws NumberFormatException {
		int[] ints = new int[strings.length];
		for (int i = 0; i < strings.length; i++) {
			ints[i] = Integer.valueOf(strings[i]);
		}
		return ints;
	} // stringsToInt

	/**
	 * Converts an array of type string to the corresponding array of floats
	 * @param String[] strings: the array of strings to convert
	 * @return float[]: the converted array of floats.
	 * @throws NumberFormatException
	 */
	private static float[] stringsToFloat(String[] strings) throws NumberFormatException {
		float[] floats = new float[strings.length];
		for (int i = 0; i < strings.length; i++) {
			floats[i] = Float.valueOf(strings[i]);
		}
		return floats;
	} // stringsToFloat
	
	/**
	 * returns the array 1:n
	 * @param int n: number to climb to
	 * @return int[]: the array 1:n
	 */
	private static int[] arrayOneTo(int n) {
		int[] a = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = i+1;
		}
		return a;
	} // arrayOneTo

	/**
	 * Removes the quotes surrounding a string if there are quotes. Otherwise returns the intact string
	 * @param string
	 * @return
	 */
	private static String stripQuotes(String string) {
		if (string.matches("\".+\"")) 
			string = string.substring(1, string.length()-1);
		return string;
	} // stripQuotes

	/**
	 * Iterate on all the thresholds
	 * @param int[] panel: integers of the molecules in the current panel
	 * @return void
	 */
	/* The actual code is kept in old version 0.3
	 * private static void iteratePanel(int[] panel)
	 */

	/**
	 * Evaluate a panel / set of thresholds
	 * @param panel: the panel as in iteratePanel
	 * @param thresholdIdx: the index of the thresholds in the panel
	 */
	/* The actual code is kept in old version 0.3
	 * private static void iteratePanel(int[] panel)
	 */

	/** 
	 * Prints an error message and exits with code i
	 * @param int i: the exit code
	 */
	private void printHelpAndExit(int i) {
		System.err.println("PanelomiX help");
		System.err.println("Finds the best combination of biomarkers");
		System.err.println("Call: java PanelomiX [-h] -c file");
		System.err.println("\t-h: prints this help message");
		System.err.println("\t-d: debug mode. Debug output is printed to the console. Note that if -d is defined after -c, configuration options cannot be debugged.");
		System.err.println("\t-c ConfigFile: the filename of a configuration file.");
		System.err.println("\t-SAH: use a standard dataset (SAH) and sensible options. Should not be used with -c.");
		System.exit(i);
		
	} // printHelpAndExit

	/**
	 * Do the comparison of the panels, once SE and SP have been computed.
	 * Will read and write maxSe and write to the output, so it needs to be synchronized. 
	 * If a better panel is found, prints it to the output.
	 * @param se: the new sensitivity
	 * @param sp: the new specificity
	 * @param panel: the panel that was tested
	 * @param thresholdIdx: the thresholds of the panel that was tested
	 * @param scorePosAt: the score at which the panel that was tested is positive.
	 * @return void
	 */
	protected synchronized void comparePanelSp(float se, float sp, int[] panel, int[] thresholdIdx, int scorePosAt, Thread callerThread) {
		if (sp < this.maxSp) {
			return;
		}
		else if (se > this.maxSe) {
			this.outputStream.printFoundBetter(se, sp);
			this.outputStream.printPanel(panel, thresholdIdx, scorePosAt);
			this.panelList.foundBetter(se, sp, panel.length);
			this.panelList.add(this, panel, thresholdIdx, scorePosAt);
			this.maxSe = se;
			this.maxSp = sp; /* reinitialize maxSp */
			this.bestLength = panel.length;
		}
		else if (se == this.maxSe) {
			if (sp > this.maxSp && panel.length == this.bestLength) { // if we find a panel with same SE but better sp, it is better!
				this.outputStream.printFoundBetter(se, sp);
				this.outputStream.printPanel(panel, thresholdIdx, scorePosAt);
				this.panelList.foundBetter(se, sp, panel.length);
				this.panelList.add(this, panel, thresholdIdx, scorePosAt);
				this.maxSp = sp; /* this is the new maxSp. We don't want a panel with same SE but lower SP anymore  */
				// this.bestLength = panel.length; // commented out: only accept longer panels if SE gets up! 
			}
			else if (sp == this.maxSp && panel.length == this.bestLength) { // with the same SE and SP, print only if not longer.
				this.outputStream.printPanel(panel, thresholdIdx, scorePosAt);
				this.panelList.add(this, panel, thresholdIdx, scorePosAt);
			}
			else if (panel.length < this.bestLength) { // with the same SE and SP, print only if not longer.
				this.outputStream.printFoundBetter(se, sp);
				this.outputStream.printPanel(panel, thresholdIdx, scorePosAt);
				this.panelList.foundBetter(se, sp, panel.length);
				this.panelList.add(this, panel, thresholdIdx, scorePosAt);
				this.maxSp = sp; /* this is the new maxSp. We don't want a panel with same SE but lower SP anymore  */
			}
//			else if (se == 1.0 && sp == 1.0) {
//				System.out.println("100/100 reached in " + callerThread.getName());
//				callerThread.interrupt();
//			}
		}
	} // comparePanelSp
		
	/**
	 * Do the comparison of the panels for high SE, once SE and SP have been computed.
	 * Will read and write maxSp and write to the output, so it needs to be synchronized. 
	 * If a better panel is found, prints it to the output.
	 * @param se: the new sensitivity
	 * @param sp: the new specificity
	 * @param panel: the panel that was tested
	 * @param thresholdIdx: the thresholds of the panel that was tested
	 * @param scorePosAt: the score at which the panel that was tested is positive.
	 * @return void
	 */
	protected synchronized void comparePanelSe(float se, float sp, int[] panel, int[] thresholdIdx, int scorePosAt, Thread callerThread) {
		if (sp > this.maxSp) {
			this.outputStream.printFoundBetter(se, sp);
			this.outputStream.printPanel(panel, thresholdIdx, scorePosAt);
			this.panelList.foundBetter(se, sp, panel.length);
			this.panelList.add(this, panel, thresholdIdx, scorePosAt);
			this.maxSe = se;
			this.maxSp = sp; /* reinitialize maxSp */
			this.bestLength = panel.length;
		}
		else if (se < this.maxSe) {
			return;
		}
		else if (sp == this.maxSp) {
			if (se > this.maxSe && panel.length <= this.bestLength) { // if we find a panel with same SE but better sp, it is better!
				this.outputStream.printFoundBetter(se, sp);
				this.outputStream.printPanel(panel, thresholdIdx, scorePosAt);
				this.panelList.foundBetter(se, sp, panel.length);
				this.panelList.add(this, panel, thresholdIdx, scorePosAt);
				this.maxSe = se; /* this is the new maxSp. We don't want a panel with same SE but lower SP anymore  */
				// this.bestLength = panel.length; // commented out: only accept longer panels if SE gets up! 
			}
			else if (se == this.maxSe && panel.length <= this.bestLength) { // with the same SE and SP, print only if not longer.
				this.panelList.add(this, panel, thresholdIdx, scorePosAt);
			}
		}
	} // comparePanelSe
		
	/**
	 * Do the comparison of the panels for overall accuracy, once SE and SP have been computed.
	 * Will read and write maxSp and write to the output, so it needs to be synchronized. 
	 * If a better panel is found, prints it to the output.
	 * @param se: the new sensitivity
	 * @param sp: the new specificity
	 * @param panel: the panel that was tested
	 * @param thresholdIdx: the thresholds of the panel that was tested
	 * @param scorePosAt: the score at which the panel that was tested is positive.
	 * @return void
	 */
	protected synchronized void comparePanelAcc(float se, float sp, int[] panel, int[] thresholdIdx, int scorePosAt, Thread callerThread) {
		float acc = sp + se;
		if (acc > this.maxAcc) {
			this.outputStream.printFoundBetter(se, sp);
			this.outputStream.printPanel(panel, thresholdIdx, scorePosAt);
			this.panelList.foundBetter(se, sp, panel.length);
			this.panelList.add(this, panel, thresholdIdx, scorePosAt);
			this.maxAcc = acc;/* reinitialize maxAcc */
			this.bestLength = panel.length;
		}
		else if (acc == this.maxSp && panel.length <= this.bestLength) { // if we find a panel with same SE but better sp, it is better!
			this.outputStream.printFoundBetter(se, sp);
			this.outputStream.printPanel(panel, thresholdIdx, scorePosAt);
			this.panelList.foundBetter(se, sp, panel.length);
			this.panelList.add(this, panel, thresholdIdx, scorePosAt);
		}
	} // comparePanelAcc

	/**
	 * Count the number of total iterations that will be performed
	 * @param void
	 * @return long: the number of iterations that should be reached.
	 */
	public long countMaxIterations() {
		MultisizeCombinationGenerator locPanels = new MultisizeCombinationGenerator(predictors.length, panelSizes);
		int[] panel;
		long iterationsCount = 0;
		while ((panel = locPanels.getNext()) != null) {
			int[] panelCloned = ArrayConcat.c(panel, fixedPanel);
			long possibilities = 1;
			for (int i = 0; i < panelCloned.length; i++) {
				possibilities *= thresholds.get(allPredictors[panelCloned[i]]).length ;
			}
			iterationsCount += possibilities;
		}
		return (long) iterationsCount;
	}
	
	/**
	 * Puts a predictor and thresholds to a panel.
	 * @param direction 
	 * @param String predictor: the predictor to add
	 * @param float[] thresholds: the thresholds corresponding to the predictor
	 * @return 
	 * @return boolean: true if added, false if the predictor was already present
	 */
	public boolean putPredictor(String predictor, String direction, float[] newThresholds) {
		if (thresholds.containsKey(predictor))
			return false;
		thresholds.put(predictor, newThresholds);
		directions.put(predictor, direction);
		predictors = ArrayConcat.c(predictors, predictor);
		return true;
	}
	
	/**
	 * Puts a fixed predictor and thresholds to a panel.
	 * @param String predictor: the predictor to add
	 * @param float[] thresholds: the thresholds corresponding to the predictor
	 * @return 
	 * @return boolean: true if added, false if the predictor was already present
	 */
	public boolean putFixedPredictor(String fixedPredictor, String direction, float[] newThresholds) {
		if (thresholds.containsKey(fixedPredictor))
			return false;
		thresholds.put(fixedPredictor, newThresholds);
		directions.put(fixedPredictor, direction);
		fixedPredictors = ArrayConcat.c(fixedPredictors, fixedPredictor);
		return true;
	}
	
	public void setDataFile(String filename) {
		filenameCSV = filename;
	}
	
	public String getDataFile() {
		return filenameCSV;
	}
	
	public void setResponse(String newResponse) {
		response = newResponse;
	}
	
	public String getResponse() {
		return response;
	}
	
	public void setLevels(String[] newLevels) {
		levels = newLevels;
	}
	
	public String[] getLevels() {
		return levels;
	}
	
	public void setPanelSizes(int[] newPanelSizes) {
		panelSizes = newPanelSizes;
	}
	
	public int[] getPanelSizes() {
		return panelSizes;
	}
	
	public double getExecTime() {
		return execTime;
	}
	
	public void setNProc(int newNProc) {
		nProc = newNProc;
	}
	
	public int getNProc() {
		return nProc;
	}
	
	/**
	 * get the current number of iterations tested
	 * @return long
	 */
	public long getCurrentIterations() {
		long iterationsCount = 0;
		Iterator<PanelomiXThreads> threadsIterator = threads.iterator();
		while (threadsIterator.hasNext()) {
			iterationsCount += threadsIterator.next().iterations;
		}
		return iterationsCount;
	}

	public long getIterations() {
		return iterations;
	}
	
	public void setDebugFlag(boolean debug) {
		this.debug = debug;
	}
	
	public boolean getDebugFlag() {
		return debug;
	}
	
	public void setOutputFile(String outputFile) {
		this.filenameOutput = outputFile;
	}
	
	public String getOutputFile() {
		return filenameOutput;
	}
	
	public void setProgressFile(String progressFile) {
		this.filenameProgress = progressFile;
	}
	
	public String getProgressFile() {
		return filenameProgress;
	}
	
	public boolean getProgressBar() {
		return monitor > 0;
	}
	
	public void setProgressBar(boolean progressBar) {
		if (progressBar) {
			setMonitorTime(100);
		}
		else {
			setMonitorTime(0);
		}
	}
	
	public void setMonitorTime(int time) {
		this.monitor = time;
	}
	
	public int getMonitorTime() {
		return monitor;
	}
	
	public void setSilent(boolean silent) {
		this.silent = silent;
	}
	
	public boolean getSilent() {
		return silent;
	}
	
	public void setOptimizeOptions(String focus, float minConstr) {
		optimizeOptions = new PanelOptimizeOptions(focus, minConstr);
	}
	
	/** setOptimizeOptions for R only: must accept a vector for some unknown reason. */
	public void setOptimizeOptions(String focus, float[] minConstr) {
		optimizeOptions = new PanelOptimizeOptions(focus, minConstr[0]);
	}
	
	public void setOptimizeOptions(String focus) {
		optimizeOptions = new PanelOptimizeOptions(focus);
	}
	
	public PanelOptimizeOptions getOptimizeOptions() {
		return optimizeOptions;
	}
	
	public PanelList getPanels() {
		return panelList;
	}
} // class
