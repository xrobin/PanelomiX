package ch.unige.bprg.panelomix;


import java.io.PrintStream;
import java.util.HashMap;

import ch.unige.bprg.panelomix.utils.*;



public class PanelomiXThreads implements Runnable {
	PanelomiX p;
	private Thread thread;
	boolean debug;
	public long iterations = 0;
	PanelPrinter outputStream;
	PrintStream progressStream;
	final boolean[] responseData;
	private byte[][][] arrayPrecomputed;
	int minTN;
	int minTP;
	boolean focusSp;
	boolean focusSe;
	boolean focusAcc;
	int numPatPos;
	int numPatNeg;
	private int rows;
	private String[] predictors;
	private HashMap<String, float[]> thresholds;
	int i;
	private int[] fixedPanel;

	/**
	 * Class constructor. 
	 * @param PanelomiX p: the configured PanelomiX object
	 */
	public PanelomiXThreads(PanelomiX p) {
		this.p = p; // make the configured PanelomiX object available. Normally only used to track the "maxSe" and the "panels" to search
		/* We need all these object in the following computations. We could use p.… but it seems to be very slightly faster */
		outputStream = p.outputStream;
		progressStream = p.progressStream;
		debug = p.getDebugFlag();
		responseData = p.responseData;
		arrayPrecomputed = p.arrayPrecomputed;
		minTN = p.minTN;
		minTP = p.minTP;
		focusSp = p.getOptimizeOptions().isFocusSp();
		focusSe = p.getOptimizeOptions().isFocusSe();
		numPatPos = p.numPatPos;
		numPatNeg = p.numPatNeg;
		rows = p.dataCSV.getRows();
		predictors = p.allPredictors;
		thresholds = p.thresholds;
		fixedPanel = p.fixedPanel;
		/* Actually define and start the threads */
		thread = new Thread(this);
	}
	/**
	 * Do the actual run. Required by Runnable.
	 * @param none
	 * @return void
	 */
	public void run() {
		int[] panel;
		int[] panelCloned;
		if (p.panels.getTotal() > 0) {
			while ((panel = p.panels.getNext()) != null) {
				//panelCloned = panel.clone(); // The panel we test must be fixed throughout all the following computations, which is not the case if we don't clone and use the reference directly
				panelCloned = ArrayConcat.c(panel, fixedPanel);
				if (debug)
					outputStream.println(new ArrayPrinter(panelCloned).toString());
				iteratePanel(panelCloned, responseData, arrayPrecomputed);
				if (progressStream != null)
					progressStream.println(new ArrayPrinter(panelCloned).toString());
			}
		}
		else { /* This (getTotal() == 0) happens if we have only fixed predictors */
			if (debug)
				outputStream.println(new ArrayPrinter(fixedPanel).toString());
			iteratePanel(fixedPanel, responseData, arrayPrecomputed);
			if (progressStream != null)
				progressStream.println(new ArrayPrinter(fixedPanel).toString());
		}
	}
	/** 
	 * Returns the thread that is used in this instance
	 * @param none
	 * @return Thread
	 */
	public Thread getThread() {
		return thread;
	}
	
	private void copyArrays(byte[] src, byte[] dest) {
		for (int i = 0; i < src.length; i++) {
			dest[i] = src[i];
		}
	}	
	
	/**
	 * Another (probably more efficient) way to iterate on all the thresholds,
	 * that computes the scores during the looping.
	 * Once the score defined by the set of panel/thresholds has been computed,
	 * it launches the panel scoring, which will in turn compare it to the other
	 * panels computed previously.
	 * @param responseData 
	 * @param arrayPrecomputed2 
	 * @param int[] panel: integers of the molecules in the current panel
	 * @return 
	 * @return void
	 * @throws InterruptedException 
	 */
	public void iteratePanel(int[] panel, final boolean[] responseData, byte[][][] arrayPrecomputed2) {
		// thresholdIdx stores the index of the threshold selected from each predictor
		int[] thresholdIdx = new int[panel.length];
		// the variable that will store the scores (first level only)
		byte[] score = new byte[rows];
		// could it be faster if we do not get the thresholds from the hash?
		int[] thisPanelPredictorsLength = new int[panel.length];
		for (int pan = 0; pan < panel.length; pan++) {
			thisPanelPredictorsLength[pan] = thresholds.get(predictors[panel[pan]]).length;
		}
		// also pre-allocating the score arrays could help
		int len = arrayPrecomputed2[0][0].length;
		byte[] score1 = new byte[len];
		byte[] score2 = new byte[len];
		byte[] score3 = new byte[len];
		byte[] score4 = new byte[len];
		byte[] score5 = new byte[len];
		byte[] score6 = new byte[len];
		byte[] score7 = new byte[len];
		byte[] score8 = new byte[len];
		byte[] score9 = new byte[len];
		// Now really start processing
		if (panel.length == 1) {
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				scorePanel(panel, thresholdIdx, score, responseData);
			}
		}
		else if (panel.length == 2) {
			// loop on the first marker
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				// get the score given by the first marker
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				// loop on the second marker
				for (thresholdIdx[1] = 0; thresholdIdx[1] < thisPanelPredictorsLength[1]; thresholdIdx[1]++) {
					// make a copy of the score
					copyArrays(score, score1);
					// compute the score: add the second marker
					for (int row1 = 0; row1 < rows; row1++) {
						score1[row1] += arrayPrecomputed2[panel[1]][thresholdIdx[1]][row1];
					}
					// now the score is computed, so evaluate it.
					scorePanel(panel, thresholdIdx, score1, responseData);
				}
			}
		}
		else if (panel.length == 3) {
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				for (thresholdIdx[1] = 0; thresholdIdx[1] < thisPanelPredictorsLength[1]; thresholdIdx[1]++) {
					copyArrays(score, score1);
					for (int row1 = 0; row1 < rows; row1++) {
						score1[row1] += arrayPrecomputed2[panel[1]][thresholdIdx[1]][row1];
					}
					for (thresholdIdx[2] = 0; thresholdIdx[2] < thisPanelPredictorsLength[2]; thresholdIdx[2]++) {
						copyArrays(score1, score2);
						for (int row2 = 0; row2 < rows; row2++) {
							score2[row2] += arrayPrecomputed2[panel[2]][thresholdIdx[2]][row2];
						}
						scorePanel(panel, thresholdIdx, score2, responseData);
					}
				}
			}
		}
		else if (panel.length == 4) {
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				for (thresholdIdx[1] = 0; thresholdIdx[1] < thisPanelPredictorsLength[1]; thresholdIdx[1]++) {
					copyArrays(score, score1);
					for (int row1 = 0; row1 < rows; row1++) {
						score1[row1] += arrayPrecomputed2[panel[1]][thresholdIdx[1]][row1];
					}
					for (thresholdIdx[2] = 0; thresholdIdx[2] < thisPanelPredictorsLength[2]; thresholdIdx[2]++) {
						copyArrays(score1, score2);
						for (int row2 = 0; row2 < rows; row2++) {
							score2[row2] += arrayPrecomputed2[panel[2]][thresholdIdx[2]][row2];
						}
						for (thresholdIdx[3] = 0; thresholdIdx[3] < thisPanelPredictorsLength[3]; thresholdIdx[3]++) {
							copyArrays(score2, score3);
							for (int row3 = 0; row3 < rows; row3++) {
								score3[row3] += arrayPrecomputed2[panel[3]][thresholdIdx[3]][row3];
							}
							scorePanel(panel, thresholdIdx, score3, responseData);
						}
					}
				}
			}
		}
		else if (panel.length == 5) {
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				for (thresholdIdx[1] = 0; thresholdIdx[1] < thisPanelPredictorsLength[1]; thresholdIdx[1]++) {
					copyArrays(score, score1);
					for (int row1 = 0; row1 < rows; row1++) {
						score1[row1] += arrayPrecomputed2[panel[1]][thresholdIdx[1]][row1];
					}
					for (thresholdIdx[2] = 0; thresholdIdx[2] < thisPanelPredictorsLength[2]; thresholdIdx[2]++) {
						copyArrays(score1, score2);
						for (int row2 = 0; row2 < rows; row2++) {
							score2[row2] += arrayPrecomputed2[panel[2]][thresholdIdx[2]][row2];
						}
						for (thresholdIdx[3] = 0; thresholdIdx[3] < thisPanelPredictorsLength[3]; thresholdIdx[3]++) {
							copyArrays(score2, score3);
							for (int row3 = 0; row3 < rows; row3++) {
								score3[row3] += arrayPrecomputed2[panel[3]][thresholdIdx[3]][row3];
							}
							for (thresholdIdx[4] = 0; thresholdIdx[4] < thisPanelPredictorsLength[4]; thresholdIdx[4]++) {
								copyArrays(score3, score4);
								for (int row4 = 0; row4 < rows; row4++) {
									score4[row4] += arrayPrecomputed2[panel[4]][thresholdIdx[4]][row4];
								}
								scorePanel(panel, thresholdIdx, score4, responseData);
							}
						}
					}
				}
			}
		}
		else if (panel.length == 6) {
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				for (thresholdIdx[1] = 0; thresholdIdx[1] < thisPanelPredictorsLength[1]; thresholdIdx[1]++) {
					copyArrays(score, score1);
					for (int row1 = 0; row1 < rows; row1++) {
						score1[row1] += arrayPrecomputed2[panel[1]][thresholdIdx[1]][row1];
					}
					for (thresholdIdx[2] = 0; thresholdIdx[2] < thisPanelPredictorsLength[2]; thresholdIdx[2]++) {
						copyArrays(score1, score2);
						for (int row2 = 0; row2 < rows; row2++) {
							score2[row2] += arrayPrecomputed2[panel[2]][thresholdIdx[2]][row2];
						}
						for (thresholdIdx[3] = 0; thresholdIdx[3] < thisPanelPredictorsLength[3]; thresholdIdx[3]++) {
							copyArrays(score2, score3);
							for (int row3 = 0; row3 < rows; row3++) {
								score3[row3] += arrayPrecomputed2[panel[3]][thresholdIdx[3]][row3];
							}
							for (thresholdIdx[4] = 0; thresholdIdx[4] < thisPanelPredictorsLength[4]; thresholdIdx[4]++) {
								copyArrays(score3, score4);
								for (int row4 = 0; row4 < rows; row4++) {
									score4[row4] += arrayPrecomputed2[panel[4]][thresholdIdx[4]][row4];
								}
								for (thresholdIdx[5] = 0; thresholdIdx[5] < thisPanelPredictorsLength[5]; thresholdIdx[5]++) {
									copyArrays(score4, score5);
									for (int row5 = 0; row5 < rows; row5++) {
										score5[row5] += arrayPrecomputed2[panel[5]][thresholdIdx[5]][row5];
									}
									scorePanel(panel, thresholdIdx, score5, responseData);
								}
							}
						}
					}
				}
			}
		}
		else if (panel.length == 7) {
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				for (thresholdIdx[1] = 0; thresholdIdx[1] < thisPanelPredictorsLength[1]; thresholdIdx[1]++) {
					copyArrays(score, score1);
					for (int row1 = 0; row1 < rows; row1++) {
						score1[row1] += arrayPrecomputed2[panel[1]][thresholdIdx[1]][row1];
					}
					for (thresholdIdx[2] = 0; thresholdIdx[2] < thisPanelPredictorsLength[2]; thresholdIdx[2]++) {
						copyArrays(score1, score2);
						for (int row2 = 0; row2 < rows; row2++) {
							score2[row2] += arrayPrecomputed2[panel[2]][thresholdIdx[2]][row2];
						}
						for (thresholdIdx[3] = 0; thresholdIdx[3] < thisPanelPredictorsLength[3]; thresholdIdx[3]++) {
							copyArrays(score2, score3);
							for (int row3 = 0; row3 < rows; row3++) {
								score3[row3] += arrayPrecomputed2[panel[3]][thresholdIdx[3]][row3];
							}
							for (thresholdIdx[4] = 0; thresholdIdx[4] < thisPanelPredictorsLength[4]; thresholdIdx[4]++) {
								copyArrays(score3, score4);
								for (int row4 = 0; row4 < rows; row4++) {
									score4[row4] += arrayPrecomputed2[panel[4]][thresholdIdx[4]][row4];
								}
								for (thresholdIdx[5] = 0; thresholdIdx[5] < thisPanelPredictorsLength[5]; thresholdIdx[5]++) {
									copyArrays(score4, score5);
									for (int row5 = 0; row5 < rows; row5++) {
										score5[row5] += arrayPrecomputed2[panel[5]][thresholdIdx[5]][row5];
									}
									for (thresholdIdx[6] = 0; thresholdIdx[6] < thisPanelPredictorsLength[6]; thresholdIdx[6]++) {
										copyArrays(score5, score6);
										for (int row6 = 0; row6 < rows; row6++) {
											score6[row6] += arrayPrecomputed2[panel[6]][thresholdIdx[6]][row6];
										}
										scorePanel(panel, thresholdIdx, score6, responseData);
									}
								}
							}
						}
					}
				}
			}
		}
		else if (panel.length == 8) {
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				for (thresholdIdx[1] = 0; thresholdIdx[1] < thisPanelPredictorsLength[1]; thresholdIdx[1]++) {
					copyArrays(score, score1);
					for (int row1 = 0; row1 < rows; row1++) {
						score1[row1] += arrayPrecomputed2[panel[1]][thresholdIdx[1]][row1];
					}
					for (thresholdIdx[2] = 0; thresholdIdx[2] < thisPanelPredictorsLength[2]; thresholdIdx[2]++) {
						copyArrays(score1, score2);
						for (int row2 = 0; row2 < rows; row2++) {
							score2[row2] += arrayPrecomputed2[panel[2]][thresholdIdx[2]][row2];
						}
						for (thresholdIdx[3] = 0; thresholdIdx[3] < thisPanelPredictorsLength[3]; thresholdIdx[3]++) {
							copyArrays(score2, score3);
							for (int row3 = 0; row3 < rows; row3++) {
								score3[row3] += arrayPrecomputed2[panel[3]][thresholdIdx[3]][row3];
							}
							for (thresholdIdx[4] = 0; thresholdIdx[4] < thisPanelPredictorsLength[4]; thresholdIdx[4]++) {
								copyArrays(score3, score4);
								for (int row4 = 0; row4 < rows; row4++) {
									score4[row4] += arrayPrecomputed2[panel[4]][thresholdIdx[4]][row4];
								}
								for (thresholdIdx[5] = 0; thresholdIdx[5] < thisPanelPredictorsLength[5]; thresholdIdx[5]++) {
									copyArrays(score4, score5);
									for (int row5 = 0; row5 < rows; row5++) {
										score5[row5] += arrayPrecomputed2[panel[5]][thresholdIdx[5]][row5];
									}
									for (thresholdIdx[6] = 0; thresholdIdx[6] < thisPanelPredictorsLength[6]; thresholdIdx[6]++) {
										copyArrays(score5, score6);
										for (int row6 = 0; row6 < rows; row6++) {
											score6[row6] += arrayPrecomputed2[panel[6]][thresholdIdx[6]][row6];
										}
										for (thresholdIdx[7] = 0; thresholdIdx[7] < thisPanelPredictorsLength[7]; thresholdIdx[7]++) {
											copyArrays(score6, score7);
											for (int row7 = 0; row7 < rows; row7++) {
												score7[row7] += arrayPrecomputed2[panel[7]][thresholdIdx[7]][row7];
											}
											scorePanel(panel, thresholdIdx, score7, responseData);
										}
									}
								}
							}
						}
					}
				}
			}
		}
		else if (panel.length == 9) {
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				for (thresholdIdx[1] = 0; thresholdIdx[1] < thisPanelPredictorsLength[1]; thresholdIdx[1]++) {
					copyArrays(score, score1);
					for (int row1 = 0; row1 < rows; row1++) {
						score1[row1] += arrayPrecomputed2[panel[1]][thresholdIdx[1]][row1];
					}
					for (thresholdIdx[2] = 0; thresholdIdx[2] < thisPanelPredictorsLength[2]; thresholdIdx[2]++) {
						copyArrays(score1, score2);
						for (int row2 = 0; row2 < rows; row2++) {
							score2[row2] += arrayPrecomputed2[panel[2]][thresholdIdx[2]][row2];
						}
						for (thresholdIdx[3] = 0; thresholdIdx[3] < thisPanelPredictorsLength[3]; thresholdIdx[3]++) {
							copyArrays(score2, score3);
							for (int row3 = 0; row3 < rows; row3++) {
								score3[row3] += arrayPrecomputed2[panel[3]][thresholdIdx[3]][row3];
							}
							for (thresholdIdx[4] = 0; thresholdIdx[4] < thisPanelPredictorsLength[4]; thresholdIdx[4]++) {
								copyArrays(score3, score4);
								for (int row4 = 0; row4 < rows; row4++) {
									score4[row4] += arrayPrecomputed2[panel[4]][thresholdIdx[4]][row4];
								}
								for (thresholdIdx[5] = 0; thresholdIdx[5] < thisPanelPredictorsLength[5]; thresholdIdx[5]++) {
									copyArrays(score4, score5);
									for (int row5 = 0; row5 < rows; row5++) {
										score5[row5] += arrayPrecomputed2[panel[5]][thresholdIdx[5]][row5];
									}
									for (thresholdIdx[6] = 0; thresholdIdx[6] < thisPanelPredictorsLength[6]; thresholdIdx[6]++) {
										copyArrays(score5, score6);
										for (int row6 = 0; row6 < rows; row6++) {
											score6[row6] += arrayPrecomputed2[panel[6]][thresholdIdx[6]][row6];
										}
										for (thresholdIdx[7] = 0; thresholdIdx[7] < thisPanelPredictorsLength[7]; thresholdIdx[7]++) {
											copyArrays(score6, score7);
											for (int row7 = 0; row7 < rows; row7++) {
												score7[row7] += arrayPrecomputed2[panel[7]][thresholdIdx[7]][row7];
											}
											for (thresholdIdx[8] = 0; thresholdIdx[8] < thisPanelPredictorsLength[8]; thresholdIdx[8]++) {
												copyArrays(score7, score8);
												for (int row8 = 0; row8 < rows; row8++) {
													score8[row8] += arrayPrecomputed2[panel[8]][thresholdIdx[8]][row8];
												}
												scorePanel(panel, thresholdIdx, score8, responseData);
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
		else if (panel.length == 10) {
			for (thresholdIdx[0] = 0; thresholdIdx[0] < thisPanelPredictorsLength[0]; thresholdIdx[0]++) {
				score = arrayPrecomputed2[panel[0]][thresholdIdx[0]];
				for (thresholdIdx[1] = 0; thresholdIdx[1] < thisPanelPredictorsLength[1]; thresholdIdx[1]++) {
					copyArrays(score, score1);
					for (int row1 = 0; row1 < rows; row1++) {
						score1[row1] += arrayPrecomputed2[panel[1]][thresholdIdx[1]][row1];
					}
					for (thresholdIdx[2] = 0; thresholdIdx[2] < thisPanelPredictorsLength[2]; thresholdIdx[2]++) {
						copyArrays(score1, score2);
						for (int row2 = 0; row2 < rows; row2++) {
							score2[row2] += arrayPrecomputed2[panel[2]][thresholdIdx[2]][row2];
						}
						for (thresholdIdx[3] = 0; thresholdIdx[3] < thisPanelPredictorsLength[3]; thresholdIdx[3]++) {
							copyArrays(score2, score3);
							for (int row3 = 0; row3 < rows; row3++) {
								score3[row3] += arrayPrecomputed2[panel[3]][thresholdIdx[3]][row3];
							}
							for (thresholdIdx[4] = 0; thresholdIdx[4] < thisPanelPredictorsLength[4]; thresholdIdx[4]++) {
								copyArrays(score3, score4);
								for (int row4 = 0; row4 < rows; row4++) {
									score4[row4] += arrayPrecomputed2[panel[4]][thresholdIdx[4]][row4];
								}
								for (thresholdIdx[5] = 0; thresholdIdx[5] < thisPanelPredictorsLength[5]; thresholdIdx[5]++) {
									copyArrays(score4, score5);
									for (int row5 = 0; row5 < rows; row5++) {
										score5[row5] += arrayPrecomputed2[panel[5]][thresholdIdx[5]][row5];
									}
									for (thresholdIdx[6] = 0; thresholdIdx[6] < thisPanelPredictorsLength[6]; thresholdIdx[6]++) {
										copyArrays(score5, score6);
										for (int row6 = 0; row6 < rows; row6++) {
											score6[row6] += arrayPrecomputed2[panel[6]][thresholdIdx[6]][row6];
										}
										for (thresholdIdx[7] = 0; thresholdIdx[7] < thisPanelPredictorsLength[7]; thresholdIdx[7]++) {
											copyArrays(score6, score7);
											for (int row7 = 0; row7 < rows; row7++) {
												score7[row7] += arrayPrecomputed2[panel[7]][thresholdIdx[7]][row7];
											}
											for (thresholdIdx[8] = 0; thresholdIdx[8] < thisPanelPredictorsLength[8]; thresholdIdx[8]++) {
												copyArrays(score7, score8);
												for (int row8 = 0; row8 < rows; row8++) {
													score8[row8] += arrayPrecomputed2[panel[8]][thresholdIdx[8]][row8];
												}
												for (thresholdIdx[9] = 0; thresholdIdx[9] < thisPanelPredictorsLength[9]; thresholdIdx[9]++) {
													copyArrays(score8, score9);
													for (int row9 = 0; row9 < rows; row9++) {
														score9[row9] += arrayPrecomputed2[panel[9]][thresholdIdx[9]][row9];
													}
													scorePanel(panel, thresholdIdx, score9, responseData);
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
		else
			throw new Error("Unimplemented panel length. Please edit iteratePanel in class Panelomix to implement a length of " + panel.length);
	} // iteratePanel

	/**
	 * Evaluate a panel / set of thresholds — version for precomputed scores. Will then call the comparison function.
	 * @param panel: the panel as in iteratePanel
	 * @param thresholdIdx: the index of the thresholds in the panel
	 * @param the scores of the patients
	 * @throws InterruptedException
	 */
	public void scorePanel(final int[] panel, final int[] thresholdIdx, final byte[] score, final boolean[] responseData) {
		iterations++; // this is just used to keep track of the number of combinations tested
		int tn, tp;
		float se, sp;
		if (focusSp) {
			for (byte scorePosAt = (byte) panel.length; scorePosAt >= 1; scorePosAt--) {
				tn = 0;
				tp = 0;
				/* compute the number of true negatives */
				for (int k = 0; k < rows; k++) {
					if (! responseData[k] && score[k] < scorePosAt) {
						tn++;
					}
					else if (responseData[k] && score[k] >= scorePosAt) {
						tp++;
					}
				}
				/* Check that we have enough true negatives */
				if (tn < minTN) { // as we go by decreasing scorePosAt, if tn is already < constraint, we're not going to do better with smaller scorePosAt…
					return; // … so return from the function (= break the loop) and go to the next combination directly
				}
				/* Compute the rest of required statistics */
				se = (float) tp/numPatPos; // Float.valueOf on the numerator to avoid returning an integer
				sp = (float) tn/numPatNeg; // or cast with (float) (seems equivalent)
				p.comparePanelSp(se, sp, panel, thresholdIdx, scorePosAt, this.thread);
				/* Compare the SE/SP just obtained with the previously obtained ones and output it to the STDOUT */

			} // for scorePosAt
		} // focusSp == true
		else if (focusSe) {
			for (byte scorePosAt = 1; scorePosAt <= panel.length; scorePosAt++) {
				tn = 0;
				tp = 0;
				/* compute the number of true negatives */
				for (int k = 0; k < rows; k++) {
					if (! responseData[k] && score[k] < scorePosAt) {
						tn++;
					}
					else if (responseData[k] && score[k] >= scorePosAt) {
						tp++;
					}
				}
				/* Check that we have enough true negatives */
				if (tp < minTP) { // as we go by increasing scorePosAt, if tp is already < constraint, we're not going to do better with bigger scorePosAt…
					return; // … so return from the function (= break the loop) and go to the next combination directly
				}
				/* Compute the rest of required statistics */
				se = (float) tp/numPatPos; // Float.valueOf on the numerator to avoid returning an integer
				sp = (float) tn/numPatNeg; // or cast with (float) (seems equivalent)
				p.comparePanelSe(se, sp, panel, thresholdIdx, scorePosAt, this.thread);
				/* Compare the SE/SP just obtained with the previously obtained ones and output it to the STDOUT */
			} // for scorePosAt
		} // focusSe == true
		else {
			for (byte scorePosAt = 1; scorePosAt <= panel.length; scorePosAt++) {
				tn = 0;
				tp = 0;
				/* compute the number of true negatives */
				for (int k = 0; k < rows; k++) {
					if (! responseData[k] && score[k] < scorePosAt) {
						tn++;
					}
					else if (responseData[k] && score[k] >= scorePosAt) {
						tp++;
					}
				}
				/* Compute the rest of required statistics */
				se = (float) tp/numPatPos; // Float.valueOf on the numerator to avoid returning an integer
				sp = (float) tn/numPatNeg; // or cast with (float) (seems equivalent)
				p.comparePanelAcc(se, sp, panel, thresholdIdx, scorePosAt, this.thread);
				/* Compare the SE/SP just obtained with the previously obtained ones and output it to the STDOUT */
			} // for scorePosAt
			
		}
	} // scorePanel
}
