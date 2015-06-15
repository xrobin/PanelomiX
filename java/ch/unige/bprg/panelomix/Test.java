package ch.unige.bprg.panelomix;

import java.io.IOException;


public class Test {
	
	/**
	 * @param args
	 * @throws IOException 
	 * @throws InterruptedException 
	 */
	public static void main(String[] args) throws InterruptedException, IOException {
		// TODO Auto-generated method stub
		PanelomiX PanelomiX = new PanelomiX();
		
		PanelomiX.setDataFile("/var/www/PanelomiX/users/xavier/150/panel/data.csv");
		PanelomiX.setResponse("outcome");
		String[] levels = new String[2];
		levels[0] = "Good";
		levels[1] = "Poor";
		PanelomiX.setLevels(levels);
		
		String[] predictors = {"hfabp",    "ndka",     "s100",     "troponin", "wfns"} ; 
		String[] directions = {"<",    "<",     "<",     "<", "<"} ; 
		float[][] possibleThresholds = {
				{0.610F,  1.105F,  1.240F,  1.330F,  1.405F,  1.535F,  1.620F,  1.800F,  1.870F,  2.205F,
					2.650F,  2.965F,  3.140F,  3.570F,  4.510F,  5.880F, 8.210F, 16.020F, 20.695F, 47.000F},
				{3.440F,   5.105F,   7.240F,   8.160F,   8.720F,   9.520F,   9.840F,  10.365F,  11.080F,
					11.700F,  12.095F,  12.730F,  12.850F, 13.160F,  13.505F,  13.955F,  15.055F,  15.715F,
					17.350F,  20.985F,  22.530F,  30.430F,  47.220F,  52.380F,  65.700F, 249.745F},
				{0.065F, 0.075F, 0.085F, 0.095F, 0.105F, 0.115F, 0.135F, 0.155F, 0.205F, 0.245F, 0.290F, 0.325F,
						0.345F, 0.395F, 0.435F, 0.475F, 0.485F, 0.510F},
				{0.035F, 0.045F, 0.055F, 0.080F, 0.095F, 0.135F, 0.315F, 0.555F, 0.775F, 1.385F, 1.560F, 2.325F,
					3.440F, 4.625F},
				{1.5F, 2.5F, 3.5F, 4.5F}
		};
		
		for (int i = 0; i < predictors.length; i++) {
			PanelomiX.putPredictor(predictors[i], directions[i], possibleThresholds[i]);
		}
		PanelomiX.setOptimizeOptions("accuracy");

		PanelomiX.setPanelSizes(new int[] {1, 2, 3, 4, 5});
		
		PanelomiX.setNProc(2);

		PanelomiX.setProgressBar(false);
		PanelomiX.setProgressFile("/var/www/PanelomiX/users/xavier/150/panel/progress");

		PanelList pl = PanelomiX.run();
		System.out.println(pl);
	}
	
}
