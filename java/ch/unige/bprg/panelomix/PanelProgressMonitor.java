package ch.unige.bprg.panelomix;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

public class PanelProgressMonitor implements Runnable {
	private PanelomiX p;
	private JProgressBar progressBar;
	private Thread thread = Thread.currentThread();
	private boolean done = false;
	private JFrame progressFrame;
	private JFrame doneFrame;
	private int progressBarMax = 5000;

	public PanelProgressMonitor(PanelomiX p) {
		this.p = p;
		thread = new Thread(this);
	}
	
	/**
	 * create a progressbar and show it on a jpanel
	 */
	private void createProgressBar() {
		try {
        progressBar = new JProgressBar(0, progressBarMax);
		} catch (Exception e) {}
        progressBar.setValue(0);
        progressBar.setIndeterminate(true);
        progressBar.setStringPainted(true);

		JPanel panel = new JPanel();
		panel.add(progressBar);

		//Create and set up the window.
		progressFrame = new JFrame("Computing panels");
		progressFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		//Create and set up the content pane.
		progressBar.setOpaque(true); //content panes must be opaque
		progressFrame.setContentPane(progressBar);

		//Display the window.
		progressFrame.pack();
		progressFrame.setSize(250, 70);
		progressFrame.setVisible(true);
	}
	
	private void endProgress() {
		progressFrame.setVisible(false); //close previous panel
	}
	
	public void showDone(boolean exitWhenDone) {
		doneFrame = new JFrame("Computed panels");        
		doneFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		JButton doneButton = new JButton("Done");
        doneButton.setActionCommand("done");
        doneButton.addActionListener(new PanelProgressMonitorDoneAction(doneFrame, exitWhenDone));

        doneFrame.setSize(250, 70);
        doneFrame.setVisible(true);
        doneFrame.setContentPane(doneButton);
	}

	/**
	 * Do the actual monitoring through run. Required by Runnable.
	 * @param none
	 * @return void
	 */
	public void run() {
		createProgressBar();
		thread.setPriority(Thread.MAX_PRIORITY);
		long taskLength = p.countMaxIterations();
		if (taskLength > 0) {
			//System.err.println(taskLength);
			progressBar.setIndeterminate(false);
			while(!done) {
				progressBar.setValue((int) (progressBarMax * p.getCurrentIterations()/taskLength));
				try {
					Thread.sleep(p.getMonitorTime());
				} catch (InterruptedException e) {break;}
			}
			progressBar.setValue((int) (progressBarMax*p.getCurrentIterations()/taskLength));
		}
		else {
			while(!done) {
				try {
					Thread.sleep(p.getMonitorTime());
				} catch (InterruptedException e) {break;}
			}
		}
		endProgress();
	}
	
	/** Hides the window */
    public void hide() {
    	doneFrame.setVisible(false);
    }

	/** 
	 * Returns the thread that is used in this instance
	 * @param none
	 * @return Thread
	 */
	public Thread getThread() {
		return thread;
	}
	
	public void setDone(boolean newDone) {
		done = newDone;
	}
	
	public boolean getDone() {
		return done;
	}

}
