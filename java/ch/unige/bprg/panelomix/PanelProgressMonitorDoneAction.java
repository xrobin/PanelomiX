package ch.unige.bprg.panelomix;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JFrame;

public class PanelProgressMonitorDoneAction implements ActionListener {
	private JFrame frame;
	private boolean exitWhenDone;
	
	PanelProgressMonitorDoneAction(JFrame progressFrame, boolean exitWhenDone) {
		frame = progressFrame;
		this.exitWhenDone = exitWhenDone;
	}

    /**
     * Invoked when the user presses "Done". Hides the 
     */
    public void actionPerformed(ActionEvent evt) {
		frame.setVisible(false);
		if (exitWhenDone)
			System.exit(0);
    }}
