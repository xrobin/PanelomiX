package ch.unige.bprg.panelomix;

public class OSCall {
    public static String dir;

    public static String NulDir() {
        if (System.getProperty("os.name").startsWith("Linux")) {
            dir = "/dev/nul";
        } else {
            dir = "nul";
        }
        return dir;
    }
}
