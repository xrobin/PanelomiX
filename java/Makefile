SOURCES = \
	./ch/unige/bprg/panelomix/Threshold.java \
	./ch/unige/bprg/panelomix/PanelNotRunException.java \
	./ch/unige/bprg/panelomix/IncompleteConfigurationException.java \
	./ch/unige/bprg/panelomix/IncomparablePanelsException.java \
	./ch/unige/bprg/panelomix/AlreadyConfiguredException.java \
	./ch/unige/bprg/panelomix/Panel.java \
	./ch/unige/bprg/panelomix/PanelOptimizeOptions.java \
	./ch/unige/bprg/panelomix/PanelPrinter.java \
	./ch/unige/bprg/panelomix/PanelomiXThreads.java \
	./ch/unige/bprg/panelomix/PanelProgressMonitorDoneAction.java \
	./ch/unige/bprg/panelomix/Test.java \
	./ch/unige/bprg/panelomix/utils/PanelCSVReader.java \
	./ch/unige/bprg/panelomix/utils/MultisizeCombinationGenerator.java \
	./ch/unige/bprg/panelomix/utils/ArrayConcat.java \
	./ch/unige/bprg/panelomix/utils/Matrix.java \
	./ch/unige/bprg/panelomix/utils/ArrayPrinter.java \
	./ch/unige/bprg/panelomix/utils/TwoPrintStreams.java \
	./ch/unige/bprg/panelomix/utils/CombinationGenerator.java \
	./ch/unige/bprg/panelomix/InvalidConfigurationException.java \
	./ch/unige/bprg/panelomix/PanelList.java \
	./ch/unige/bprg/panelomix/PanelProgressMonitor.java \
	./ch/unige/bprg/panelomix/PanelomiX.java \
	./ch/unige/bprg/panelomix/PanelComparator.java
OBJECTS = $(SOURCES:.java=.class)
JARS = PanelomiX.jar

JAVAC = javac
JAR = jar

.SUFFIXES: .class .java

.java.class:
		$(JAVAC) -target 1.6 -source 1.6 $<

all: compile install clean

$(JARS): $(OBJECTS)
		$(JAR) cvf $@ $(OBJECTS) 

compile: $(JARS)

install:
		@cp $(JARS) ../inst/java

clean:
		-@rm -f $(OBJECTS) $(JARS)
