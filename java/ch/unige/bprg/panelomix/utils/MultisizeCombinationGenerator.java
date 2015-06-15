package ch.unige.bprg.panelomix.utils;

/**
 * Generate combination of multiple sizes
 * @author Xavier Robin
 */
public class MultisizeCombinationGenerator {
	private CombinationGenerator[] combinationGenerators;
	private int r = 1;
	private int[] Rs;
	private long numLeft;
	private long total;

	/** 
	 * Class constructor of MultisizeCombinationGenerator.
	 * @param int n: number of elements
	 */
	public MultisizeCombinationGenerator (int n) {
		Initialize(n, arrayOneTo(n));
	}
	/** 
	 * Class constructor of MultisizeCombinationGenerator.
	 * @param int n: number of elements
	 */
	public MultisizeCombinationGenerator (int n, int maxR) {
		Initialize(n, arrayOneTo(maxR));
	}
	/** 
	 * Class constructor of MultisizeCombinationGenerator.
	 * @param int n: number of elements
	 */
	public MultisizeCombinationGenerator (int n, int[] Rs) {
		Initialize(n, Rs);
	}
	
	/**
	 * returns the array 1:n
	 * @param int n: number to climb to
	 * @return int[]: the array 1:n
	 */
	private int[] arrayOneTo(int n) {
		int[] a = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = i+1;
		}
		return a;
	}
	/**
	 * Initializes the combination iterator
	 * @param n
	 * @param Rs
	 */
	private void Initialize (int n, int[] Rs) {
		this.Rs = Rs;
		long total = 0;
		combinationGenerators = new CombinationGenerator[Rs.length];
		if (n > 0) {
			for (int i = 0; i < Rs.length; i++) {
				combinationGenerators[i] = new CombinationGenerator (n, Rs[i]);
				// compute the totals
				total = total + combinationGenerators[i].getTotal();
			}
		}
		this.total = total;
		reset();
	}

	/** Reset the combinations. Needed to restart the iteration.
	 * @param none
	 * @return void */
	public void reset () {
		if (total > 0) {
			for (int i = 0; i < this.Rs.length; i++) {
				combinationGenerators[i].reset();
			}
		}
		numLeft = total;
		r = 0;
	}

	/** Return the total number of combinations not yet generated.
	 * @param none 
	 * @return long */
	public long getNumLeft () {
		return numLeft;
	}

	/** Return the current R.
	 * @param none 
	 * @return int */
	public int getR () {
		return Rs[r];
	}

	/** Are there more combinations to be generated?
	 * @param none 
	 * @return boolean */
	public boolean hasMore () {
		return numLeft > 0;
	}

	/** Total number of combinations that can be generated
	 * @param none 
	 * @return long */
	public long getTotal () {
		return total;
	}

	/** Get the next combination in the iterator
	 * @param none
	 * @return int[] */
	public synchronized int[] getNext () {
		if (! this.hasMore())
			  return null;
		numLeft--;
		if (combinationGenerators[r].hasMore()) {
			return combinationGenerators[r].getNext();
		}
		else {
			r++;
			return combinationGenerators[r].getNext();
		}
	}
	/**
	 * Tests the class
	 * @param args
	 * @throws IllegalStateException in case of unexpected results
	 */
	public static void main(String[] args) {
		/* Single argument construct */
		MultisizeCombinationGenerator mcg = new MultisizeCombinationGenerator(3);
		String[] expectedCombinations = new String[] {"0", "1", "2", "0, 1", "0, 2", "1, 2", "0, 1, 2"};
		// Test if the total is good
		if (mcg.getTotal() != expectedCombinations.length) {
			throw new IllegalStateException("Unexpected number of combinations");
		}
		// Test if the iteration works correctly
		int i = 0;
		while (mcg.hasMore()) {
			// do we get the correct number of combinations left?
			if (mcg.numLeft + i != mcg.getTotal()) {
				throw new IllegalStateException("Unexpected number of combinations left");				
			}
			// do we get the expected combination?
			int[] currentCombination = mcg.getNext();
			if (! new ArrayPrinter(currentCombination).toString().equals(expectedCombinations[i]))
				throw new IllegalStateException("Unexpected combination");
			// do we get the correct size of R?
			if (!((expectedCombinations[i].length() == 1 && mcg.getR() == 1)||(expectedCombinations[i].length() == 4 && mcg.getR() == 2)||(expectedCombinations[i].length() == 7 && mcg.getR() == 3))) {
				throw new IllegalStateException("Unexpected combination length");
			}
			i++;
		}
		/* int, int construct */
		mcg = new MultisizeCombinationGenerator(4, 3);
		expectedCombinations = new String[] {"0", "1", "2", "3", "0, 1", "0, 2", "0, 3", "1, 2", "1, 3", "2, 3", "0, 1, 2", "0, 1, 3", "0, 2, 3", "1, 2, 3"};
		// Test if the total is good
		if (mcg.getTotal() != expectedCombinations.length) {
			throw new IllegalStateException("Unexpected number of combinations in multiple arguments construct");
		}
		// Test if the iteration works correctly
		i = 0;
		while (mcg.hasMore()) {
			// do we get the correct number of combinations left?
			if (mcg.numLeft + i != mcg.getTotal()) {
				throw new IllegalStateException("Unexpected number of combinations left in multiple arguments construct");				
			}
			// do we get the expected combination?
			int[] currentCombination = mcg.getNext();
			if (! new ArrayPrinter(currentCombination).toString().equals(expectedCombinations[i]))
				throw new IllegalStateException("Unexpected combination in multiple arguments construct");
			// do we get the correct size of R?
			if (!((expectedCombinations[i].length() == 1 && mcg.getR() == 1)||(expectedCombinations[i].length() == 4 && mcg.getR() == 2)||(expectedCombinations[i].length() == 7 && mcg.getR() == 3))) {
				throw new IllegalStateException("Unexpected combination length in multiple arguments construct");
			}
			
			i++;
		}
		/* int, int[] construct */
		mcg = new MultisizeCombinationGenerator(4, new int[] {2, 3});
		expectedCombinations = new String[] {"0, 1", "0, 2", "0, 3", "1, 2", "1, 3", "2, 3", "0, 1, 2", "0, 1, 3", "0, 2, 3", "1, 2, 3"};
		// Test if the total is good
		if (mcg.getTotal() != expectedCombinations.length) {
			throw new IllegalStateException("Unexpected number of combinations in multiple arguments construct");
		}
		// Test if the iteration works correctly
		i = 0;
		while (mcg.hasMore()) {
			// do we get the correct number of combinations left?
			if (mcg.numLeft + i != mcg.getTotal()) {
				throw new IllegalStateException("Unexpected number of combinations left in multiple arguments construct");				
			}
			// do we get the expected combination?
			int[] currentCombination = mcg.getNext();
			if (! new ArrayPrinter(currentCombination).toString().equals(expectedCombinations[i]))
				throw new IllegalStateException("Unexpected combination in multiple arguments construct");
			// do we get the correct size of R?
			if (!((expectedCombinations[i].length() == 1 && mcg.getR() == 1)||(expectedCombinations[i].length() == 4 && mcg.getR() == 2)||(expectedCombinations[i].length() == 7 && mcg.getR() == 3))) {
				throw new IllegalStateException("Unexpected combination length in multiple arguments construct");
			}
			
			i++;
		}
		System.out.println("MultisizeCombinationGenerator looks good");
	}
}
