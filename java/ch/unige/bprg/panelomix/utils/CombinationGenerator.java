package ch.unige.bprg.panelomix.utils;

import java.math.BigInteger;

/** 
 * Systematically generate combinations.
 * @author Michael Gilleland
 * @author Xavier Robin (minor modifications to the code)
 * @source http://www.merriampark.com/comb.htm#Source
*/
public class CombinationGenerator {

  private int[] a;
  private final int n;
  private final int r;
  private long numLeft;
  private final long total;

  /** Systematically generate combinations
   * @param int n: the number n of parameters to combine
   * @param int r: the number r of parameters in each returned combination
   * @throws IllegalArgumentException
   */
  public CombinationGenerator (int n, int r) {
    if (r > n) {
      throw new IllegalArgumentException ();
    }
    if (n < 1) {
      throw new IllegalArgumentException ();
    }
    this.n = n;
    this.r = r;
    a = new int[r];
    /* For factorials, work with big integers */
    BigInteger nFact = getFactorial (n);
    BigInteger rFact = getFactorial (r);
    BigInteger nminusrFact = getFactorial (n - r);
    total = nFact.divide(rFact.multiply(nminusrFact)).longValue();
    //total = nFact / (rFact*nminusrFact);
    reset ();
  }

  /** Reset the combinations. Needed to restart the iteration.
   * @param none
   * @return void */
  public void reset () {
    for (int i = 0; i < a.length; i++) {
      a[i] = i;
    }
    numLeft = total;
  }

  /** Return number of combinations not yet generated.
   * @param none 
   * @return long */
  public long getNumLeft () {
    return numLeft;
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


  /** Compute the factorial of n
   * @param int n: the factorial to compute
   * @return long */
private static BigInteger getFactorial (int n) {
	BigInteger fact = BigInteger.ONE;
	for (BigInteger i = BigInteger.valueOf(n); i.compareTo(BigInteger.ONE) == 1; i = i.subtract(BigInteger.ONE)) {
		fact = fact.multiply(i);
    }
	return fact;
}

  /** Get the next combination in the iterator
   * @param none
   * @return int[] */
  public int[] getNext () {
    if (numLeft == total) {
      numLeft--;
      return a;
    }

    int i = r - 1;
    while (a[i] == n - r + i) {
      i--;
    }
    a[i] = a[i] + 1;
    for (int j = i + 1; j < r; j++) {
      a[j] = a[i] + j - i;
    }

    numLeft--;
    return a;
  }
}
