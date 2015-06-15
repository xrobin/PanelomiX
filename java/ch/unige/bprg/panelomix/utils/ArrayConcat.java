package ch.unige.bprg.panelomix.utils;

import java.util.Arrays;

/** 
 * Method to concatenate Arrays
 */
public class ArrayConcat {
	/**  
	 * Concatenate two or more arrays as with R's "c" function
	 * @param first T[] a first array of any type
	 * @param rest T[] a second array of the same type than first
	 * @return T[] the merged array
	 * @author Joachim Sauer 
	 * @see http://stackoverflow.com/questions/80476/how-to-concatenate-two-arrays-in-java
	 */
	public static <T> T[] c(T[] first, T[]... rest) {
		if (rest[0] == null)
			return first;
		int totalLength = first.length;
		for (T[] array : rest) {
			totalLength += array.length;
		}
		T[] result = Arrays.copyOf(first, totalLength);
		int offset = first.length;
		for (T[] array : rest) {
			System.arraycopy(array, 0, result, offset, array.length);
			offset += array.length;
		}
		return result;
	}
	/**  
	 * Concatenate an array with an element as with R's "c" function
	 * @param first T[] a first array of any type
	 * @param rest T an element of the same type than first
	 * @return T[] the merged array
	 */
	public static <T> T[] c(T[] first, T... rest) {
		/* Return first if rest is null */
		if (rest == null)
			return first;
		T[] result;
		/* First may be null, too! */
		if (first == null) {
			result = (T[]) new Object[rest.length];
			for (int i = 0; i < rest.length; i++) {
				result[i] = rest[i];
			}
		}
		else {
			result = Arrays.copyOf(first, first.length + rest.length);
			int offset = first.length;
			for (T newElt : rest) {
				result[offset] = newElt;
				offset += 1;
			}
		}
		return result;
	}
	/** 
	 * Same of int[], so just duplicate the code (which is not a T[] !!!)
	 * @param <T>
	 * @param first
	 * @param rest
	 * @return
	 */
	public static int[] c(int[] first, int[]... rest) {
		if (rest[0] == null)
			return first;
		int totalLength = first.length;
		for (int[] array : rest) {
			totalLength += array.length;
		}
		int[] result = Arrays.copyOf(first, totalLength);
		int offset = first.length;
		for (int[] array : rest) {
			System.arraycopy(array, 0, result, offset, array.length);
			offset += array.length;
		}
		return result;
	}
	/**  
	 * And for String as well!!!!!!!!!!!
	 * @param first String[] a first array of any type
	 * @param rest String an element of the same type than first
	 * @return String[] the merged array
	 */
	public static String[] c(String[] first, String... rest) {
		/* Return first if rest is null */
		if (rest == null)
			return first;
		String[] result;
		/* First may be null, too! */
		if (first == null) {
			result = new String[rest.length];
			for (int i = 0; i < rest.length; i++) {
				result[i] = rest[i];
			}
		}
		else {
			result = Arrays.copyOf(first, first.length + rest.length);
			int offset = first.length;
			for (String newElt : rest) {
				result[offset] = newElt;
				offset += 1;
			}
		}
		return result;
	}
}
