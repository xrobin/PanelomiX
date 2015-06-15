package ch.unige.bprg.panelomix.utils;
/**  A bare-bones immutable data type for M-by-N matrices.
 * @author Robert Sedgewick
 * @author Kevin Wayne
 * @copyright Copyright © 2007, Robert Sedgewick and Kevin Wayne.
 * @version Last updated: Tue Sep 29 16:17:41 EDT 2009. 
 * @source http://www.cs.princeton.edu/introcs/95linear/Matrix.java.html
 * @source Introduction to Programming in Java: An Interdisciplinary Approach, ISBN 0321498054
 */


final public class Matrix {
	private final int M;			 // number of rows
	private final int N;			 // number of columns
	private final double[][] data;   // M-by-N array

	/** 
	 * Constructor: create M-by-N matrix of 0's
	 * @param int M: the number of rows in the matrix
	 * @param int N: the number of columns in the matrix
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public Matrix(int M, int N) {
		this.M = M;
		this.N = N;
		data = new double[M][N];
	}

	/**
	 * Constructor: create matrix based on 2d array
	 * @param double[][] data: an 2D array of doubles
	 * @author Robert Sedgewick
	 * @author Kevin Wayne 
	 */
	public Matrix(double[][] data) {
		M = data.length;
		N = data[0].length;
		this.data = new double[M][N];
		for (int i = 0; i < M; i++)
			for (int j = 0; j < N; j++)
					this.data[i][j] = data[i][j];
	}

	/**
	 * Constructor: copy constructor
	 * @param Matrix A: the matrix to copy
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	private Matrix(Matrix A) { this(A.data); }

	/**
	 * Create and return a random M-by-N matrix with values between 0 and 1
	 * @param int M: the number of rows in the matrix
	 * @param int N: the number of columns in the matrix
	 * @return Matrix: the random matrix
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public static Matrix random(int M, int N) {
		Matrix A = new Matrix(M, N);
		for (int i = 0; i < M; i++)
			for (int j = 0; j < N; j++)
				A.data[i][j] = Math.random();
		return A;
	}

	/**
	 * Create and return the N-by-N identity matrix
	 * @param N: the size of the square matrix
	 * @return Matrix: the identity matrix
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public static Matrix identity(int N) {
		Matrix I = new Matrix(N, N);
		for (int i = 0; i < N; i++)
			I.data[i][i] = 1;
		return I;
	}

	/**
	 * Swap rows i and j
	 * @param int i and j: the rows to swap
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	private void swap(int i, int j) {
		double[] temp = data[i];
		data[i] = data[j];
		data[j] = temp;
	}

	/**
	 * Create and return the transpose of the invoking matrix
	 * @return Matrix: the transposed matrix
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public Matrix transpose() {
		Matrix A = new Matrix(N, M);
		for (int i = 0; i < M; i++)
			for (int j = 0; j < N; j++)
				A.data[j][i] = this.data[i][j];
		return A;
	}

	/**
	 * Return C = A + B
	 * @param Matrix B: the matrix to add
	 * @return Matrix: the sum of the matrices
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public Matrix plus(Matrix B) {
		Matrix A = this;
		if (B.M != A.M || B.N != A.N) throw new RuntimeException("Illegal matrix dimensions.");
		Matrix C = new Matrix(M, N);
		for (int i = 0; i < M; i++)
			for (int j = 0; j < N; j++)
				C.data[i][j] = A.data[i][j] + B.data[i][j];
		return C;
	}


	/**
	 * Return C = A - B
	 * @param B: the matrix to substract
	 * @return Matrix: the difference between the matrices
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public Matrix minus(Matrix B) {
		Matrix A = this;
		if (B.M != A.M || B.N != A.N) throw new RuntimeException("Illegal matrix dimensions.");
		Matrix C = new Matrix(M, N);
		for (int i = 0; i < M; i++)
			for (int j = 0; j < N; j++)
				C.data[i][j] = A.data[i][j] - B.data[i][j];
		return C;
	}

	/**
	 * Does A = B exactly?
	 * @param B: the matrix to compare with
	 * @return boolean: comparison between the matrices
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public boolean eq(Matrix B) {
		Matrix A = this;
		if (B.M != A.M || B.N != A.N) throw new RuntimeException("Illegal matrix dimensions.");
		for (int i = 0; i < M; i++)
			for (int j = 0; j < N; j++)
				if (A.data[i][j] != B.data[i][j]) return false;
		return true;
	}

	/**
	 * Return C = A * B
	 * @param B: the matrix to multiply
	 * @return Matrix: the product of the matrices
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public Matrix times(Matrix B) {
		Matrix A = this;
		if (A.N != B.M) throw new RuntimeException("Illegal matrix dimensions.");
		Matrix C = new Matrix(A.M, B.N);
		for (int i = 0; i < C.M; i++)
			for (int j = 0; j < C.N; j++)
				for (int k = 0; k < A.N; k++)
					C.data[i][j] += (A.data[i][k] * B.data[k][j]);
		return C;
	}


	/**
	 * Return x = A^-1 b, assuming A is square and has full rank
	 * @param Matrix rhs: the matrix to solve
	 * @return Matrix: the solved matrix
	 */
	public Matrix solve(Matrix rhs) {
		if (M != N || rhs.M != N || rhs.N != 1)
			throw new RuntimeException("Illegal matrix dimensions.");

		// create copies of the data
		Matrix A = new Matrix(this);
		Matrix b = new Matrix(rhs);

		// Gaussian elimination with partial pivoting
		for (int i = 0; i < N; i++) {

			// find pivot row and swap
			int max = i;
			for (int j = i + 1; j < N; j++)
				if (Math.abs(A.data[j][i]) > Math.abs(A.data[max][i]))
					max = j;
			A.swap(i, max);
			b.swap(i, max);

			// singular
			if (A.data[i][i] == 0.0) throw new RuntimeException("Matrix is singular.");

			// pivot within b
			for (int j = i + 1; j < N; j++)
				b.data[j][0] -= b.data[i][0] * A.data[j][i] / A.data[i][i];

			// pivot within A
			for (int j = i + 1; j < N; j++) {
				double m = A.data[j][i] / A.data[i][i];
				for (int k = i+1; k < N; k++) {
					A.data[j][k] -= A.data[i][k] * m;
				}
				A.data[j][i] = 0.0;
			}
		}

		// back substitution
		Matrix x = new Matrix(N, 1);
		for (int j = N - 1; j >= 0; j--) {
			double t = 0.0;
			for (int k = j + 1; k < N; k++)
				t += A.data[j][k] * x.data[k][0];
			x.data[j][0] = (b.data[j][0] - t) / A.data[j][j];
		}
		return x;
   
	}

	/**
	 * Print matrix to standard output
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public void show() {
		for (int i = 0; i < M; i++) {
			for (int j = 0; j < N; j++) 
				System.out.printf("%9.4f ", data[i][j]);
			System.out.println();
		}
	}



	/**
	 * Test client
	 * @param args
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	public static void main(String[] args) {
		double[][] d = { { 1, 2, 3 }, { 4, 5, 6 }, { 9, 1, 3} };
		Matrix D = new Matrix(d);
		D.show();		
		System.out.println();

		Matrix A = Matrix.random(5, 5);
		A.show(); 
		System.out.println();

		A.swap(1, 2);
		A.show(); 
		System.out.println();

		Matrix B = A.transpose();
		B.show(); 
		System.out.println();

		Matrix C = Matrix.identity(5);
		C.show(); 
		System.out.println();

		A.plus(B).show();
		System.out.println();

		B.times(A).show();
		System.out.println();

		// shouldn't be equal since AB != BA in general	
		System.out.println(A.times(B).eq(B.times(A)));
		System.out.println();

		Matrix b = Matrix.random(5, 1);
		b.show();
		System.out.println();

		Matrix x = A.solve(b);
		x.show();
		System.out.println();

		A.times(x).show();
		
	}
}
