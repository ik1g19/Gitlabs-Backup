/*
 * q2.c
 *
 *  Created on: 23 May 2020
 *      Author: isaac
 */


#include <stdio.h>


/*
 * typedef new types
 */
typedef int* NEWTYPE;


/*
 * declaration of functions
 */
NEWTYPE ARRAY(int n);

int STORE(NEWTYPE as, int N, int row, int col, int val);

int FETCH(NEWTYPE as, int N, int row, int col);


/*
 * creates new array of size n*n
 * mallocs memory
 */
NEWTYPE ARRAY(int n) {
	NEWTYPE ptr;

	ptr = malloc(n * n * sizeof(int));;

	return ptr;
}


/*
 * stores value at specified row/column
 * returns -1 if invalid position
 * returns 1 if successfully stored
 */
int STORE(NEWTYPE as, int N, int row, int col, int val) {
	if ((row % 2) != (col % 2)) return -1; // returns -1 if row and column have different parity

	if (row > N || col > N || row < 0 || col < 0) return -1; // returns -1 if row or column is outside of array

	NEWTYPE start = as;

	as += row;
	as += (col * N);

	*as = val;

	as = start;

	return 1;
}


/*
 * fetches value from specified location
 * returns -1 if invalid position
 * returns stored value if successful
 */
int FETCH(NEWTYPE as, int N, int row, int col) {
	if ((row % 2) != (col % 2)) return -1; // returns -1 if row and column have different parity

	if (row > N || col > N || row < 0 || col < 0) return -1; // returns -1 if row or column is outside of array

	NEWTYPE start = as;

	as += row;
	as += (col * N);

	int val = *as;

	as = start;

	return val;
}
