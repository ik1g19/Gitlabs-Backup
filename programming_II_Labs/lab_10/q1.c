/*
 * q1.c
 *
 *  Created on: 22 May 2020
 *      Author: isaac
 */


#include <stdio.h>
#include <math.h>


/*
 * declaration of functions
 */
int CACHE(double *prod, double *sum, double d1, double d2);


/*
 * returns 1 if two given doubles have the same sign
 * otherwise returns -1
 */
int CACHE(double *prod, double *sum, double d1, double d2) {
	double d1Round = floor(d1);
	double d2Round = floor(d2);

	*prod = d1Round * d2Round;
	*sum = d1Round + d2Round;

	if (d1Round > 0.0 && d2Round > 0.0)
		return 1;
	else if (d1Round < 0.0 && d2Round < 0.0)
		return 1;
	else if (d1Round == 0.0 && d2Round == 0.0)
		return 1;
	else
		return -1;
}
