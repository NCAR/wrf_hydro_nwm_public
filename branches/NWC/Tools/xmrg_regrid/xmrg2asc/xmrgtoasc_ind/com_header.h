#ifndef COM_HEADER_H
#define COM_HEADER_H

#include <stdio.h>
#include <string.h> 
#include <math.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h> 
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
/* #include <conio.h> */

/*declare functions for swapping bytes */
void swap4byte_int(int *);
void swaplong_int(long int *);
void swap2byte_array(short *,int);
void swap4byte_array(int *,int);
void swap4byte_array_float(float *,int);

#endif /*#ifndef COM_HEADER_H */
