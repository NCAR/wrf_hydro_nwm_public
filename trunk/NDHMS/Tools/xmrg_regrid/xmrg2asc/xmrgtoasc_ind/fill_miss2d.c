/*******  Fill the missing data by averaging 8 its neighbor values       ***
***       The averaging procedure is repited NLOOP times. If still there ***
***       are missed values, another checking will be in 1-D array for   ***
***       selected basins.                                               ***/
/**  VK  program uses parameter 'neg_val' to allow negative values       ***
***      but not less than 'neg_val'. It means that an index of missed   *** 
***      data of grid file should be less than 'neg_val'                 ***/ 

#include "com_header.h"

void fill_miss2d(int ncol, int nrow, float **par_tmp, int neg_val) 
{

  int i, j, ii, jj, nloopx=0;
  int is, iend, js, jend, ns;
  float s;
/* nloop is a constant that defines a level of interpolation to fill
   missed data. If nloop iterations do not fill all gaps, program will stop */
  int nloop=2;  

  int nmiss=1;
  float **temp_array;
  
  temp_array = (float**) malloc(sizeof(float*)*nrow);
  for(i=0; i<nrow; i++) temp_array[i] = (float*) malloc(sizeof(float)*ncol);
  
   while(nmiss != 0 && nloopx < nloop) {
    for(i=0; i<nrow; i++)
     for(j=0; j<ncol; j++) temp_array[i][j] = par_tmp[i][j];
    nmiss=0;
    nloopx=nloopx+1;
    for(i=0; i<nrow; i++) {
     for(j=0; j<ncol; j++) {
      if(par_tmp[i][j] < neg_val) {
       s=0;
       ns=0;
       is=i-1;
       iend=i+2;
       js=j-1;
       jend=j+2;
     
     /* define neighbour points on edges */
       if(i == 0) is=0;
       if(i == nrow-1) iend=i;
       if(j == 0) js=0;
       if(j == ncol-1) jend=j;
       
       for(ii=is; ii<iend; ii++) {
        for(jj=js; jj<jend; jj++) {
         if(temp_array[ii][jj] >= neg_val) {
          s=s+temp_array[ii][jj];
          ns=ns+1;
         } /* end if */
        }  /* end jj */ 
       }   /* end ii */
       if(ns == 0) 
        nmiss=nmiss+1;
       else
        par_tmp[i][j]=s/ns;
      } /* end if */
     }  /* end j  */ 
    }   /* end i  */
   }    /* end while */

   for(i=0; i<nrow; i++) free(temp_array[i]);
   free(temp_array);
    
 }  /* end of fill_miss2d()  */
