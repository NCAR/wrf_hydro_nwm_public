/*  
*   This header is used to deal with underscore requirement
*   when calling Fortran  from C under Linux enviornment
*   Modified 4/18/2007 by Seann Reed to add do_route_loc.f
*/

/* Define OSLinux if compiled under Linux environment */

#ifndef LINUX_H
#define LINUX_H

/* VK data ID definition */
#define NDATA_ID 8
   static char *data_id[] = {  
      "SQIN",         /* Discharge             */
      "MAPX",         /* Precipitation         */
      "MAT",          /* Temperature           */ 
      "SASC",         /* Snow cover            */
      "SWE",          /* Snow water equivalent */
      "SNSG",         /* Snow depth            */
      "PSFR",         /* Snow fraction         */
      "RAIM"          /* Rain + Melt           */
    };  
   static char *data_unit[] = {
      "CMS",
      "MM",
      "DEGF",
      "REAL",
      "MM",
      "CM",
      "REAL",
      "MM"
    };  
   static char *data_dim[] = {
      "L3/T",
      "L",
      "TEMP",
      "DLES",
      "L",
      "L",
      "DLES",
      "L"
    };    
   static char *grid_id[] = {
      "xxxx",
      "xmrg",
      "tair",
      "sasc",
      "swet",
      "snsg",
      "psfr",
      "raim"
    }; 
            
/* #ifdef OSLinux */
#ifdef LINX
    #define DDGHC2     ddghc2_
    #define DO_ROUTE   do_route_
    #define DO_ROUTE_LOC  do_route_loc_
    #define DO_ROUTE_LOC_FFG  do_route_loc_ffg_
    #define INIT_RUT1  init_rut1_
    #define INIT_RUT2  init_rut2_
    #define REORDER    reorder_
    #define RD_DECK1   rd_deck1_
    #define RD_DECK2   rd_deck2_
    #define RDWTPDB    rdwtpdb_
    #define DDYCDL     ddycdl_
    #define FLAND1     fland1_
    #define APIC24     apic24_
    #define WRITE_GRID2  write_grid2_
    #define DDRMCLX    ddrmcl_
    #define CKCO19     ckco19_
    #define PACK19     pack19_
    #define HSLOPE     hslope_
    #define HSTREAM    hstream_
    #define SBLLGD     SBLLGD_
    #define icp1       icp1_
/***************************************************
 * Following is added according to Lee
 * Zhengtao 11/5/03
 * *************************************************/
    #define HLRMSFLAND1    hlrmsfland1_
    #define HLRMSAPIC24    hlrmsapic24_
    #define WRITE_GRID3  write_grid3_
/************************************************
 * End os added block
 * Zhengtao 11/5/03
 * *******************************************/


    /* following are for reading original xmrg data */
    void swap4byte_int(int *);
    void swaplong_int(long int *);
    void swap2byte_array(short *,int);
    void swap4byte_array(int *,int);
    void swap4byte_array_float(float *,int);
#else
    #define DDGHC2     ddghc2
    #define DO_ROUTE   do_route
    #define DO_ROUTE_LOC   do_route_loc
    #define DO_ROUTE_LOC_FFG   do_route_loc_ffg_
    #define INIT_RUT1  init_rut1
    #define INIT_RUT2  init_rut2
    #define REORDER    reorder
    #define RD_DECK1   rd_deck1
    #define RD_DECK2   rd_deck2
    #define RDWTPDB    rdwtpdb
    #define DDYCDL     ddycdl
    #define FLAND1     fland1
    #define APIC24     apic24
    #define WRITE_GRID2  write_grid2
    #define DDRMCLX    ddrmcl
    #define CKCO19     ckco19
    #define PACK19     pack19
    #define SBLLGD     sbllgd

/***************************************************
 * Following is added according to Lee
 * Zhengtao 11/5/03
 * *************************************************/
    #define HLRMSFLAND1    hlrmsfland1
    #define HLRMSAPIC24    hlrmsapic24
    #define WRITE_GRID3  write_grid3
/************************************************
 * End os added block
 * Zhengtao 11/5/03
 * *******************************************/

#endif

#endif /*ifndef LINUX_H */
