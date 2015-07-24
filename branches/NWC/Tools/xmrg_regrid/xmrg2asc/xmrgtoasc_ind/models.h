#ifndef MODELS_H
#define MODELS_H

#include <time.h>
void do_sac(int npix, float *runf_par, float *runf_st,
            int nfpt, float *pcp, float *ped, int dtm, 
	    float *surf, float *subf, float *tet,
	    float *frz_par,float *frz_st, float *sacst_prv, 
	    float *smc, float *sh2o,
	    float dtfrz,
	    //int ivers, 
	    int isFrz,
	    int isSnow,
	    float *aesc, float *ta, 
	    float *we,float *sh,
	    int nprunf, int nsrunf, int npsac_def, int *nsoil, 
	    int *nupl, int *nsac,
	    int nssac_def, float *frzd_up, float *frzd_bt, float *frost,
	    float* tsint, float* swint, float* swhint,
            float* DSINT, int NINT, float* DSINTW, int NINTW, int NORMALIZE,
	    int* error );

void do_sac_ffg(int *ffg_done, int npix, float *runf_par, float *runf_st,
            int nfpt, float *pcp, float *ped, int dtm, 
	    float *surf, float *subf, float *tet,
	    float *frz_par,float *frz_st, float *sacst_prv, 
	    float *smc, float *sh2o,
	    float dtfrz,
	    //int ivers, 
	    int isFrz,
	    int isSnow,
	    float *aesc, float *ta, 
	    float *we,float *sh,
	    int nprunf, int nsrunf, int npsac_def, int *nsoil, 
	    int *nupl, int *nsac,
	    int nssac_def, float *frzd_up, float *frzd_bt, float *frost,
	    float* tsint, float* swint, float* swhint,
            float* DSINT, int NINT, float* DSINTW, int NINTW );

void do_api(int npix, int *listfpt, int *num_fpt, float *runf_par,
            float *runf_st, int nfpt,
	    float *pcp, float *ped, int dtm, long int tloop,
	    long int tstart, long int tend,
	    float *surf, float *subf, float *tet, int KHR,
	    int ITP, int IVOPT, int IFRZE, int ITTA, int IDA, int IMO );

void get_ped(int y, int m, int d, int npix, int dtm,
             float *pe, float *pe_adj, float *ped);

void get_daily_pe(int yr, int mon, int day, int npix,
                  float *pe, float *pe_adj, float *pe_day);

void do_snow17(int npix, long int tloop, long int tstart, float *dthr,
        float *snow_par, float *snow_st, int* irdco, int nexlag,
        float *pcp, float *tair, float *tair_prev, float *psfrac,
        float *rmlt, float *twe, float *aesc, float *snsg,
        float *snof, int neg_tmp, int sw);

void fill_miss2d(int ncol, int nrow, float **par_tmp, int neg_val);

int days_of_month(int m, int y);
int is_leap_year(int yr);
void decodedate_(char DateStr[],int *DateLen, float *today, float *hour);

void HSLOPE( float*, float*, float*, float*, float*, int*, float*, float*,
             int*, char*, int );
void HSTREAM( float*, float*, float*, float*, float*, float*, float*, float*,
		int*, float*, float*, float*, int*, float*, 
		float*, float*, float*, int*, char*, int );

void WRITE_GRID2( int*, int*, int*, int*, char*, 
		  float*, int*, int*, int*, int*, int*, float*, float* );

void ddgch2_( long*, int*, int*, int*, int* );

void calc_frzcnst(int npix, float *sac_parx, float *frz_parx,
		            int *nsoil, int *nupl, int *nsac, float* smax);

int gen_frzst(int npix, int *nsoil, int *nupl, float *sac_parx,
               float *frz_parx, float *frz_stx, float *sac_stx,
               float *sacst_prv, float *smc, float *sh2o, float dthr,
                float* dtfrz, float* frzd_up, float* frzd_bt, int ivers,
               char* errmsg  );

void do_route_( float*, float*, float*, float*, float*, int*, float*, int*,
		float*, int*, float*, float*, int*, int*, int*, float* );

void do_route_loc_( float*, float*, float*, float*, float*, int*, float*, int*,
		float*, float*, float*, float*, int*);

void do_route_loc_ffg_( int*, float*, float*, float*, float*, float*, int*, float*, int*,
		float*, float*, float*);

#endif /*#ifndef MODELS_H*/
