#include <map>
#include <iostream>

using namespace std;

#define INDX(row, col, ld) (row + col*ld)

void _getLocalIndx_C(int * gLinkId, int glinksl, int *llinkid, int * llinkidindx, int llinklen)
{
  map<int,int> hash;
  map<int,int>::iterator it;

  for(int i=0; i < llinklen; i++)
    hash.insert(std::pair<int,int>(llinkid[i],i));

  for(int i=0; i < glinksl; i++)
    {
      it = hash.find(gLinkId[i]);
      if(it != hash.end())
	{
	  llinkidindx[it->second] = i+1;
	  hash.erase(it);
	}
    }

}

void _LandRT_C(int *ch_lnkrt, int *ch_lnkrt_sl, int i_start, int i_end, int j_start, int j_end, int *LLINKID, int LNLINKSL, int leading_dim)
{
  map<int,int> hash;
  map<int,int>::iterator it;

  for(int i=0; i < LNLINKSL; i++)
    hash.insert(std::pair<int,int>(LLINKID[i],i));
  
  for(int i=i_start-1; i < i_end-1; i++)
    {
      for(int j=j_start-1; j < j_end-1; j++)
	{
	  it = hash.find(ch_lnkrt[INDX(i, j, leading_dim)]);
	  if(it != hash.end())
	    {
	      ch_lnkrt_sl[INDX(i, j, leading_dim)] = it->second+1;
	      //hash.erase(it);
	    }
	}
    }
}

void _readBucket_nhd_C(int *tmpLinkid, int gnid, int *linkid, int numbasns,
		       int *nhdBuckMask, float *gw_buck_coeff, float *gw_buck_exp,
		       float *gw_buck_loss, float *tmpCoeff, float *tmpExp, float *tmpLoss,
		       float *z_max, float *z_init, float *tmpz_max, float *tmpz_init,
		       int bucket_loss)
{
  map<int,int> hash;
  map<int,int>::iterator it;

  // for(int i=0; i < gnid; i++)
  //   hash.insert(std::pair<int,int>(tmpLinkid[i],i));

  // for(int i=0; i < numbasns; i++)
  //   {
  //     if(nhdBuckMask[i] != -999)
  // 	{
  // 	  it = hash.find(linkid[i]);
  // 	  if(it != hash.end())
  // 	    {
  // 	      gw_buck_coeff[i] = tmpCoeff[it->second];
  // 	      gw_buck_exp[i] = tmpExp[it->second];
  // 	      if(bucket_loss == 1)
  // 		gw_buck_loss[i] = tmpLoss[it->second];
  // 	      z_max[i] = tmpz_max[it->second];
  // 	      z_init[i] = tmpz_init[it->second];
  // 	      nhdBuckMask[i] = 1;
  // 	    }
  // 	}
  //   }

  for(int i=0; i < numbasns; i++)
    hash.insert(std::pair<int,int>(linkid[i],i));

  for(int i=0; i < gnid; i++)
    {
      it = hash.find(tmpLinkid[i]);
      if(it != hash.end() && nhdBuckMask[it->second] != -999)
  	{
  	  gw_buck_coeff[it->second] = tmpCoeff[i];
  	  gw_buck_exp[it->second] = tmpExp[i];
  	  if(bucket_loss == 1)
  	    gw_buck_loss[it->second] = tmpLoss[i];
  	  z_max[it->second] = tmpz_max[i];
  	  z_init[it->second] = tmpz_init[i];
  	  nhdBuckMask[it->second] = 1;
  	}
    }
}

extern "C"
{
  void getLocalIndx_C(int * gLinkId, int glinksl, int *llinkid, int * llinkidindx, int llinklen)
  {
    _getLocalIndx_C(gLinkId, glinksl, llinkid, llinkidindx, llinklen);
  }

  void LandRT_C(int *ch_lnkrt, int *ch_lnkrt_sl, int i_start, int i_end, int j_start, int j_end, int *LLINKID, int LNLINKSL, int leading_dim)
  {
    _LandRT_C(ch_lnkrt, ch_lnkrt_sl, i_start, i_end, j_start, j_end, LLINKID, LNLINKSL, leading_dim);
  }

  void readBucket_nhd_C(int *tmpLinkid, int gnid, int *linkid, int numbasns,
			int *nhdBuckMask, float *gw_buck_coeff, float *gw_buck_exp,
			float *gw_buck_loss, float *tmpCoeff, float *tmpExp, float *tmpLoss,
			float *z_max, float *z_init, float *tmpz_max, float *tmpz_init,
			int bucket_loss)
  {
    _readBucket_nhd_C(tmpLinkid, gnid, linkid, numbasns,
		      nhdBuckMask, gw_buck_coeff, gw_buck_exp,
		      gw_buck_loss, tmpCoeff, tmpExp, tmpLoss,
		      z_max, z_init, tmpz_max, tmpz_init,
		      bucket_loss);
  }


}
