#include <map>
#include <iostream>
#include <algorithm>

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

  for(int i=0; i < numbasns; i++)
    hash.insert(std::pair<int,int>(linkid[i],i));

  for(int i=0; i < gnid; i++)
    {
      it = hash.find(tmpLinkid[i]);
      if(it != hash.end() && nhdBuckMask[it->second] == -999)
	{
	  gw_buck_coeff[it->second] = tmpCoeff[i];
	  gw_buck_exp[it->second] = tmpExp[i];
	  if(bucket_loss == 1)
	    gw_buck_loss[it->second] = tmpLoss[i];
	  z_max[it->second] = tmpz_max[i];
	  z_init[it->second] = tmpz_init[i];
	  nhdBuckMask[it->second] = 1;
	  hash.erase(it);
	}
    }
}

void _nhdLakeMap_mpp_maxNum_C(int *gto, int gnlinksl, int *linkid, int nlinksl, int *tmp_kk, int *tmp_max_num)
{
  map<int,int> hash;
  map<int,int>::iterator it;

  int *tmp = new int[nlinksl];

  for(int i=0; i < nlinksl; i++)
    {
      tmp[i] = 0;
      hash.insert(std::pair<int,int>(linkid[i],i));
    }

  for(int i=0; i < gnlinksl; i++)
    {
      it = hash.find(gto[i]);
      if(it != hash.end())
	{
	  (*tmp_kk)++;
	  tmp[it->second]++;
	}
    }

  for(int i=0;i<nlinksl;i++)
    if(*tmp_max_num < tmp[i])
      *tmp_max_num = tmp[i];

  delete(tmp);
}

void _nhdLakeMap_mpp_tonodeout_C(int *gto,int gnlinksl,int *linkid,int nlinksl,int *tmp_kk,
				 int *ind, int *tmpTo_Node, int *gToNodeOut)
{
  map<int,int> hash;
  map<int,int>::iterator it;

  int *tmp = new int[nlinksl];

  for(int m=0; m < nlinksl; m++)
    {
      tmp[m] = 1;
      hash.insert(std::pair<int,int>(linkid[m],m));
    }

  for(int k=0; k < gnlinksl; k++)
    {
      it = hash.find(gto[k]);
      if(it != hash.end())
	{
	  ind[*tmp_kk] = k+1; //To fortran
	  tmpTo_Node[*tmp_kk] = gto[k];
	  gToNodeOut[INDX(it->second, tmp[it->second] + 1, nlinksl)] = *tmp_kk + 1; //To fortran
	  gToNodeOut[INDX(it->second, 0, nlinksl)] = tmp[it->second];
	  tmp[it->second]++;
	  (*tmp_kk)++;
	}
    }
  delete(tmp);
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


  void nhdLakeMap_mpp_maxNum_C(int *gto, int gnlinksl, int *linkid, int nlinksl, int *kk, int *maxNum)
  {
    int tmp_max_num = *maxNum;
    int tmp_kk = 0;
    _nhdLakeMap_mpp_maxNum_C(gto, gnlinksl, linkid, nlinksl, &tmp_kk, &tmp_max_num);
    *maxNum = tmp_max_num;
    *kk = tmp_kk;
  }

  void nhdLakeMap_mpp_tonodeout_C(int *gto,int gnlinksl,int *linkid,int nlinksl,int *kk,
				  int *ind, int *tmpTo_Node, int *gToNodeOut)
  {
    int tmp_kk = *kk;
    _nhdLakeMap_mpp_tonodeout_C(gto, gnlinksl, linkid, nlinksl, &tmp_kk,
				ind, tmpTo_Node, gToNodeOut);
    *kk = tmp_kk;
  }

}
