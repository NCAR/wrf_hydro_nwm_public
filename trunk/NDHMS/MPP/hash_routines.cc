#include <map>
#include <iostream>

using namespace std;

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

extern "C"
{
  void getLocalIndx_C(int * gLinkId, int glinksl, int *llinkid, int * llinkidindx, int llinklen)
  {
    _getLocalIndx_C(gLinkId, glinksl, llinkid, llinkidindx, llinklen);
  }
}
