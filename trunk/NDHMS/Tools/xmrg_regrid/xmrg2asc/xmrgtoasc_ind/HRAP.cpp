/** File Name: HRAP.cpp
  * Author   : Zhengtao Cui
  * Created on : 10/28/03
  * Description: Implementation of HRAP class
  */

/**
  * This class deals with HRAP coordinates:

    The HRAP grid is defined in the plane of a polar stereographic map
    projection with the following parameters:
    longitude of the projection center = -105, standard (true) latitude = 60 N.
    A spherical earth of radius 6371.2 km is assumed in defining the HRAP
    coordinate system.  Using "ster" as the second argument to the
    xmrgtoasc_lin.c program generates header information in 
    Polar Stereographic coordinates with units of meters. 
    The "hrap" argument generates header information in HRAP units. 
    These Polar Stereographic coordinates are related to HRAP
    coordinates as follows: 

      xster=hrapx*4762.5 - 401*4762.5 
      yster=hrapy*4762.5-1601*4762.5 

      Example projection file: 
         input 
         projection polar 
         spheroid sphere 
         units meters 
         parameters 
         -105 0 0          // longitude of the center of the projection
         60 0 24.5304792            // true latitude dd mm ss         
         0.0                                //false easting          
         0.0                                //false northing        
         output 
         projection geographic 
         spheroid sphere 
         units dd 
         parameters 
         end
*
*/


#include "boost/lexical_cast.hpp"
#include "HRAP.h"
#include "projection.h"

using namespace std;


/* default constructor
 */
template< typename T> 
HRAP< T >::HRAP(){ } 

/* constructor that takes two arguments
 */
template< typename T>
HRAP< T >::HRAP( T const& harpx, T const& harpy ){
	x = harpx;
	y = harpy;
} //HRAP::HRAP( float harpx, float harpy )

/** constructor that takes one argument
 */

template< typename T>
HRAP< T >::HRAP( pair<T, T> const& point ){
	x = point.first;
	y = point.second;
} // HRAP::HRAP( pair<float, float> point )

/** set values using lon, lat
 */
template< typename T>
void HRAP< T >::setByLonLat( float const& lon, float const& lat ){

      assert( lon >= -180.0 && lon <= 180.0 );
      assert( lat >=  0.0 && lat <= 90.0 );
      struct point inpoint;
      struct point outpoint;
      inpoint.x = lon;
      inpoint.y = lat;
      outpoint = geotohrap( inpoint );

      // should be OK here casting double to float
      x = (T)outpoint.x;
      y = (T)outpoint.y;
} //HRAP::setByLonLat( float lon, float lat )

template< typename T>
void HRAP< T >::setByLonLat( std::pair<float, float> const& lonLatPoint ){
        float lon, lat;
	lon = lonLatPoint.first;
	lat = lonLatPoint.second;
	setByLonLat( lon, lat );
} // HRAP::setByLonLat( std::pair<float, float> lonLatPoint )

/** return lat, lon of this HRAP point
 * first is lon, second is lat
 */
template< typename T>
pair<float, float> HRAP< T >::LonLat(){
      
      pair<float, float> lonlatPoint;

      struct point HRAPPoint;
      struct point LonLatPoint;
      HRAPPoint.x = x;
      HRAPPoint.y = y;

      LonLatPoint = hraptogeo( HRAPPoint );

      lonlatPoint.first  = (float)LonLatPoint.x;
      lonlatPoint.second = (float)LonLatPoint.y;

      return lonlatPoint;

} //std::pair<float, float> LonLat()

/** Get X coordinate
 */
template< typename T>
T HRAP< T >::getX(){ return x; }

/** Get Y coordinate
 */
template< typename T>
T HRAP< T >::getY(){ return y; }

/** shift by delta x and delta y
 */
template< typename T>
void HRAP< T >::shift( T const& deltaX, T const& deltaY ){
       x += deltaX;
       y += deltaY;
} // void shift( float deltaX, float deltaY )

/** shift by lat, lon
 */
template< typename T>
void HRAP< T >::shiftByLonLat( float const& deltaLon, float const& deltaLat ){
        // get lon, lat of this point
        pair<float, float> HrapPoint = LonLat();
	// shift the point
	HrapPoint.first += deltaLon;
	HrapPoint.second += deltaLat;

	setByLonLat( HrapPoint );

}// void HRAP::shiftByLonLat( float deltaLon, deltaLat )


template< typename T>
std::string HRAP< T >::toString()
{//toString
     return "(" + boost::lexical_cast< std::string >( x ) + ',' +
                  boost::lexical_cast< std::string >( y ) + ")";
}//toString

/** get Polar Stereographic coordinates with units 
* of meters
 */
template< typename T>
pair<float, float> HRAP< T >::ster(){
      pair<float, float> sterPoint;

      sterPoint.first = x*4762.5 - 401*4762.5 ;
      sterPoint.second = y*4762.5-1601*4762.5 ;

      return sterPoint;
}// pair<float, float> HRAP::ster()

template< typename T>
bool HRAP< T >::operator==( const HRAP< T >& right) const{
     return( x == right.x && y == right.y );
}// operator ==

template< typename T>
HRAP< T >& HRAP< T >::operator=( const HRAP< T >& right){
      x = right.x;
      y = right.y;
      return *this;
} // operator=

template< typename T>
float HRAP< T >::HRAP2KM( T const& unitInHrap ){
     return unitInHrap * 4.7625;
}//HRAP2KM

template< typename T>
T HRAP< T >::KM2HRAP( float const& unitInKM ){
     return ( T )( unitInKM / 4.7625 );
}//HRAP2KM

template< typename T > 
const float HRAP< T >::PROJECT_CENTER_LON_IN_DEGREE  = -105.0;

template< typename T > 
const float HRAP< T >::TRUE_LAT_IN_DEGREE    = 60.0068140;

template< typename T > 
const float HRAP< T > ::EARTH_RADIUS_IN_KM    = 6371.2;     // KM

#if 0
template< typename T>
ostream& operator<< ( ostream& os, const HRAP< T >& h ){
	os << '(' << h.x << ',' << h.y << ')';
	return os;
}// ostream& opertor<< ( ostream& os, const HCell& hc )

template< typename T>
istream& operator>> ( istream& is, HRAP< T >& h ){
	is >> h.x ;
        is.ignore( );
        is >> h.y;
	return is;
}// istream& opertor>> ( istream& is, HCell& hc )

//template< typename T>
ostream& operator<< ( ostream& os, const HRAP< float >& h ){
	os << '(' << h.x << ',' << h.y << ')';
	return os;
}// ostream& opertor<< ( ostream& os, const HCell& hc )

//template< typename T>
istream& operator>> ( istream& is, HRAP< float >& h ){
	is >> h.x ;
        is.ignore( );
        is >> h.y;
	return is;
}// istream& opertor>> ( istream& is, HCell& hc )

//template< typename T>
ostream& operator<< ( ostream& os, const HRAP< int >& h ){
	os << '(' << h.x << ',' << h.y << ')';
	return os;
}// ostream& opertor<< ( ostream& os, const HCell& hc )

//template< typename T>
istream& operator>> ( istream& is, HRAP< int >& h ){
	is >> h.x ;
        is.ignore( );
        is >> h.y;
	return is;
}// istream& opertor>> ( istream& is, HCell& hc )
#endif //#if 0

//  Explicitly instantiate the needed types.  This allows for the
//  implementation to be separated from the definition (*.h file)
//
template class HRAP< int >;
template class HRAP< float >;
//
// These must go after all explicit instantiations above or otherwise
// the compiler will not instantiate since it thinks space already exists.
// Twisted, yes, but statics cannot be initialized unless they exist and
// if they exist then the class containing them exists, or at least that
// what g++ seems to be counting on.

#if 0
const float HRAP< int >::PROJECT_CENTER_LON_IN_DEGREE  = -105.0;     // degree
const float HRAP< int >::TRUE_LAT_IN_DEGREE    = 60.0068140; // degree North
const float HRAP< int >::EARTH_RADIUS_IN_KM    = 6371.2;     // KM

const float HRAP< float >::PROJECT_CENTER_LON_IN_DEGREE  = -105.0;     // degree
const float HRAP< float >::TRUE_LAT_IN_DEGREE    = 60.0068140; // degree North
const float HRAP< float >::EARTH_RADIUS_IN_KM    = 6371.2;     // KM

float HRAP< int >::HRAP2KM( int unitInHrap );

int HRAP< int >::KM2HRAP( float unitInKM );

float HRAP< float >::HRAP2KM( float unitInHrap );

float HRAP< float >::KM2HRAP( float unitInKM );
#endif //#if 0

//ostream& operator<< ( ostream& os, const HRAP< int >& h );
//ostream& operator<< ( ostream& os, const HRAP< float >& h );

//istream& operator>> ( istream& is, HRAP< int >& h );
//istream& operator>> ( istream& is, HRAP< float >& h );
//#pragma instantiate ostream& operator<< ( ostream& os, const HRAP< int >& h );
//#pragma instantiate ostream& operator<< ( ostream& os, const HRAP< float >& h );
//#pragma istream& operator>> ( istream& is, HRAP< int >& h );
//#pragma istream& operator>> ( istream& is, HRAP< float >& h );

