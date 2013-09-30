#ifndef HRAP_H
#define HRAP_H
/** File Name: HRAP.h
  * Author   : Zhengtao Cui
  * Created on : 10/27/03
  * Description: Declearation of HRAP class
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

#include <utility>
#include <assert.h>
#include <iostream>

#define MAX_HRAP                        1601
#define MIN_HRAP                       -1601

template< typename T >
class HRAP {
	friend std::ostream& operator<< ( std::ostream& os, 
			                      const HRAP< T >& hr )
	{//friend
	     os << '(' << hr.x << ',' << hr.y << ')';
	     return os;
	}//friend

	friend std::istream& operator>> ( std::istream& is, HRAP< T >& hr )
	{//friend
	     is >> hr.x ;
             is.ignore( );
             is >> hr.y;
	     return is;
	}//friend

	public:

	typedef T ValueType;

		T x; // x coordinate
		T y; // y coordinate
		/** default constructor
		 */
		HRAP(void);
		/** Constructor
		 */
		HRAP( T const& harpx, T const& harpy );

		/** Constructor
		 */
		HRAP( std::pair<T, T> const& point );

		/** set values using lon, lat
		 */
		void setByLonLat( float const& lon, float const& lat );
		void setByLonLat( std::pair<float, float> const& lonLatPoint );
		/** return lon, lat of this HRAP point
		 */
		std::pair<float, float> LonLat();
		/** Get X coordinate
		 */
		T getX();
		/** Get Y coordinate
		 */
		T getY();
		/** shift by delta x and delta y
		 */
		void shift( T const& deltaX, T const& deltaY );
		/** shift by lon, lat
		 */
		void shiftByLonLat( float const& deltaLon, float const& deltaLat );
		/** get Polar Stereographic coordinates with units 
		 * of meters
		 */
		std::pair<float, float> ster();

		bool isInsideBound( T const& lftx, T const& rgtx,
				    T const& dny, T const& upy )
               {//isInsideBound
                  return x >= lftx && x <= rgtx && y >= dny && y <= upy;
               }//isInsideBound

                /** output to std string in the format of "(x,y)"
                 */
                std::string toString();

		bool operator==( const HRAP< T >& ) const;
		
		HRAP< T >& operator=( const HRAP< T >& );

		/* parameters of HRAP
		 */
		static const float PROJECT_CENTER_LON_IN_DEGREE;
		static const float TRUE_LAT_IN_DEGREE;
		static const float EARTH_RADIUS_IN_KM;

                static float HRAP2KM( T const& unitInHrap );
                static T KM2HRAP( float const& unitInKM );
}; // class HRAP

typedef std::pair< HRAP< float >, HRAP< float > > BOUNDARY;

#define MAX_HRAP_BOUND std::make_pair( HRAP< float >(               \
                                         float( MAX_HRAP ),         \
                                         float( MAX_HRAP )          \
                                                    ),              \
	                               HRAP< float >(               \
                                         float( MIN_HRAP ),         \
                                         float( MIN_HRAP )          \
                                                    )               \
                                     )

#endif //#ifndef HRAP_H
