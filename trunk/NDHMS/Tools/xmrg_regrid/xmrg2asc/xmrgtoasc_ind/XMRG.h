#ifndef XMRG_H
#define XMRG_H
/** File Name: XMRG.h
  * Author   : Zhengtao Cui
  * Created on : 10/24/03
  * Description: Declearation of XMRG class
  */

/** REFERENCE: Seann Reed
  * This class deal with three type of xmrg headers:
  * 1) pre-AWIPS Bld 4.2 format
  * 2) post-AWIPS Bld 4.2 format
  * 3) HL-RMS model parameter grid format. These xmrg files have two header
  *    records. The first record is the same as the XMRG record and consiss
  *    of four integers: X- and Y-origins of HARP, and numbers of columns and
  *    rows in the file. The second record differs from XMRG records. It 
  *    includes two variables: an integer scale factor to convert integer*2
  *    values into real values( to get the true values, divide  the value
  *    stored in the file by this factor), and number of byte per value ( 2 for
  *    integer* format and 4 for real*4 foramt). This allows keeping all grids
  *    in either integer*2 or real*4 formats.
  * In all XMRG files, data values are written to the file in row-column format,  * with the southernmost row written to the file first and the northernmost
  * row written last.
  */

#include "HRAP.h"
#include <cmath>
#include <string>
#include <vector>
#include <stdexcept>
//#include <qdatetime.h>

class XMRG{
      private:

         HRAP< float > _origin;               //X- and Y-origins of HRAP

	 std::pair<int, int> _colrows;// number of columns and rows

	 int _scaleFactor;            // scale factor to convert integer*2
	                              // value to real values, to get the true
				      // values, divide the value stored in the 
				      // file by this factor

	 int _numberOfBytes;           // number of bytes per value

         std::string _source;         // the original xmrg file

	 std::vector< std::vector<float > > _values;   // cell values

         float _cellSizeInM;           //cell size in meters

 //        QDateTime _time;              // time stamp of this xmrg

         float _NO_DATA_VALUE;

	 std::string _unit;            //unit of data value

	 // covert hrap to col and row numbers
	 std::pair<int, int> hrapToColRow( const HRAP< float >& ) const ;

	 // covert col and row numbers to HRAP
	 HRAP< float > colRowToHrap( int col, int row );

         // set initial member values
	 void initXMRG();

	 // load XMRG from a file
	 void loadXMRG(const char* file, const bool isBigEndianPar = true,
			                 const bool readAsPar = true,
                                         const bool relcoord = false );
	 // load XMRG from a file for a given boundary
	 void loadXMRG(const char* file, const BOUNDARY& bound,
			                 const bool isBigEndianPar = true,
			                 const bool readAsPar = true,
                                         const bool relcoord = false );
         //load arc/info ASCII  file
         void loadASC( const char* file );

      public:

	 typedef HRAP< float > CoordT;
         //
	 // default constructor
	 //
	 XMRG();

	 // constructor that takes a xmrg file name 
	 XMRG( std::string xmrgFileName, const bool isBigEndianPar = true,
			                 const bool readAsPar = true,
	                                 float nodatavalue = -1.f );

	 XMRG( std::string xmrgFileName, const float& cellSizeInHRAP, 
	                                 float nodatavalue = -1.f, 
                                         bool relcoord = false );

	 // constructor that takes a xmrg file name for a given boundary
	 XMRG( std::string xmrgFileName, const BOUNDARY& bound,  
			                 const bool isBigEndianPar = true,
			                 const bool readAsPar = true,
	                                 float nodatavalue = -1.f );

	 XMRG( std::string xmrgFileName, const BOUNDARY& bound,  
			                 const float& cellSizeInHRAP,
	                                 float nodatavalue = -1.f,
                                         bool relcoord = false );

	 //construct an empty XMRG that has a boundard of bound, 
	 //and has a cell size of cellSizeInHRAP
	 XMRG( const BOUNDARY& bound, float cellSizeInHRAP = 1.0,
	     float nodatavalue = -999.f );

         // copy constructor
	 XMRG( const XMRG& );

	 // get origin
	 HRAP< float > getOrigin() const;

	 // set origin
	 void setOrigin( HRAP< float > const& );

	 // get the file name
	 std::string getSource() const;

	 // get the scale factor
	 int getScaleFactor();

         /**	The orientation of the precipitation field with respect to the
	    i and j indices used in the program is as follows:

          i      j      physical location
          ------------------------------------------------------------------
          0      0      lowerleft corner of the RFC rectangle
     MAXX-1      0      lowerright corner of the RFC rectangle
     0 MAXY-1      upperleft corner of the RFC rectangle
     MAXX-1 MAXY-1      uppperright corner of the RFC rectangle
         */
	 // get value at column i, and row j
	 float valueAt( int i, int j) const;

	 // set value at column i, and row j
	 void setValueAt( int i, int j, float v);

	 //get value at HRAP point
	 float valueAtHRAP( const HRAP< float >& p ) const;

	 //set value at HRAP point
	 void setValueAtHRAP( const HRAP< float >& p, float const& v );

	 //get cell size
	 float getCellSizeInM();

	 void setCellSizeInM( float size );

	 //get cell size
	 float getCellSizeInHRAP () const;

	 void setCellSizeInHRAP( float size );

	 // get total number of columns and rows
	 std::pair<int, int> getColRows();

         // get values
	 std::vector< std::vector<float> > getValues();

	 // get unit
	 std::string unit() const;

	 // set unit
	 void setUnit( std::string const& u );

	 // output to Arc/Info ASCII raster format
	 // first argument is a file name
	 // second argument is either "HRAP" or "ster", default is "HRAP"
	 // if "HRAP" the ASCII raster file will be in HRAP projection
	 // if "ster" it will be in Polar Sterrographic coordinate projection
	 void toASC( std::string fileName, std::string proj = "HRAP",
                     std::string fmt = "%|e|" );

	 void toGnuplot( std::string fileName, std::string proj = "HRAP" );

	 // output to Arc/Info Grid
	 void toGrid( std::string gridName );

         // get time stamp
//	 QDateTime getTime();

	 // add to another xmrg 
         XMRG& operator+=( const XMRG& right );
         XMRG& operator-=( const XMRG& right );

	 // add two xmrgs
         XMRG operator+( const XMRG& op2 );
         XMRG operator-( const XMRG& op2 );

	 // add a float value
         XMRG& operator+=( const float& right );
         XMRG& operator-=( const float& right );

	 // add a float value
         XMRG operator+( const float& right );
         XMRG operator-( const float& right );

         // divided by a number
         template< typename T >
         XMRG operator/( const T op2 ){
	         XMRG x( *this );
		 std::vector< float >::iterator itr;
		std::vector< std::vector< float > >::iterator vitr;
             for( vitr = x._values.begin(); 
        		     vitr != x._values.end(); ++vitr )
             {//fo vitr
                 for( itr = vitr->begin(); itr != vitr->end(); ++itr )
		 {//for itr
                     if( fabs( *itr - _NO_DATA_VALUE ) > 0.00001  )
	        	 *itr /= op2;
		 }//for itr

             }//fo vitr
#if 0
             vector< vector<float> >::iterator itr;
             for( itr = x._values.begin(); itr != x._values.end(); ++itr ){
        	     for_each( itr->begin(), itr->end(),
        			     bind2nd( divides< float >(), op2 ) );
             } // for itr
#endif //#if 0
              return x;
        }//operator/

        // divided by a number
        template< typename T >
        XMRG& operator/=( const T op2 ){
		std::vector< float >::iterator itr;
		std::vector< std::vector< float > >::iterator vitr;
             for( vitr = _values.begin(); 
        		     vitr != _values.end(); ++vitr )
             {//fo vitr
                 for( itr = vitr->begin(); itr != vitr->end(); ++itr )
		 {//for itr
                     if( fabs( *itr - _NO_DATA_VALUE ) > 0.00001  )
	        	 *itr /= op2;
		 }//for itr

             }//fo vitr
             return *this;
        }//operator/

        // multiply  a number
        template< typename T >
        XMRG operator*( const T op2 )
        {//operator*
        	XMRG x( *this );
		std::vector< float >::iterator itr;
		std::vector< std::vector< float > >::iterator vitr;
             for( vitr = x._values.begin(); 
        		     vitr != x._values.end(); ++vitr )
             {//fo vitr
                 for( itr = vitr->begin(); itr != vitr->end(); ++itr )
		 {//for itr
                     if( fabs( *itr - _NO_DATA_VALUE ) > 0.00001  )
	        	 *itr *= op2;
		 }//for itr

             }//fo vitr
             return x;
        }//operator*

        // multiply  a number
        template< typename T >
        XMRG& operator*=( const T op2 )
        {//operator*=
		std::vector< float >::iterator itr;
		std::vector< std::vector< float > >::iterator vitr;
             for( vitr = _values.begin(); 
        		     vitr != _values.end(); ++vitr )
             {//fo vitr
                 for( itr = vitr->begin(); itr != vitr->end(); ++itr )
		 {//for itr
                     if( fabs( *itr - _NO_DATA_VALUE ) > 0.00001  )
	        	 *itr *= op2;
		 }//for itr

             }//fo vitr
             return *this;
        }//operator*=

	 // assign xmrgs
	 XMRG& operator=( const XMRG& right );

	 //Fill the missing data by averaging its neighbor's values
	 void fillMissing();

         void fillMissing( int neg_val );

         //fill with constant value
         void fillConst( float v, int sf = 1 );

	 std::vector< float > & operator[]( const int& row );

	 void toBinFile( const std::string& filename );
	 void toOldPar( std::string filename );

	 float minMignitude() const ;
	 float maxMignitude() const ;
	 float max() const ;
	 float min() const ;
	 float noDataValue() const ;
	 void setNoDataValue( float const& nodata );
	 void resetScale( int );
    //smr add
    void stertohrap();
    //smr add
	 void setNumberOfBytesPerValue( int );
	 int getNumberOfBytesPerValue( );
         void write( std::string filename );
         void writeWithNodatavalue( std::string filename, 
                                               bool compress = true );
         void toXMRGBinary( std::string filename, bool bigendian = false );

         bool empty();

	 static std::string DEGF;
	 static std::string DEGC;
	 static std::string CMS;
	 static std::string CFS;
	 static std::string SQIN;
	 static std::string QIN;
	 static std::string MM;
	 static std::string PERCENT;
	 static std::string NONE;
	 static std::string METER;
	 static std::string PER_DAY;

}; //class XMRG

//exception class for XMRG reading
class XmrgError : public std::exception
{//XmrgError
	public:
		XmrgError( const std::string& whatStr )
		{//XmrgError
			_whatStr = whatStr;
		}//XmrgError

		virtual ~XmrgError() throw() {}

		virtual const char* what() const throw()
		{//what
			return _whatStr.c_str();
		}//what

	private:
		std::string _whatStr;
};//XmrgError
#endif //#ifndef XMRG_H
