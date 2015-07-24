/** File Name: XMRG.cpp
  * Author   : Zhengtao Cui
  * Created on : 05/14/10
  * Description: Implementation of XMRG class
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

#include <unistd.h>
#include "XMRG.h"
#include "linux.h"
#include <zlib.h>
#include <cstdlib>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <cassert>
#include <algorithm>
#include <numeric>
#include <vector>
#include <iostream>
#include <fstream>
#include <functional>
#include <limits>
#include <stdexcept>
#include <ios>

#include <boost/spirit/core.hpp>
#include <boost/spirit/iterator/file_iterator.hpp>
#include <boost/utility.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/format.hpp>
#include "boost/date_time/posix_time/posix_time.hpp"
#include "boost/regex.hpp"
#include "boost/filesystem.hpp"

//extern "C"{
//  #include "gioapi.h"
//} // extern "C"
extern "C"{
  #include "models.h"
} // extern "C"
using namespace std;
using namespace boost::spirit;
namespace fs = boost::filesystem;

extern "C" {
  int big_endian(void);
  int little_endian(void);
  void swap_4_bytes(void *word);
  void swap_2_bytes(void *word);
//  void write_avgrid(CELLTYPE **rectin, const char name1[STRLEN],
//               double csize, double bndbox[4], 
//	       int gridtype, FILE *fp_debug);
} //extern "C"

#define MAX_FILENAME_LENGTH             256

string XMRG::DEGF = "DEGF";
string XMRG::DEGC = "DEGC";
string XMRG::CMS  = "CMS";
string XMRG::CFS  = "CFS";
string XMRG::SQIN  = "SQIN";
string XMRG::QIN  = "QIN";
string XMRG::MM = "MM";
string XMRG::PERCENT = "PERCENT";
string XMRG::NONE    = "NONE";
string XMRG::METER    = "METER";
string XMRG::PER_DAY = "PER_DAY";

// set initial member values
void XMRG::initXMRG(){
//      _colrows = make_pair( 0, 0 );
      _scaleFactor       = 1; //should be read from xmrg file
      _numberOfBytes     = 4; 
      _source            = "N/A";
      _cellSizeInM         = 4762.5; //cell size
//     _time              = QDateTime::currentDateTime();
     _NO_DATA_VALUE   = -999.f;
     _unit = NONE;
} // void XMRG::initXMRG()

// this method can handle .gz file. However, it can not handle .Z file
// the algorithm here is obtained from Seann Read and the modules of 
// read_xmrg_c.c and xmrgtogrid.c
void XMRG::loadXMRG( const char* file, const bool isBigEndianParG /*sreed*/,
		     const bool readAsPar, const bool relcoord ){

     int col, row, tmpBytes;
     void *buffer = malloc( 4 );
     void *buffer2 = malloc( 2 );

     gzFile fp = gzopen( file, "rb");
     if ( fp == NULL ) throw ios_base::failure( "Couldn't open " + _source );

     //assert ( fp != NULL );

     //smr start------------
     // allow the program to switch from one type of file 
     // to another
     bool isBigEndianPar = isBigEndianParG;
     gzread( fp, buffer, 4 );
     //tmpBytes = *((int*)buffer);
     //cerr << "-1 tmpBytes = " << tmpBytes << endl;
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     tmpBytes = *((int*)buffer);
     //cerr << "0 tmpBytes = " << tmpBytes << endl;
     if (tmpBytes != 16)
     {
        //cout << "tempBytes " << tmpBytes << endl;
        if (isBigEndianPar)
           isBigEndianPar=false;
        else
           isBigEndianPar=true;
        
      }

     // cerr << "isBigEndianPar " << isBigEndianPar << endl;
     //smr end--------------
     
     gzseek( fp, 4, SEEK_SET );

     //read X-origin
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     _origin.x = (float)(*((int*)buffer));

     //cerr << " origin.x = " << _origin.x << endl;
     //read Y-origin
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) || 
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     _origin.y = (float)(*((int*)buffer));

     //cerr << " origin.y = " << _origin.y << endl;
     //read number of columns
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     _colrows.first = *((int*)buffer);

     //cerr << " colrow.first = " << _colrows.first << endl;
     //read number of rows
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     _colrows.second = *((int*)buffer);
     //cerr << " colrow.second = " << _colrows.second << endl;

     /*each record is preceded and followed by 4 bytes*/
     /*first record is 4+16+4 bytes*/

     //skip 4 byte to finish first record
     gzseek( fp, 4, SEEK_CUR );

     // read the second record
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     tmpBytes = *((int*)buffer);

     //cerr << "1 tmpBytes = " << tmpBytes << endl;
     //move file pointer to the appropriate position
         /* first record (24) plus second record(74=66+8) is 98*/
     if( tmpBytes ==  66 ){
	          gzseek( fp, 98, SEEK_SET ); 
	          _scaleFactor = 100; 
     } // if typeBytes == 66
     else if( tmpBytes == 38 ){
	          gzseek( fp, 70, SEEK_SET ); // 24 + 4 + 38 + 4 = 70
		  _scaleFactor = 100;
     } // else if tmpBytes == 38
     else if( tmpBytes == 37 ){
	        //One byte less, assuming data is still valid, continue ...
	          gzseek( fp, 69, SEEK_SET ); 
		  _scaleFactor = 100;
     } // else if tmpBytes == 37
     else if( tmpBytes == 16 ){
	     //this is the format of parameter that contains cellsize and
	     // NO DATA VALUE
             //
                     gzseek( fp, 4, SEEK_SET );

                     //read X-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                        ( big_endian()    && !isBigEndianPar ) ) 
	                  swap_4_bytes( buffer );
                      _origin.x = *((float*)buffer);

                      //read Y-origin
                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                           ( big_endian()    && !isBigEndianPar ) ) 
	                    swap_4_bytes( buffer );
                      _origin.y = *((float*)buffer);

	              gzseek( fp, 28, SEEK_SET ); 

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                            ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		       _scaleFactor = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _numberOfBytes = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _cellSizeInM = *((float*)buffer) * 4762.5;

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _NO_DATA_VALUE = *((float*)buffer);

	   	      gzseek( fp, 48, SEEK_SET );
	    
     }//else if 16
     else if( tmpBytes == 12 ){
	     //this is the format of parameter that contains cellsize, which
             //enables float number HRAP coordinate and thus higher resolution
                     gzseek( fp, 4, SEEK_SET );

                     //read X-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                        ( big_endian()    && !isBigEndianPar ) ) 
	                  swap_4_bytes( buffer );
                      _origin.x = *((float*)buffer);

                      //read Y-origin
                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                           ( big_endian()    && !isBigEndianPar ) ) 
	                    swap_4_bytes( buffer );
                      _origin.y = *((float*)buffer);

	              gzseek( fp, 28, SEEK_SET ); 

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                            ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		       _scaleFactor = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _numberOfBytes = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _cellSizeInM = *((float*)buffer) * 4762.5;

	   	      gzseek( fp, 44, SEEK_SET );
	    
     }//else if 12
     else if( tmpBytes == 8 ){

	     bool readAsParL;
	     //
	     //try to find out it is precip or parameter
	     // and reset readAsPar
	     //
             gzseek( fp, 4, SEEK_SET );

	     int ixcoord;
	     float fxcoord;
	     int bytes;

             //try to read X-origin
             gzread( fp, buffer, 4 );
             if( ( little_endian() && isBigEndianPar ) ||
                  ( big_endian()    && !isBigEndianPar ) ) 
	            swap_4_bytes( buffer );
                     ixcoord = *((int*)buffer);
                     fxcoord = *((float*)buffer);

             gzseek( fp, 32, SEEK_SET );

             //try to read bytes
             gzread( fp, buffer, 4 );
             if( ( little_endian() && isBigEndianPar ) ||
                  ( big_endian()    && !isBigEndianPar ) ) 
	            swap_4_bytes( buffer );
                     bytes = *((int*)buffer);
	     
             
             int intMaxHRAP( MAX_HRAP ), intMinHRAP( MIN_HRAP );
             float floatMaxHRAP( MAX_HRAP ), floatMinHRAP( MIN_HRAP );
             if ( relcoord )
             {
                intMaxHRAP = static_cast< int >( 
                               MAX_HRAP / getCellSizeInHRAP() );
                intMinHRAP = static_cast< int >( 
                               MIN_HRAP / getCellSizeInHRAP() );
                floatMaxHRAP = MAX_HRAP / getCellSizeInHRAP();
                floatMinHRAP = MIN_HRAP / getCellSizeInHRAP();
             }
             else
             {
                 //find out it is precip or parameter format
                 if ( ( fxcoord <= floatMaxHRAP && fxcoord >= floatMinHRAP ) &&
                  ( ixcoord >= intMaxHRAP || ixcoord <= intMinHRAP ) &&
		  ( bytes == 2 || bytes == 4 ) )
	          {//if
                     readAsParL = true; 
	          }//if
	          else
	          {//else if
                     readAsParL = false; 
	           }//else if
             }

		 if( readAsParL ){ //it is parameter
        /* if there are only 4 bytes in the second record, this is not
	    XMRG but an HRAP cell parameter file */
                     gzseek( fp, 4, SEEK_SET );

                     //read X-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                        ( big_endian()    && !isBigEndianPar ) ) 
	                  swap_4_bytes( buffer );
                      _origin.x = *((float*)buffer);

                      //read Y-origin
                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                           ( big_endian()    && !isBigEndianPar ) ) 
	                    swap_4_bytes( buffer );
                      _origin.y = *((float*)buffer);

	              gzseek( fp, 28, SEEK_SET ); 

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                            ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		       _scaleFactor = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _numberOfBytes = *((int*)buffer);

		 }//if _origin.x
		 else{ //if readAsPar          //it is precipitation
			 //re-read _origin.x
                     gzseek( fp, 4, SEEK_SET );

                     //read X-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                         ( big_endian()    && !isBigEndianPar ) ) 
                	     swap_4_bytes( buffer );
                     _origin.x = (float)(*((int*)buffer));

                     //read Y-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) || 
                         ( big_endian()    && !isBigEndianPar ) ) 
                	     swap_4_bytes( buffer );
                     _origin.y = (float)(*((int*)buffer));

                     //read number of columns
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                         ( big_endian()    && !isBigEndianPar ) ) 
                	     swap_4_bytes( buffer );
                     _colrows.first = *((int*)buffer);

                     //read number of rows
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                         ( big_endian()    && !isBigEndianPar ) ) 
                	     swap_4_bytes( buffer );
                     _colrows.second = *((int*)buffer);

  	             _scaleFactor = 100;
		 }// else if _origin.x
		 //skip to the begining of data
		 gzseek( fp, 40, SEEK_SET );
     } // else if tmpBytes == 8
     else if( tmpBytes == 4 ){
                 gzseek( fp, 4, SEEK_SET );

                 //read X-origin
                 gzread( fp, buffer, 4 );
                 if( ( little_endian() && isBigEndianPar ) ||
                     ( big_endian()    && !isBigEndianPar ) ) 
	                swap_4_bytes( buffer );
                 _origin.x = *((float*)buffer);

                 //read Y-origin
                 gzread( fp, buffer, 4 );
                 if( ( little_endian() && isBigEndianPar ) ||
                     ( big_endian()    && !isBigEndianPar ) ) 
	                swap_4_bytes( buffer );
                 _origin.y = *((float*)buffer);

	         gzseek( fp, 28, SEEK_SET ); 

                 gzread( fp, buffer, 4 );
                 if( ( little_endian() && isBigEndianPar ) ||
                     ( big_endian()    && !isBigEndianPar ) ) 
	                swap_4_bytes( buffer );
		 _scaleFactor = *((int*)buffer);

		 gzseek( fp, 36, SEEK_SET );
     } // else if tmpBytes == 4
     else if( tmpBytes == 2 * _colrows.first ){
	   // pre-1997 format
	     // BUG??? if _colrows.first = 2, 4, 6, 19, 33
	     //
	    gzseek( fp, 24, SEEK_SET );
	    _scaleFactor = 100;
     } // else if tmpBytes == 2 * 
     else{
             //cerr << " tmpBytes = " << tmpBytes << endl;
	     throw XmrgError( _source + "Header Error! Check Endian!" );
     } // else if tmpBytes == 2

     if ( relcoord )
     {
        _origin.x = _origin.x * getCellSizeInHRAP();
        _origin.y = _origin.y * getCellSizeInHRAP();
     }
     
     equal_to< float > eq;
     bool first = true;
     //read cell values row by row, each row starts and ends with a 
     //4 byte blank
     if( _numberOfBytes == 2 ){
      for( row = 0; row < _colrows.second; row++){
         gzseek( fp, 4, SEEK_CUR);
	 _values.push_back( vector< float >() );
	 for( col = 0; col < _colrows.first; col++ ){

	     if( gzread( fp, buffer2, 2) <= 0 ){
	      //  cerr << " XMRG data error! Exiting..." << endl;
		if ( first )
		{//if first	
	           cerr << _source << "incomplete! Assumeing Zero ... " << endl;
		   first = false;
		}//if first	
		*((short*)buffer2) = 0;
	//	exit(1);
	     } // if gzread
             else
             {
                if( ( little_endian() && isBigEndianPar ) ||
                 ( big_endian()    && !isBigEndianPar ) ) 
	               swap_2_bytes( buffer2 );
             }

	     if( /*tmpBytes == 16 && */
		   eq( static_cast< float >( *((short*)buffer2) ), 
			   _NO_DATA_VALUE * _scaleFactor ) )
	     {//if
	         _values[ row ].push_back( _NO_DATA_VALUE );
	     }//if
	     else
	     {//else if
	         _values[ row ].push_back( static_cast<float>( 
                         ( *((short*)buffer2) ) ) / _scaleFactor );
	     }//else if

	 } // for col
         gzseek( fp, 4, SEEK_CUR);
      } // for row
     } //if( _numberOfBytes == 2 )
     else{ // if _numberOfBytes
       for( row = 0; row < _colrows.second; row++){
         gzseek( fp, 4, SEEK_CUR);
	 _values.push_back( vector< float >() );
	 for( col = 0; col < _colrows.first; col++ ){

	     if( gzread( fp, buffer, 4) <= 0 ){
	        //cerr << " XMRG data error! Exiting..." << endl;
		if ( first )
		{//if first	
	           cerr << _source << "incomplete! Assumeing Zero ... " << endl;
		   first = false;
		}//if first	
		//exit(1);
	     } // if gzread
             else
             {
                if( ( little_endian() && isBigEndianPar ) ||
                 ( big_endian()    && !isBigEndianPar ) ) 
	               swap_4_bytes( buffer );
             }

	     //if( tmpBytes == 16 && eq( *((float*)buffer), _NO_DATA_VALUE ) )
	     if( eq( *((float*)buffer), _NO_DATA_VALUE  * _scaleFactor) )
	     {//if
	        _values[ row ].push_back( _NO_DATA_VALUE );
	     }//if
	     else
	     {//else if
	        _values[ row ].push_back( *((float*)buffer)  / _scaleFactor );
	     }//else if

	 } // for col
         gzseek( fp, 4, SEEK_CUR);
      } // for row
     } // else if _numberOfBytes

     // cleaning up
     free( buffer );
     free( buffer2);
     gzclose( fp );

} // void loadXMRG() 

// this method can handle .gz file. However, it can not handle .Z file
// the algorithm here is obtained from Seann Reed and the modules of 
// read_xmrg_c.c and xmrgtogrid.c
void XMRG::loadXMRG( const char* file, const BOUNDARY& boundg, 
		                       const bool isBigEndianParG,/*sreed*/
		                       const bool readAsPar,
                                       const bool relcoord ){

     int col, row, tmpBytes, ncol, nrow;
     int bytes_ignored, bytes_right, bytes_left;
     void *buffer = malloc( 4 );
     void *buffer2 = malloc( 2 );

     gzFile fp = gzopen( file, "rb");
     if ( fp == NULL ) throw ios_base::failure( "Couldn't open " + _source );

     //smr start------------
     // allow the program to switch from one type of file 
     // to another
     bool isBigEndianPar = isBigEndianParG;
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     tmpBytes = *((int*)buffer);
     if (tmpBytes != 16)
     {
        //cout << "tempBytes " << tmpBytes << endl;
        if (isBigEndianPar)
           isBigEndianPar=false;
        else
           isBigEndianPar=true;
        
      }
      //cout << "isBigEndianPar " << isBigEndianPar << endl;
     //smr end--------------

     gzseek( fp, 4, SEEK_SET );

     //read X-origin
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     _origin.x = (float)(*((int*)buffer));

     //read Y-origin
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) || 
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     _origin.y = (float)(*((int*)buffer));

     //read number of columns
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     _colrows.first = *((int*)buffer);

     //read number of rows
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     _colrows.second = *((int*)buffer);

     /*each record is preceded and followed by 4 bytes*/
     /*first record is 4+16+4 bytes*/

     //skip 4 byte to finish first record
     gzseek( fp, 4, SEEK_CUR );

     // read the second record
     gzread( fp, buffer, 4 );
     if( ( little_endian() && isBigEndianPar ) ||
         ( big_endian()    && !isBigEndianPar ) ) 
	     swap_4_bytes( buffer );
     tmpBytes = *((int*)buffer);

     //move file pointer to the appropriate position
         /* first record (24) plus second record(74=66+8) is 98*/
     if( tmpBytes ==  66 ){
	          gzseek( fp, 98, SEEK_SET ); 
	          _scaleFactor = 100; 
     } // if typeBytes == 66
     else if( tmpBytes == 38 ){
	          gzseek( fp, 70, SEEK_SET ); // 24 + 4 + 38 + 4 = 70
		  _scaleFactor = 100;
     } // else if tmpBytes == 38
     else if( tmpBytes == 37 ){
	        //One byte less, assuming data is still valid, continue ...
	          gzseek( fp, 69, SEEK_SET ); 
		  _scaleFactor = 100;
     } // else if tmpBytes == 37
     else if( tmpBytes == 16 ){
	     //this is the format of parameter that contains cellsize and
	     // NO DATA VALUE
             //
                     gzseek( fp, 4, SEEK_SET );

                     //read X-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                        ( big_endian()    && !isBigEndianPar ) ) 
	                  swap_4_bytes( buffer );
                      _origin.x = *((float*)buffer);

                      //read Y-origin
                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                           ( big_endian()    && !isBigEndianPar ) ) 
	                    swap_4_bytes( buffer );
                      _origin.y = *((float*)buffer);

	              gzseek( fp, 28, SEEK_SET ); 

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                            ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		       _scaleFactor = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _numberOfBytes = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _cellSizeInM = *((float*)buffer) * 4762.5;

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _NO_DATA_VALUE = *((float*)buffer);

	   	      gzseek( fp, 48, SEEK_SET );
	    
     }//else if 16
     else if( tmpBytes == 12 ){
	     //this is the format of parameter that contains cellsize, which
             //enables float number HRAP coordinate and thus higher resolution
                     gzseek( fp, 4, SEEK_SET );

                     //read X-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                        ( big_endian()    && !isBigEndianPar ) ) 
	                  swap_4_bytes( buffer );
                      _origin.x = *((float*)buffer);

                      //read Y-origin
                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                           ( big_endian()    && !isBigEndianPar ) ) 
	                    swap_4_bytes( buffer );
                      _origin.y = *((float*)buffer);

	              gzseek( fp, 28, SEEK_SET ); 

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                            ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		       _scaleFactor = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _numberOfBytes = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _cellSizeInM = *((float*)buffer) * 4762.5;

		      gzseek( fp, 44, SEEK_SET );
	    
     }//else if 12
     else if( tmpBytes == 8 ){

	     bool readAsParL;
	     //
	     //try to find out it is precip or parameter
	     // and reset readAsPar
	     //
             gzseek( fp, 4, SEEK_SET );

	     int ixcoord;
	     float fxcoord;
	     int bytes;

             //try to read X-origin
             gzread( fp, buffer, 4 );
             if( ( little_endian() && isBigEndianPar ) ||
                  ( big_endian()    && !isBigEndianPar ) ) 
	            swap_4_bytes( buffer );
                     fxcoord = *((float*)buffer);
                     ixcoord = *((int*)buffer);

             gzseek( fp, 32, SEEK_SET );

             //try to read bytes
             gzread( fp, buffer, 4 );
             if( ( little_endian() && isBigEndianPar ) ||
                  ( big_endian()    && !isBigEndianPar ) ) 
	            swap_4_bytes( buffer );
                     bytes = *((int*)buffer);
	     
             int intMaxHRAP( MAX_HRAP ), intMinHRAP( MIN_HRAP );
             float floatMaxHRAP( MAX_HRAP ), floatMinHRAP( MIN_HRAP );
             if ( relcoord )
             {
                intMaxHRAP = static_cast< int >( 
                                 MAX_HRAP / getCellSizeInHRAP() );
                intMinHRAP = static_cast< int >( 
                                 MIN_HRAP / getCellSizeInHRAP() );
                floatMaxHRAP = MAX_HRAP / getCellSizeInHRAP();
                floatMinHRAP = MIN_HRAP / getCellSizeInHRAP();
             }
             else
             {
                //find out it is precip or parameter format
                if ( ( fxcoord <= floatMaxHRAP && fxcoord >= floatMinHRAP ) &&
                  ( ixcoord >= intMaxHRAP || ixcoord <= intMinHRAP ) &&
		  ( bytes == 2 || bytes == 4 ) )
	         {//if
                   readAsParL = true; 
	          }//if
	         else
	         {//else if
                   readAsParL = false; 
	         }//else if
             }

		 if( readAsParL ){ //it is parameter
        /* if there are only 4 bytes in the second record, this is not
	    XMRG but an HRAP cell parameter file */
                     gzseek( fp, 4, SEEK_SET );

                     //read X-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                        ( big_endian()    && !isBigEndianPar ) ) 
	                  swap_4_bytes( buffer );
                      _origin.x = *((float*)buffer);

                      //read Y-origin
                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                           ( big_endian()    && !isBigEndianPar ) ) 
	                    swap_4_bytes( buffer );
                      _origin.y = *((float*)buffer);

	              gzseek( fp, 28, SEEK_SET ); 

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                            ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		       _scaleFactor = *((int*)buffer);

                      gzread( fp, buffer, 4 );
                      if( ( little_endian() && isBigEndianPar ) ||
                              ( big_endian()    && !isBigEndianPar ) ) 
	                   swap_4_bytes( buffer );
		      _numberOfBytes = *((int*)buffer);

		 }//if _origin.x
		 else{ //if readAsPar          //it is precipitation
			 //re-read _origin.x
                     gzseek( fp, 4, SEEK_SET );

                     //read X-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                         ( big_endian()    && !isBigEndianPar ) ) 
                	     swap_4_bytes( buffer );
                     _origin.x = (float)(*((int*)buffer));

                     //read Y-origin
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) || 
                         ( big_endian()    && !isBigEndianPar ) ) 
                	     swap_4_bytes( buffer );
                     _origin.y = (float)(*((int*)buffer));

                     //read number of columns
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                         ( big_endian()    && !isBigEndianPar ) ) 
                	     swap_4_bytes( buffer );
                     _colrows.first = *((int*)buffer);

                     //read number of rows
                     gzread( fp, buffer, 4 );
                     if( ( little_endian() && isBigEndianPar ) ||
                         ( big_endian()    && !isBigEndianPar ) ) 
                	     swap_4_bytes( buffer );
                     _colrows.second = *((int*)buffer);
		 }// else if _origin.x
		 //skip to the begining of data
		 gzseek( fp, 40, SEEK_SET );
     } // else if tmpBytes == 8
     else if( tmpBytes == 4 ){
                 gzseek( fp, 4, SEEK_SET );

                 //read X-origin
                 gzread( fp, buffer, 4 );
                 if( ( little_endian() && isBigEndianPar ) ||
                     ( big_endian()    && !isBigEndianPar ) ) 
	                swap_4_bytes( buffer );
                 _origin.x = *((float*)buffer);

                 //read Y-origin
                 gzread( fp, buffer, 4 );
                 if( ( little_endian() && isBigEndianPar ) ||
                     ( big_endian()    && !isBigEndianPar ) ) 
	                swap_4_bytes( buffer );
                 _origin.y = *((float*)buffer);

	         gzseek( fp, 28, SEEK_SET ); 

                 gzread( fp, buffer, 4 );
                 if( ( little_endian() && isBigEndianPar ) ||
                     ( big_endian()    && !isBigEndianPar ) ) 
	                swap_4_bytes( buffer );
		 _scaleFactor = *((int*)buffer);

		 gzseek( fp, 36, SEEK_SET );
     } // else if tmpBytes == 4
     else if( tmpBytes == 2 * _colrows.first ){
	   // pre-1997 format
	     // BUG??? if _colrows.first = 2, 4, 6, 19, 33
	     //
	    gzseek( fp, 24, SEEK_SET );
	    _scaleFactor = 100;
     } // else if tmpBytes == 2 * 
     else{
	    throw XmrgError( _source + "Header Error! Check Endian!" );
     } // else if tmpBytes == 2

     if ( relcoord )
     {
        _origin.x = _origin.x * getCellSizeInHRAP();
        _origin.y = _origin.y * getCellSizeInHRAP();
     }

     //
     //reset the boundary because current XMRG doesn't allow real number of 
     //boundary, and the cell size is fixed at 1 HRAP unit
     //
     //When the XMRG has been changed to allow smaller cell size, this part
     //will have to be changed accordingly
     //
     
     BOUNDARY bound( boundg );
     equal_to< float > eq;
     if ( eq( fmod( _cellSizeInM, 4762.5f ), 0.0f ) )
     {//if

        bound.first.x = floor( boundg.first.x );
        bound.first.y = floor( boundg.first.y );
        bound.second.x = ceil( boundg.second.x );
        bound.second.y = ceil( boundg.second.y );
     }//if

     //read cell values row by row, each row starts and ends with a 
     //4 byte blank
     //
     //get the sub-window position 
                               //bytes ignored on top
     bytes_ignored = (int)( fabs( bound.first.y - _origin.y ) 
		                                 / getCellSizeInHRAP() ) 
	                * ( _colrows.first * _numberOfBytes +  8 ); 
			                       //4 bytes at each line's
         			   	      //start and end
					      //
     if ( bound.second.x < _origin.x ) 
     {
	     throw std::runtime_error( 
			     "boundary is out of range -- " + _source );
      }

     //assert(  bound.second.x >=  _origin.x );

     bytes_right = static_cast< int >( _colrows.first - 
                                      round( 1.f / getCellSizeInHRAP() ) -
                         round( ( bound.second.x - _origin.x ) 
			          / getCellSizeInHRAP() )
		        ) * _numberOfBytes;
     bytes_left = static_cast< int >( 
                          round( fabs( bound.first.x - _origin.x ) / 
		         getCellSizeInHRAP() ) ) * _numberOfBytes;

     //get number of col and row for the subwindow
     nrow = static_cast< int >( round( ( bound.second.y - bound.first.y ) /
		                                getCellSizeInHRAP() ) + 
            round( 1.f / getCellSizeInHRAP() ) );
     ncol = static_cast< int >( round( ( bound.second.x - bound.first.x ) / 
		                                getCellSizeInHRAP() ) +
            round( 1.f / getCellSizeInHRAP() ) );
     bool first = true;
     //skip ingored data
     gzseek( fp, bytes_ignored, SEEK_CUR);
     if( _numberOfBytes == 2 ){
      for( row = 0; row < nrow; row++){
         gzseek( fp, 4 + bytes_left, SEEK_CUR); //4 bytes at the begining of 
	                                        //the line
	 _values.push_back( vector< float >() );
	 for( col = 0; col < ncol; col++ ){

	     if( gzread( fp, buffer2, 2) <= 0 ){
	        //cerr << " XMRG data error! Exiting..." << endl;
		if ( first )
		{//if first	
	           cerr << _source << "incomplete! Assumeing Zero..." << endl;
		   first = false;
		}//if first	
		*((short*)buffer2) = 0;
		//exit(1);
	     } // if gzread
             else
             {
               if( ( little_endian() && isBigEndianPar ) ||
                 ( big_endian()    && !isBigEndianPar ) ) 
	               swap_2_bytes( buffer2 );
             }

	     //if( tmpBytes == 16 && 
	//	   eq( static_cast< float >( *((short*)buffer2) ), 
	     if( eq( static_cast< float >( *((short*)buffer2) ), 
			   _NO_DATA_VALUE * _scaleFactor ) )
	     {//if
	         _values[ row ].push_back( _NO_DATA_VALUE );
	     }//if
	     else
	     {//else if
	         _values[ row ].push_back( (float)( *((short*)buffer2) ) /
			                _scaleFactor );
	     }//else if

	 } // for col
         gzseek( fp, 4 + bytes_right, SEEK_CUR); //skip bytes at the right
      } // for row
     } //if( _numberOfBytes == 2 )
     else{ // if _numberOfBytes == 2
       for( row = 0; row < nrow; row++){
         gzseek( fp, 4 + bytes_left, SEEK_CUR);
	 _values.push_back( vector< float >() );
	 for( col = 0; col < ncol; col++ ){

	     if( gzread( fp, buffer, 4) <= 0 ){
		if ( first )
		{//if first	
	           cerr << _source << "incomplete! Assumeing Zero..." << endl;
		   first = false;
		}//if first	
	     } // if gzread
             else
             {
               if( ( little_endian() && isBigEndianPar ) ||
                 ( big_endian()    && !isBigEndianPar ) ) 
	               swap_4_bytes( buffer );
             }

	     if( /*tmpBytes == 16 &&*/ eq( *((float*)buffer), _NO_DATA_VALUE * _scaleFactor ) )
	     {//if
	        _values[ row ].push_back( _NO_DATA_VALUE );
	     }//if
	     else
	     {//else if
	        _values[ row ].push_back( *((float*)buffer)  / _scaleFactor );
	     }//else if

	 } // for col
         gzseek( fp, 4 + bytes_right, SEEK_CUR); //skip bytes at the right
      } // for row
     } // else if _numberOfBytes

     // cleaning up
     free( buffer );
     free( buffer2);
     gzclose( fp );

     //update _colrows
     _colrows.first = ncol;
     _colrows.second = nrow;
     //update _origin
     _origin.x =  bound.first.x;
     _origin.y =  bound.first.y;

     return;
} // void loadXMRG() 

// covert hrap to col and row numbers
pair<int, int> XMRG::hrapToColRow( const HRAP< float >& p) const {
     float cellSize = getCellSizeInHRAP();
     if ( !( ( p.x - _origin.x >= 0 ) && 
             ( (int)( ( p.x - _origin.x ) / cellSize ) < _colrows.first ) &&
	     ( p.y - _origin.y >= 0 ) &&
	     ( (int)( ( p.y - _origin.y ) / cellSize ) < _colrows.second ) ) )
     {//if
	     throw std::runtime_error( "HRAP point( " +
			    boost::lexical_cast< string >( p.x ) + ',' +
			    boost::lexical_cast< string >( p.y ) +
			     ") is out of range -- " + _source );
     }//if

//     assert( ( p.x - _origin.x >= 0 ) && 
//             ( (int)( ( p.x - _origin.x ) / cellSize ) <= _colrows.first ) &&
//	     ( p.y - _origin.y >= 0 ) &&
//	     ( (int)( ( p.y - _origin.y ) / cellSize ) <= _colrows.second ) );

     pair<int, int> colrow;
     colrow.first = static_cast<int>(round( ( p.x - _origin.x ) / cellSize ));
     colrow.second = static_cast<int>(round( ( p.y - _origin.y ) / cellSize ));
     return colrow;
} // hrapToColRow( const HRAP ) 

// covert col and row numbers to HRAP
HRAP< float > XMRG::colRowToHrap( int col, int row ){
    assert( ( col <= _colrows.first ) && ( col >= 0 ) &&
            ( row <= _colrows.second ) && ( row >= 0 ) );
    HRAP< float > hp;
    hp.x = _origin.x + col * getCellSizeInHRAP();
    hp.y = _origin.y + row * getCellSizeInHRAP();
    return hp;
} //colRowToHRrap

//
// default constructor
//
XMRG::XMRG(){

   initXMRG(); 

} // XMRG()

// constructor that takes a xmrg file name 
XMRG::XMRG( std::string xmrgFileName, const bool isBigEndianPar,
		                      const bool readAsPar, float nodatavalue ){

   _scaleFactor       = 100; //should be read from xmrg file
   _numberOfBytes     = 2;
   _cellSizeInM         = 4762.5; //cell size
   _source = xmrgFileName;

   int fileNameLen = xmrgFileName.length();

   if( xmrgFileName.substr( fileNameLen - 2, 2 ) == ".Z" ){
      throw XmrgError( "XMRG can not handle .Z file! --- " + _source );
   }//

   _NO_DATA_VALUE = nodatavalue;
   if( xmrgFileName.substr( xmrgFileName.length() - 4 ) == ".asc" )
	   loadASC( xmrgFileName.c_str() );
   else
	   loadXMRG( xmrgFileName.c_str(), isBigEndianPar, readAsPar, false );

   _unit = NONE;

} //XMRG( std::string )

// constructor that takes a xmrg file name 
XMRG::XMRG( std::string xmrgFileName, const float& cellSizeInHRAP,
		                      float nodatavalue, bool relcoord ){

   _scaleFactor       = 100; //should be read from xmrg file
   _numberOfBytes     = 2;
   _cellSizeInM         = 4762.5 * cellSizeInHRAP; //cell size
   _source = xmrgFileName;

   int fileNameLen = xmrgFileName.length();

   if( xmrgFileName.substr( fileNameLen - 2, 2 ) == ".Z" ){
      throw XmrgError( "XMRG can not handle .Z file! --- " + _source );
   }//

   _NO_DATA_VALUE = nodatavalue;
   if( xmrgFileName.substr( xmrgFileName.length() - 4 ) == ".asc" )
	   loadASC( xmrgFileName.c_str() );
   else
	   loadXMRG( xmrgFileName.c_str(), false, false, relcoord );

   _unit = NONE;

} //XMRG( std::string )

XMRG::XMRG( std::string xmrgFileName, const BOUNDARY& bound,
		                      const bool isBigEndianPar,
		                      const bool readAsPar, float nodatavalue ){

   _scaleFactor       = 100; //should be read from xmrg file
   _numberOfBytes     = 2;
   _cellSizeInM         = 4762.5; //cell size
   _source = xmrgFileName;

   int fileNameLen = xmrgFileName.length();

   if( xmrgFileName.substr( fileNameLen - 2, 2 ) == ".Z" ){
      throw XmrgError( "XMRG can not handle .Z file! --- " + _source );
   }//

   _NO_DATA_VALUE = nodatavalue;

   _unit = NONE;

   loadXMRG( xmrgFileName.c_str(), bound, isBigEndianPar, readAsPar );

} //XMRG( std::string )

XMRG::XMRG( std::string xmrgFileName, const BOUNDARY& bound,
		                      const float& cellSizeInHRAP,
		                      float nodatavalue, bool relcoord ){

   _scaleFactor       = 100; //should be read from xmrg file
   _numberOfBytes     = 2;
   _cellSizeInM         = 4762.5 * cellSizeInHRAP; //cell size
   _source = xmrgFileName;

   int fileNameLen = xmrgFileName.length();

   if( xmrgFileName.substr( fileNameLen - 2, 2 ) == ".Z" ){
      throw XmrgError( "XMRG can not handle .Z file! --- " + _source );
   }//

   _NO_DATA_VALUE = nodatavalue;

   _unit = NONE;

   loadXMRG( xmrgFileName.c_str(), bound, false, false, relcoord );

} //XMRG( std::string )

//construct an empty XMRG that has a boundard of bound, 
//and has a cell size of cellSizeInHRAP
XMRG::XMRG( const BOUNDARY& bound, float cellSizeInHRAP, float nodatavalue ){
        _scaleFactor       = 1; //should be read from xmrg file
        _source            = "N/A";
        _numberOfBytes     = 4;
	_origin.x        = bound.first.x;
	_origin.y        = bound.first.y;
	_colrows.first   = static_cast< int >( round( 
                             ( bound.second.x - bound.first.x ) / 
		            cellSizeInHRAP ) + 
                            round( 1.f / cellSizeInHRAP ) );
	_colrows.second  = static_cast< int >( round( 
                             ( bound.second.y - bound.first.y ) /
		            cellSizeInHRAP ) + 
                           round( 1.f / cellSizeInHRAP ) );

	_cellSizeInM     = cellSizeInHRAP * 4762.5;
        _NO_DATA_VALUE = nodatavalue;

        _unit = NONE;

	_values.resize( _colrows.second );
	for( vector< vector< float > >::iterator itr = _values.begin();
			itr != _values.end(); ++itr ){
		itr->resize( _colrows.first );
	}//itr

	fillConst( _NO_DATA_VALUE  * _scaleFactor);

	return;
}//XMRG

// copy constructor 
XMRG::XMRG( const XMRG& xmrg ){
     _origin          = xmrg._origin;
     _colrows         = xmrg._colrows;
     _scaleFactor     = xmrg._scaleFactor;
     _numberOfBytes   = xmrg._numberOfBytes;
     _source          = xmrg._source;
     _values          = xmrg._values;
     _cellSizeInM     = xmrg._cellSizeInM;
//     _time            = xmrg._time;
     _NO_DATA_VALUE   = xmrg._NO_DATA_VALUE;
     _unit            = xmrg._unit;
}// XMRG

// get origin
HRAP< float > XMRG::getOrigin() const{
    return _origin;
} // getOrigin()

// set origin
void XMRG::setOrigin( HRAP< float > const& o ){
    _origin = o;
} // setOrigin()

// get the file name
string XMRG::getSource() const { return _source; }

// get the scale factor
int XMRG::getScaleFactor(){ return _scaleFactor; }

// get value at column i, and row j
float XMRG::valueAt( int i, int j) const{
    return _values[ j ][ i ];
} //valueAt

//get value at HRAP point
float XMRG::valueAtHRAP( const HRAP< float >& p ) const {

        pair<int , int> colrow; 
	try{
            colrow = hrapToColRow( p );
	}//try
	catch ( ... ) 
	{//catch
	   throw; 
	}//catch

	   //data are stored row by row
   return _values[ colrow.second ][ colrow.first ];
}// valueAtHRAP

// set value at column i, and row j
void XMRG::setValueAt( int i, int j, float v){
     assert( i <= _colrows.first && i >= 0 &&
             j <= _colrows.second && j >= 0 );
     _values[ j ][ i ] =  v;
} //valueAt

//set value at HRAP point
void XMRG::setValueAtHRAP( const HRAP< float >& p, float const& v ){
//   assert( ( p.x - _origin.x >= 0 ) && 
//           ( p.x - _origin.x <= _colrows.first ) &&
//	   ( p.y - _origin.y >= 0 ) &&
//	   ( p.y - _origin.y <= _colrows.second ) );
   try{
   pair<int , int> colrow = hrapToColRow( p );
   _values[ colrow.second ][ colrow.first ] = v;
   }//try{
   catch( ... ){
      throw;
   }

} //setValueAtHRAP()

//get cell size
float XMRG::getCellSizeInM(){ return _cellSizeInM; }

void XMRG::setCellSizeInM( float size )
{//setCellSizeInM
       _cellSizeInM = size; 
}//setCellSizeInM

float XMRG::getCellSizeInHRAP() const
{//getCellSizeInHRAP
	return HRAP< float >::KM2HRAP( _cellSizeInM / 1000. );
}//getCellSizeInHRAP

void XMRG::setCellSizeInHRAP( float size )
{//setCellSizeInHRAP
          _cellSizeInM = HRAP< float >::HRAP2KM( size ) * 1000.; //KM to M
}//setCellSizeInHRAP

// get total number of columns and rows
pair<int, int> XMRG::getColRows(){ return _colrows; }

// get values
vector< vector<float> > XMRG::getValues(){
     return _values;
} // getValues

// get unit
std::string XMRG::unit() const{ return _unit; }

// set unit
void XMRG::setUnit( std::string const& u ){ _unit = u; }

// output to Arc/Info ASCII raster format
// first argument is a file name
// second argument is either "HRAP" or "ster", default is "HRAP"
// if "HRAP" the ASCII raster file will be in HRAP projection
// if "ster" it will be in Polar Sterrographic coordinate projection
void XMRG::toASC( string fileName, string proj, string fmt ){
       if ( !( proj == "HRAP" || proj == "ster") ) {
	   throw XmrgError( fileName + " Wrong projection name !" );
       } // if proj

       // to see if we can open it for write
       ofstream ascFile( fileName.c_str(), ios::out );
       if( !ascFile ){
	   throw ios_base::failure( fileName + " could not be opened!" );
       } // if !ascFile

       int col, row;

       // print ASCII raster header
       ascFile << "ncols " << _colrows.first << endl;
       ascFile << "nrows " << _colrows.second << endl;
       ascFile.setf(ios::fixed, ios::floatfield );
       if( proj == "HRAP" ){
            ascFile << "xllcorner " << _origin.x << endl;
            ascFile << "yllcorner " << _origin.y << endl;
            ascFile << "cellsize " << getCellSizeInHRAP() << endl;
       } // if"HRAP" 
       else{ // if "HRAP"
            ascFile << "xllcorner " << _origin.ster().first << endl;
            ascFile << "yllcorner " << _origin.ster().second << endl;
            ascFile << "cellsize "  << _cellSizeInM << endl;
       } // else if "HRSP"
       ascFile << "NODATA_value " << _NO_DATA_VALUE << endl;
       ascFile.unsetf(ios::fixed );

       equal_to< float > eq;
       //print cell values
       for( row = _colrows.second - 1; row >= 0; row-- ){
            for( col = 0; col < _colrows.first; col++ ){
	        if( eq( _values[ row ][ col ], _NO_DATA_VALUE ) ){
		    ascFile << _NO_DATA_VALUE << ' ';
		} // if _values
		else{ // if values
		    ascFile << boost::format( fmt ) % _values[ row ][ col ] 
			    << ' ';
		} // else if
	    } // for col
            ascFile << endl;
       } //for row

       ascFile.close();
}// toASC

// output to Gnuplot format
// first argument is a file name
// second argument is either "HRAP" or "ster", default is "HRAP"
// if "HRAP" the ASCII raster file will be in HRAP projection
// if "ster" it will be in Polar Sterrographic coordinate projection
void XMRG::toGnuplot( string fileName, string proj ){
       if ( !( proj == "HRAP" || proj == "ster") ) {
	   throw XmrgError( fileName + " Wrong projection name !" );
       } // if proj

       // to see if we can open it for write
       ofstream ascFile( fileName.c_str(), ios::out );
       if( !ascFile ){
	   throw ios_base::failure( fileName + " could not be opened!" );
       } // if !ascFile

       int col, row;
       float cellSize;

       // print ASCII raster header
       ascFile << "#ncols " << _colrows.first << endl;
       ascFile << "#nrows " << _colrows.second << endl;
       ascFile.setf(ios::fixed, ios::floatfield );
       if( proj == "HRAP" ){
            ascFile << "#xllcorner " << _origin.x << endl;
            ascFile << "#yllcorner " << _origin.y << endl;
            ascFile << "#cellsize " << getCellSizeInHRAP() << endl;
            cellSize = getCellSizeInHRAP();
       } // if"HRAP" 
       else{ // if "HRAP"
            ascFile << "#xllcorner " << _origin.ster().first << endl;
            ascFile << "#yllcorner " << _origin.ster().second << endl;
            ascFile << "#cellsize "  << _cellSizeInM << endl;
            cellSize = _cellSizeInM;
       } // else if "HRSP"
       ascFile << "#NODATA_value " << _NO_DATA_VALUE << endl;
       ascFile.unsetf(ios::fixed );

       equal_to< float > eq;
       //print cell values
       for( col = 0; col < _colrows.first; ++col ){
           for( row = 0; row < _colrows.second; ++row ){
	        if( eq( _values[ row ][ col ], _NO_DATA_VALUE ) ){
		    ascFile << _origin.x + cellSize * col << ' '
                            << _origin.y + cellSize * row << ' '
                            << _NO_DATA_VALUE << std::endl;
		} // if _values
		else{ // if values
		    ascFile << _origin.x + cellSize * col << ' '
                            << _origin.y + cellSize * row << ' '
		            << _values[ row ][ col ] << std::endl;
		} // else if
	    } // for row
            ascFile << endl;
       } //for col
       ascFile.close();
}// toGnuplot

// output to an ascii file list
// first argument is a file name
// if "HRAP" the ASCII raster file will be in HRAP projection
// if "ster" it will be in Polar Sterrographic coordin
/*void XMRG::toAsclist( string fileName, string proj ){
       if ( !( proj == "HRAP" || proj == "ster") ) {
	   throw XmrgError( fileName + " Wrong projection name !" );
       } // if proj

       // to see if we can open it for write
       ofstream ascFile( fileName.c_str(), ios::out );
       if( !ascFile ){
	   throw ios_base::failure( fileName + " could not be opened!" );
       } // if !ascFile

       int col, row;

       // print ASCII raster header
       ascFile << "ncols " << _colrows.first << endl;
       ascFile << "nrows " << _colrows.second << endl;
       ascFile.setf(ios::fixed, ios::floatfield );
       if( proj == "HRAP" ){
            ascFile << "xllcorner " << _origin.x << endl;
            ascFile << "yllcorner " << _origin.y << endl;
            ascFile << "cellsize " << getCellSizeInHRAP() << endl;
       } // if"HRAP" 
       else{ // if "HRAP"
            ascFile << "xllcorner " << _origin.ster().first << endl;
            ascFile << "yllcorner " << _origin.ster().second << endl;
            ascFile << "cellsize "  << _cellSizeInM << endl;
       } // else if "HRSP"
       ascFile << "NODATA_value " << _NO_DATA_VALUE << endl;
       ascFile.unsetf(ios::fixed );

       equal_to< float > eq;
       //print cell values
       for( row = _colrows.second - 1; row >= 0; row-- ){
            for( col = 0; col < _colrows.first; col++ ){
	        if( eq( _values[ row ][ col ], _NO_DATA_VALUE ) ){
		    ascFile << _NO_DATA_VALUE << ' ';
		} // if _values
		else{ // if values
		    ascFile << _values[ row ][ col ] << ' ';
		} // else if
	    } // for col
            ascFile << endl;
       } //for row

       ascFile.close();
}// toASC*/

// output to Arc/Info Grid
void XMRG::toGrid( std::string gridName ){
     cerr << " need to be implemented!" << gridName << endl;
#if 0 // relize that the avgridio library is not available on Linux
      // need to be implemented later

     CELLTYPE** matrix = (CELLTYPE**)NULL;
     double outbndbox[4];

     FILE* debugfile = fopen( "toGridDebug.txt", "w+");
     int col, row;
     //bounding box
     outbndbox[ 0 ] = (double)(_origin.ster().first);
     outbndbox[ 1 ] = (double)(_origin.ster().second);
     outbndbox[ 2 ] = (double)(_origin.ster().first + 
                               _colrows.first * _cellSizeInM );
     outbndbox[ 3 ] = (double)(_origin.ster().second + 
                               _colrows.second * _cellSizeInM );
      

      // allocate memory
      matrix = new CELLTYPE*[ _colrows.first ];
      for( col = 0; col < _colrows.first; col++){
          matrix[ col ] = new CELLTYPE[ _colrows.second ];
      }// for col

      // fill the matrix
      for( col = 0; col < _colrows.first; col++){
         for( row = 0; row < _colrows.second; row++){
	     matrix[ col ][ row ] = (CELLTYPE)(
	                     _values[ col ][ _colrows.second - row - 1]
			     / _scaleFactor
			            );
	 } // for row
      } //for col
      
      write_avgrid( matrix, 
                    gridName.c_str(),
		    _cellSizeInM, 
		    outbndbox, 
		    CELLFLOAT, 
		    debugfile );

       //clean up
       for( col = 0; col < _colrows.first; col++) delete[] matrix[ col ];
       delete[] matrix;

       fclose( debugfile );
#endif //#if 0
       return;
}// toGrid

// get time stamp
//QDateTime XMRG::getTime() { return _time; }

// add another xmrg 
XMRG& XMRG::operator+=( const XMRG& right ){
     if ( _cellSizeInM != right._cellSizeInM )
     {
	     throw std::runtime_error( std::string(
                                    "XMRG cell size not equal (+=): " ) + 
                 _source + " = " + 
                  boost::lexical_cast< std::string > ( _cellSizeInM  ) +
                 right._source + " = " + 
                  boost::lexical_cast< std::string > ( right._cellSizeInM  ) );
     }
     if ( _origin.x != right._origin.x || _origin.y != right._origin.y )
     {
	     throw std::runtime_error( std::string (
                           "XMRG origins are not the same (+=): " ) + 
                 _source + " = ( " + 
                  boost::lexical_cast< std::string > ( _origin.x  ) + ", " +
                  boost::lexical_cast< std::string > ( _origin.y  ) + ") " +
                 right._source + " = (" + 
                  boost::lexical_cast< std::string > ( right._origin.x ) +
                  ", " +
                  boost::lexical_cast< std::string > ( right._origin.y ) + ") "
                   );
     }
     if ( _colrows.first != right._colrows.first || 
          _colrows.second != right._colrows.second )
     {
	     throw std::runtime_error( std::string(
                              "XMRG number of columns/rows are not "
                                       " the same (+=): " ) + 
                 _source + " = ( " + 
                boost::lexical_cast< std::string > ( _colrows.first ) + ", " +
                boost::lexical_cast< std::string > ( _colrows.second ) + ") " +
                right._source + "  = (" + 
                boost::lexical_cast< std::string > ( right._colrows.first ) +
                  ", " +
            boost::lexical_cast< std::string > ( right._colrows.second ) + ") "
                   );
     }
     int col, row;
     not_equal_to< float > neq;
     for( row = 0; row < _colrows.second; row++){
        for( col = 0; col < _colrows.first; col++)
	{//for col
         if( neq( _values[row][col], _NO_DATA_VALUE ) )	
	 {//if
            if( neq( right._values[row][col], _NO_DATA_VALUE ) )
	           _values[row][col] += right._values[row][col];
	 }//if
	 else
            if( neq( right._values[row][col], _NO_DATA_VALUE ) )
	           _values[row][col] = right._values[row][col];
	}//for col
     } // for row
      // this block can be replaced by above 
//      vector< vector<float> >::iterator itr;
//      for( itr = _values.begin(); itr != _values.end(); ++itr ){
//           transform( itr->begin(), itr->end(), 
//	            (right._values.begin() + 
//		      distance( _values.begin(), itr ) )->begin(),
//	              itr->begin(),
//		      plus<float>() );
//      } // for itr
      // end of replacement
     _source += right._source;
     return *this;
}//operator+=

// substract another xmrg 
XMRG& XMRG::operator-=( const XMRG& right ){
     assert( abs( _cellSizeInM - right._cellSizeInM ) < 0.00001f );
     assert( _origin == right._origin );
     assert( _colrows == right._colrows );
     int col, row;
     not_equal_to< float > neq;
     for( row = 0; row < _colrows.second; row++){
        for( col = 0; col < _colrows.first; col++)
	{//for col
         if( neq( _values[row][col], _NO_DATA_VALUE ) )	
	 {//if
            if( neq( right._values[row][col], _NO_DATA_VALUE ) )
	           _values[row][col] -= right._values[row][col];
	 }//if
	 else
            if( neq( right._values[row][col], _NO_DATA_VALUE ) )
	           _values[row][col] = right._values[row][col];
	}//for col
     } // for row
      // this block can be replaced by above 
//      vector< vector<float> >::iterator itr;
//      for( itr = _values.begin(); itr != _values.end(); ++itr ){
//           transform( itr->begin(), itr->end(), 
//	            (right._values.begin() + 
//		      distance( _values.begin(), itr ) )->begin(),
//	              itr->begin(),
//		      plus<float>() );
//      } // for itr
      // end of replacement
     _source += right._source;
     return *this;
}//operator+=
	 
// sum two xmrgs
XMRG XMRG::operator+( const XMRG& op2 ){
     if( abs( _cellSizeInM - op2._cellSizeInM ) > 0.001f )
     {
       throw std::runtime_error( " different cell size for ""-"" "
                  " operation on XMRG!" ) ;
     }
     assert( _origin == op2._origin );
     assert( _colrows == op2._colrows );

     XMRG x( *this );

     int col, row;
     not_equal_to< float > neq;

     for( row = 0; row < _colrows.second; row++){
        for( col = 0; col < _colrows.first; col++)
	{//for col
            if( neq( op2._values[row][col], _NO_DATA_VALUE ) &&
                neq( x._values[row][col], _NO_DATA_VALUE) )	
	    {//if
	         x._values[row][col] += op2._values[row][col];
	    }//if
         }
     }
     x._source = _source + op2._source;

     return x;
} // operator+;

// sum two xmrgs
XMRG XMRG::operator-( const XMRG& op2 ){

     if( abs( _cellSizeInM - op2._cellSizeInM ) > 0.001f )
     {
       cerr << "left cell size =" << _cellSizeInM << endl;
       cerr << "right cell size =" << op2._cellSizeInM << endl;
       throw std::runtime_error( " different cell size for ""-"" "
                  " operation on XMRG!" ) ;
     }
     assert( _origin == op2._origin );
     assert( _colrows == op2._colrows );

     XMRG x( *this );

     int col, row;
     not_equal_to< float > neq;

     for( row = 0; row < _colrows.second; row++){
        for( col = 0; col < _colrows.first; col++)
	{//for col
            if( neq( op2._values[row][col], _NO_DATA_VALUE ) &&
                neq( x._values[row][col], _NO_DATA_VALUE) )	
	    {//if
	         x._values[row][col] -= op2._values[row][col];
	    }//if
         }
     }
     x._source = _source + op2._source;

     return x;
} // operator+;

// add a float value
XMRG& XMRG::operator+=( const float& right )
{//operator +=
     int col, row;
     not_equal_to< float > neq;
     for( row = 0; row < _colrows.second; row++){
        for( col = 0; col < _colrows.first; col++)
	{//for col
         if( neq( _values[row][col], _NO_DATA_VALUE ) )	
	 {//if
	           _values[row][col] += right;
	 }//if
	}//for col
     } // for row
     return *this;
}//operator +=

// substract a float value
XMRG& XMRG::operator-=( const float& right )
{//operator -=
     int col, row;
     not_equal_to< float > neq;
     for( row = 0; row < _colrows.second; row++){
        for( col = 0; col < _colrows.first; col++)
	{//for col
         if( neq( _values[row][col], _NO_DATA_VALUE ) )	
	 {//if
	           _values[row][col] -= right;
	 }//if
	}//for col
     } // for row
     return *this;
}//operator -=

// add a float value
XMRG XMRG::operator+( const float& right )
{//operator+
     int col, row;
     XMRG x( *this );
     not_equal_to< float > neq;
     for( row = 0; row < _colrows.second; row++){
        for( col = 0; col < _colrows.first; col++)
	{//for col
         if( neq( x[row][col], _NO_DATA_VALUE ) )	
	 {//if
	           x[row][col] += right;
	 }//if
	}//for col
     } // for row
     return x;
}//operator+

// substract a float value
XMRG XMRG::operator-( const float& right )
{//operator-
     int col, row;
     XMRG x( *this );
     not_equal_to< float > neq;
     for( row = 0; row < _colrows.second; row++){
        for( col = 0; col < _colrows.first; col++)
	{//for col
         if( neq( x[row][col], _NO_DATA_VALUE ) )	
	 {//if
	           x[row][col] -= right;
	 }//if
	}//for col
     } // for row
     return x;
}//operator-

// assign xmrgs
XMRG& XMRG::operator=( const XMRG& right ){
     _origin          = right._origin;
     _colrows         = right._colrows;
     _scaleFactor     = right._scaleFactor;
     _numberOfBytes   = right._numberOfBytes;
     _source          = right._source;
     _values          = right._values;
     _cellSizeInM     = right._cellSizeInM;
//     _time            = right._time;
     _NO_DATA_VALUE   = right._NO_DATA_VALUE;
     _unit            = right._unit;
     return *this;
} // operator=

vector< float > & XMRG::operator[]( const int&  row ){
	return _values[ row ];
}//operator[]

void XMRG::fillMissing(){
   int col, row, count;
   int up, down, left, right;
   float sum = .0;
   for( col = 0; col < _colrows.first; col++)
   {
     for( row = 0; row < _colrows.second; row++)
     {
       if ( equal_to< float >()( _values[ row ][ col ], 
                      static_cast< float >( _NO_DATA_VALUE ) ) )
       {
          sum = .0;
          int distance = 1;

          while ( true )
          {//while ( !foundNearest )

             up = row + distance;
             down = row - distance;
             left = col - distance;
             right = col + distance;

             //reach to end
             if ( up    >= _colrows.second &&
                  down  < 0                &&
                  right >= _colrows.first  && 
                  left  < 0 )
             {
                cerr << " Empty XMRG grid! Not filling! --- " 
                     << _source << endl;;
                return;
             }

             sum = .0;
             count = 0;

             //
             // missing value cell in the middle
             //
             if ( left >= 0                  &&
                  right < _colrows.first    &&
                  down >= 0                  &&
                  up    < _colrows.second   )
             {
               //
               //eastern side
               //
               for ( int y = down; y <= up; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ left ],
                              static_cast< float > ( _NO_DATA_VALUE ) ) )
                 {
                    count ++;
                    sum += _values[ y ][ left ]; 
                 }
               }  

               //
               //western side
               //
               for ( int y = down; y <= up; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ right ],
                              static_cast< float > ( _NO_DATA_VALUE ) ) )
                 {
                    count ++;
                    sum += _values[ y ][ right ]; 
                 }
               }  

               //
               //sourthern side
               //
               for ( int x = left + 1; x < right; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ down ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ down ][ x ]; 
                 }
               }  

               //
               //northern side
               //
               for ( int x = left + 1; x < right; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ up ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ up ][ x ]; 
                 }
               }  
             }
             //
             // lower left corner
             //
             else if ( left < 0               &&
                       down < 0               &&
                       right < _colrows.first &&
                       up    < _colrows.second )
             {
               //
               //western side
               //
               for ( int y = 0; y <= up; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ right ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ right ]; 
                 }
               }  
               //
               //northern side
               //
               for ( int x = 0; x < right; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ up ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ up ][ x ]; 
                 }
               }  
             }
             //
             // lower middle
             //
             else if ( left >= 0               &&
                       down < 0               &&
                       right < _colrows.first &&
                       up    < _colrows.second )
             {
               //
               //eastern side
               //
               for ( int y = 0; y <= up; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ left ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ left ]; 
                 }
               }  

               //
               //western side
               //
               for ( int y = 0; y <= up; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ right ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ right ]; 
                 }
               }  

               //
               //northern side
               //
               for ( int x = left + 1; x < right; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ up ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ up ][ x ]; 
                 }
               }  
             }
             //
             // lower right corner
             //
             else if ( left >= 0               &&
                       down < 0               &&
                       right >= _colrows.first &&
                       up    < _colrows.second )
             {
               //
               //eastern side
               //
               for ( int y = 0; y <= up; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ left ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ left ]; 
                 }
               }  

               //
               //northern side
               //
               for ( int x = left + 1; x < _colrows.first; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ up ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ up ][ x ]; 
                 }
               }  
             }

             //
             // right middle
             //
             else if ( left >= 0               &&
                       down >= 0               &&
                       right >= _colrows.first &&
                       up    < _colrows.second )
             {
               //
               //eastern side
               //
               for ( int y = down; y <= up; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ left ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ left ]; 
                 }
               }  

               //
               //northern side
               //
               for ( int x = left + 1; x < _colrows.first; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ up ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ up ][ x ]; 
                 }
               }  

               //
               //sourthern side
               //
               for ( int x = left + 1; x < _colrows.first; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ down ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ down ][ x ]; 
                 }
               }  
             }
             //
             // upper right corner
             //
             else if ( left >= 0               &&
                       down >= 0               &&
                       right >= _colrows.first &&
                       up    >= _colrows.second )
             {
               //
               //eastern side
               //
               for ( int y = down; y < _colrows.second; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ left ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ left ]; 
                 }
               }  

               //
               //southern side
               //
               for ( int x = left + 1; x < _colrows.first; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ down ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ down ][ x ]; 
                 }
               }  
             }
             //
             // upper middle
             //
             else if ( left >= 0               &&
                       down >= 0               &&
                       right < _colrows.first &&
                       up    >= _colrows.second )
             {
               //
               //eastern side
               //
               for ( int y = down; y < _colrows.second; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ left ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ left ]; 
                 }
               }  
               //
               //western side
               //
               for ( int y = down; y < _colrows.second; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ right ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ right ]; 
                 }
               }  
               //
               //southern side
               //
               for ( int x = left + 1; x < right; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ down ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ down ][ x ]; 
                 }
               }  
             }
             //
             // upper left corner
             //
             else if ( left < 0               &&
                       down >= 0               &&
                       right < _colrows.first &&
                       up    >= _colrows.second )
             {
               //
               //western side
               //
               for ( int y = down; y < _colrows.second; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ right ],
                             _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ right ]; 
                 }
               }  

               //
               //southern side
               //
               for ( int x = 0; x < right ; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ down ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ down ][ x ]; 
                 }
               }  
             }
             //
             // left middle
             //
             else if ( left < 0               &&
                       down >= 0               &&
                       right < _colrows.first &&
                       up    < _colrows.second )
             {
               //
               //western side
               //
               for ( int y = down; y <= up; y ++ )
               {
                 if ( not_equal_to< float >()( _values[ y ][ right ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ y ][ right ]; 
                 }
               }  
               //
               //southern side
               //
               for ( int x = 0; x < right; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ down ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ down ][ x ]; 
                 }
               }  
               //
               //northern side
               //
               for ( int x = 0; x < right; x ++ )
               {
                 if ( not_equal_to< float >()( _values[ up ][ x ],
                              _NO_DATA_VALUE ) )
                 {
                    count ++;
                    sum += _values[ up ][ x ]; 
                 }
               }  
             }
             else
             {
             //   throw std::runtime_error ( 
             //            "Wrong position of nodatavalue cell!" );
               if ( right < _colrows.first )
               {
                  if ( not_equal_to< float >()( _values[ row ][ right ],
                                           _NO_DATA_VALUE ) )
                  {
                      count ++;
                      sum += _values[ row ][ right ]; 
                  }
               }
               if ( left > 0 )
               {
                  if ( not_equal_to< float >()( _values[ row ][ left ],
                                           _NO_DATA_VALUE ) )
                  {
                      count ++;
                      sum += _values[ row ][ left ]; 
                  }
               }

               if ( up < _colrows.second )
               {
                  if ( not_equal_to< float >()( _values[ up ][ col ],
                                           _NO_DATA_VALUE ) )
                  {
                      count ++;
                      sum += _values[ up ][ col ]; 
                  }
               }
               if ( down > 0 )
               {
                  if ( not_equal_to< float >()( _values[ down ][ col ],
                                           _NO_DATA_VALUE ) )
                  {
                      count ++;
                      sum += _values[ down ][ col ]; 
                  }
               }

               if ( right < _colrows.first  &&
                    up < _colrows.second )
               {
                  if ( not_equal_to< float >()( _values[ up ][ right ],
                                           _NO_DATA_VALUE ) )
                  {
                      count ++;
                      sum += _values[ up ][ right ]; 
                  }
               }

               if ( left > 0 &&
                    up < _colrows.second )
               {
                  if ( not_equal_to< float >()( _values[ up ][ left ],
                                           _NO_DATA_VALUE ) )
                  {
                      count ++;
                      sum += _values[ up ][ left ]; 
                  }
               }

               if ( left > 0 && down > 0)
               {
                  if ( not_equal_to< float >()( _values[ down ][ left ],
                                           _NO_DATA_VALUE ) )
                  {
                      count ++;
                      sum += _values[ down ][ left ]; 
                  }
               }

               if ( right < _colrows.first && down > 0)
               {
                  if ( not_equal_to< float >()( _values[ down ][ right ],
                                           _NO_DATA_VALUE ) )
                  {
                      count ++;
                      sum += _values[ down ][ right ]; 
                  }
               }
             }

             if ( count > 0 )
             {
                break;
             }
             else
             {
               distance ++;
             }
              
          }//while ( true ) 

          if ( count > 0 )
          {
             _values[ row ][ col ] = sum / count;
          }
       }
     }//for( row =
  }//for col
}//fillMissing

void XMRG::fillMissing( int neg_val ){
       float** par_tmp = (float**)NULL;
       int col, row;

       par_tmp = new float*[ _colrows.second ];
       assert( par_tmp != NULL );

       for( row = 0; row < _colrows.second; row++ ){
	       par_tmp[ row ] = new float[ _colrows.first ];
	       assert( par_tmp[ row ] != NULL );
	       for( col = 0; col < _colrows.first; col ++ )
		       par_tmp[ row ][ col ] = _values[ row ][ col ];
       }//for row

       fill_miss2d( _colrows.first, _colrows.second, par_tmp, neg_val );

       for( row = 0; row < _colrows.second; row++ ){
	       for( col = 0; col < _colrows.first; col ++ )
		       _values[ row ][ col ] = par_tmp[ row ][ col ];
       }//for row

       for( row = 0; row < _colrows.second; row++ ){
	       delete[] par_tmp[ row ];
       }//for row
       delete[] par_tmp;

       return;
}//void XMRG::fillMissing()

//fill with constant value
void XMRG::fillConst( float v, int sf )
{//fillConst
     for( vector< vector< float > >::iterator itr = _values.begin();
          itr != _values.end(); ++itr ) 
     {//for itr
        fill( itr->begin(), itr->end(), v );
     }//for itr
     _scaleFactor = sf;
}//fillConst

//this is the wrapper to call WRITE_GRID2.f
void XMRG::toBinFile( const std::string& filename ){
       int npix = _colrows.first * _colrows.second;
       float* data = new float[ npix ];
       int row, col, index;
       int lftx = (int)_origin.x;
       int dny = (int)_origin.y;
       int rgtx = lftx + _colrows.first - 1;
       int upy = dny + _colrows.second - 1;
       int* colno = new int[ npix ];
       int* rowno = new int[ npix ];
       int k = 1;

       float cellSize = getCellSizeInHRAP();

       char binFile[ MAX_FILENAME_LENGTH ];
       strcpy( binFile, filename.c_str() );
       index = 0;
       for( row = 0; row < _colrows.second; row++ ){
	       for( col = 0; col < _colrows.first; col ++ ){
		     data[ index ] = _values[ row ][ col ] * _scaleFactor;
                     colno[ index ] = col;
		     rowno[ index ] = row;
		     index++;
	       }//for( col = 0; col < _colrows.first; col ++ )
       }//for row

       WRITE_GRID2( &lftx, &rgtx, &dny, &upy, 
    		    binFile,
		    data,
		    &npix,
		    &k,
		    colno, rowno,
		    &_scaleFactor,
		    &cellSize,
		    &_NO_DATA_VALUE );

       delete[] data;
       delete[] rowno;
       delete[] colno;

}//toBinFile

float XMRG::minMignitude() const
{//minMignitude
        float min = 3.4e38; //max possible float
	size_t i, j;
	float v;
	not_equal_to< float > neq;
	for( i = 0; i < _values.size(); i ++ )
	{//for i
	    for( j = 0; j < _values[ i ].size(); j ++ )
	    {//for j
		    v = fabs( _values[ i ][ j ] );
		    if(  v  < min && neq( _values[ i ][ j ], _NO_DATA_VALUE ) )
			    min = v; 
	    }//for j
	}//for j

	return min;
}//minMignitude

float XMRG::maxMignitude() const
{//maxMignitude
        float max = 3.4e-38; //min possible float
	size_t i, j;
	float v;
	not_equal_to< float > neq;
	for( i = 0; i < _values.size(); i ++ )
	{//for i
	    for( j = 0; j < _values[ i ].size(); j ++ )
	    {//for j
		    v = fabs( _values[ i ][ j ] );
		    if( v > max &&
		        neq( _values[ i ][ j ], _NO_DATA_VALUE ) )
			    max = v; 
	    }//for j
	}//for j

	return max;
}//maxMignitude

float XMRG::max() const 
{//max
       vector< vector<float > > tmp( _values );
       vector< float > _valuesWithoutNodata;

       for ( vector< vector<float > >::iterator itr = tmp.begin();
		       itr != tmp.end(); ++itr )
       {//for itr
	       remove_copy( itr->begin(), itr->end(), 
			    inserter( _valuesWithoutNodata,
				    _valuesWithoutNodata.end() ),
	   	            _NO_DATA_VALUE );

       }//for itr

       if ( _valuesWithoutNodata.empty() )
       {//if empty
	       return _NO_DATA_VALUE;
       }//if empty
       else
       {//else if
               return *max_element( _valuesWithoutNodata.begin(),
		            _valuesWithoutNodata.end() );
       }//else if
}//max

float XMRG::min() const
{//min
       vector< vector<float > > tmp( _values );
       vector< float > _valuesWithoutNodata;

       for ( vector< vector<float > >::iterator itr = tmp.begin();
		       itr != tmp.end(); ++itr )
       {//for itr
	       remove_copy( itr->begin(), itr->end(), 
			    inserter( _valuesWithoutNodata,
				    _valuesWithoutNodata.end() ),
	   	            _NO_DATA_VALUE );

       }//for itr

       if ( _valuesWithoutNodata.empty() )
       {//if empty
	       return _NO_DATA_VALUE;
       }//if empty
       else
       {//else if
               return *min_element( _valuesWithoutNodata.begin(),
		            _valuesWithoutNodata.end() );
       }//else if
}//min

float XMRG::noDataValue() const { return _NO_DATA_VALUE; }

void XMRG::setNoDataValue( float const& nodata ) { _NO_DATA_VALUE = nodata; }

//idig idefines how many digits to keep for smallest value
//       (after multiplied by scale factor).
//
void XMRG::resetScale( int idig )
{//resetScale
	float max = maxMignitude();
	float min = minMignitude();

	if ( max < abs( static_cast< float >( _NO_DATA_VALUE ) ) )  
	{//if
		max = abs( static_cast< float >(  _NO_DATA_VALUE ) );
	}//if

	if ( min > abs( static_cast< float >( _NO_DATA_VALUE ) ) )  
	{//if
		min = abs( static_cast< float >( _NO_DATA_VALUE ) );
	}//if

	int scaleFactorExp = (int)( std::log10( 32767. ) - std::log10( max ) );
	_numberOfBytes = 2;
	if( scaleFactorExp < 1 ||
			pow( 10.f, scaleFactorExp ) * min < 
				          std::pow( 10., idig - 1) )
	{//if
		_scaleFactor = 1;
		_numberOfBytes = 4;
	}//if

        if ( scaleFactorExp < 0 )
        {
           scaleFactorExp = 0;
        }
	_scaleFactor = static_cast< int >( std::pow( 10.f, scaleFactorExp ) );

}//resetScale

//smr 7/2007
//convert origin and cell size from stereographic to hrap
//       
//
void XMRG::stertohrap()
{//stertohrap
	cout << "xorign " << _origin.x << endl;
   cout << "yorigin " << _origin.y << endl;
   cout << "sizeinhrap " << getCellSizeInHRAP () << endl;
   _origin.x = (_origin.x/4762.5+401.0);
	_origin.y = (_origin.y/4762.5+1601.0);
   //_origin.x = (_origin.x/SIZEAT60+HRAPXOR);
	//_origin.y = (_origin.y/SIZEAT60+HRAPYOR);
    setCellSizeInHRAP( (getCellSizeInHRAP()/4762.5) );
    cout << "xorign " << _origin.x << endl;
   cout << "yorigin " << _origin.y << endl;
   cout << "sizeinhrap " << getCellSizeInHRAP () << endl;

}//stertohrap
//smr 7/2007

void XMRG::setNumberOfBytesPerValue( int nb ){ _numberOfBytes = nb; }

int XMRG::getNumberOfBytesPerValue( ){ return _numberOfBytes; }

//write to XMRG parameter format considering a smaller( real number) cell size
void XMRG::write( string filename )
{//write
     filename += ".gz";
     gzFile fp = gzopen( filename.c_str(), "wb");
     void *buffer = (void*)NULL;
     int numBytes ;
     float cellSize = getCellSizeInHRAP();
     short data2;
     float data;

     buffer = &numBytes;
     //first record, 16 X Y col row 16,  24 bytes totally 
     numBytes = 16; 
     gzwrite( fp, buffer, 4 );
     gzwrite( fp, (void*)&_origin.x, 4 );
     gzwrite( fp, (void*)&_origin.y, 4 );
     gzwrite( fp, (void*)&_colrows.first, 4 );
     gzwrite( fp, (void*)&_colrows.second, 4 );
     gzwrite( fp, buffer, 4 );

     //second record, 12 scale _numberOfBytes 
     numBytes = 12; 
     gzwrite( fp, buffer, 4 );
     gzwrite( fp, (void*)&_scaleFactor, 4 );
     gzwrite( fp, (void*)&_numberOfBytes, 4 );
     gzwrite( fp, (void*)&cellSize, 4 );
     gzwrite( fp, buffer, 4 );

     //data
     numBytes = _numberOfBytes * _colrows.first;
     if( _numberOfBytes == 2 )
     {//if _numberOfBytes
	  for( int row = 0; row < _colrows.second; row++ )
	  {//for row
             gzwrite( fp, buffer, 4 );
	     for( int col = 0; col < _colrows.first; col++ )
	     {//for col
		 data2 = static_cast< short >( _values[ row ][ col ] *
				                  _scaleFactor );
                 gzwrite( fp, (void*)&data2, 2 );
	     }//for col
             gzwrite( fp, buffer, 4 );
	  }//for row
                 
     }//if _numberOfBytes
     else if( _numberOfBytes == 4 )
     {// else if _numbrOfBytes
	  for( int row = 0; row < _colrows.second; row++ )
	  {//for row
             gzwrite( fp, buffer, 4 );
	     for( int col = 0; col < _colrows.first; col++ )
	     {//for col
		 data =  _values[ row ][ col ] * _scaleFactor;
                 gzwrite( fp, (void*)&data, 4 );
	     }//for col
             gzwrite( fp, buffer, 4 );
	  }//for row
     }//else if _numberOFbytes
     else
     {//else
	     cerr << " Warning: wrong num of bytes -- data not writting!" 
		     << endl;
     }//else 

     // cleaning up
     gzclose( fp );
}//write

//write to XMRG parameter format considering a smaller( real number) cell size
//and write the no data value too
void XMRG::writeWithNodatavalue( string filename, bool compress )
{//write
     gzFile fp = (gzFile)NULL;
     if ( !compress )
     {
       fp = gzopen( filename.c_str(), "wb");
       if ( fp == NULL )
       {
         throw std::runtime_error( "Could not open file: " + filename );
       }
       gzsetparams(fp, Z_NO_COMPRESSION, Z_DEFAULT_STRATEGY);
     }
     else
     {
       filename += ".gz";
       fp = gzopen( filename.c_str(), "wb");
       if ( fp == NULL )
       {
         throw std::runtime_error( "Could not open file: " + filename );
       }
     }

     void *buffer = (void*)NULL;
     int numBytes ;
     float cellSize = getCellSizeInHRAP();
     short data2;
     float data;

     buffer = &numBytes;
     //first record, 16 X Y col row 16,  24 bytes totally 
     numBytes = 16; 
     gzwrite( fp, buffer, 4 );
     gzwrite( fp, (void*)&_origin.x, 4 );
     gzwrite( fp, (void*)&_origin.y, 4 );
     gzwrite( fp, (void*)&_colrows.first, 4 );
     gzwrite( fp, (void*)&_colrows.second, 4 );
     gzwrite( fp, buffer, 4 );

     //second record, 16 scale _numberOfBytes 
     numBytes = 16; 
     gzwrite( fp, buffer, 4 );
     gzwrite( fp, (void*)&_scaleFactor, 4 );
     gzwrite( fp, (void*)&_numberOfBytes, 4 );
     gzwrite( fp, (void*)&cellSize, 4 );
     gzwrite( fp, (void*)&_NO_DATA_VALUE, 4 );
     gzwrite( fp, buffer, 4 );

     equal_to< float > eq;
     //data
     numBytes = _numberOfBytes * _colrows.first;
     if( _numberOfBytes == 2 )
     {//if _numberOfBytes
	  for( int row = 0; row < _colrows.second; row++ )
	  {//for row
             gzwrite( fp, buffer, 4 );
	     for( int col = 0; col < _colrows.first; col++ )
	     {//for col
/*                 if ( eq( _values[ row ][ col ], _NO_DATA_VALUE ) )
		 {//if
			 if ( static_cast< float > (
					 numeric_limits< short >::max() )
					 < fabs( _NO_DATA_VALUE ) )
			 {//if
				 _NO_DATA_VALUE = static_cast< float > (
                                         numeric_limits< short >::max() );
			 }//

                       data2 = static_cast< short >( round( _NO_DATA_VALUE ) );
		 }//if
		 else
		 {//else if */
		   data2 = static_cast< short >( round( _values[ row ][ col ] *
				                            _scaleFactor ) );
/*		 }//else if */
                 gzwrite( fp, (void*)&data2, 2 );
	     }//for col
             gzwrite( fp, buffer, 4 );
	  }//for row
                 
     }//if _numberOfBytes
     else if( _numberOfBytes == 4 )
     {// else if _numbrOfBytes
	  for( int row = 0; row < _colrows.second; row++ )
	  {//for row
             gzwrite( fp, buffer, 4 );
	     for( int col = 0; col < _colrows.first; col++ )
	     {//for col
/*                 if ( eq( _values[ row ][ col ], _NO_DATA_VALUE ) )
		 {//if
			 if ( numeric_limits< float >::max() 
					 < fabs( _NO_DATA_VALUE ) )
			 {//if
				 _NO_DATA_VALUE = 
                                         numeric_limits< float >::max();
			 }//

			 data = _NO_DATA_VALUE;
		 }//if
		 else
		 {//else if */
		   data =  _values[ row ][ col ] * _scaleFactor;
/*		 }//else if */
                 gzwrite( fp, (void*)&data, 4 );
	     }//for col
             gzwrite( fp, buffer, 4 );
	  }//for row
     }//else if _numberOFbytes
     else
     {//else
	     cerr << " Warning: wrong num of bytes -- data not writting!" 
		     << endl;
     }//else 

     gzsetparams(fp, Z_DEFAULT_COMPRESSION, Z_DEFAULT_STRATEGY);
     // cleaning up
     gzclose( fp );
}//writeWithNodatavalue

//write to the old XMRG parameter format (no cell size, without no data value)
void XMRG::toOldPar( string filename )
{//write
     filename += ".gz";
     gzFile fp = gzopen( filename.c_str(), "wb");
     if ( fp == NULL )
	     throw std::runtime_error( "Could not open file: " + filename );
     void *buffer = (void*)NULL;
     int numBytes ;
     short data2;
     float data;

     cerr << "_cellsize = " << _cellSizeInM << endl;
     if ( _cellSizeInM < 4762.49 || _cellSizeInM > 4762.51 )
     {
      throw std::runtime_error( "Cellsize is not one unit of HARP, use other "
                               "methods to output!" );
     }
     buffer = &numBytes;
     //first record, 16 X Y col row 16,  24 bytes totally 
     numBytes = 16; 
     gzwrite( fp, buffer, 4 );
     gzwrite( fp, (void*)&_origin.x, 4 );
     gzwrite( fp, (void*)&_origin.y, 4 );
     gzwrite( fp, (void*)&_colrows.first, 4 );
     gzwrite( fp, (void*)&_colrows.second, 4 );
     gzwrite( fp, buffer, 4 );

     //second record, 16 scale _numberOfBytes 
     numBytes = 8; 
     gzwrite( fp, buffer, 4 );
     gzwrite( fp, (void*)&_scaleFactor, 4 );
     gzwrite( fp, (void*)&_numberOfBytes, 4 );
     gzwrite( fp, buffer, 4 );

     equal_to< float > eq;
     //data
     numBytes = _numberOfBytes * _colrows.first;
     if( _numberOfBytes == 2 )
     {//if _numberOfBytes
	  for( int row = 0; row < _colrows.second; row++ )
	  {//for row
             gzwrite( fp, buffer, 4 );
	     for( int col = 0; col < _colrows.first; col++ )
	     {//for col
/*                 if ( eq( _values[ row ][ col ], _NO_DATA_VALUE ) )
		 {//if
			 if ( static_cast< float > (
					 numeric_limits< short >::max() )
					 < fabs( _NO_DATA_VALUE ) )
			 {//if
				 _NO_DATA_VALUE = static_cast< float > (
                                         numeric_limits< short >::max() );
			 }//

                       data2 = static_cast< short >( round( _NO_DATA_VALUE ) );
		 }//if
		 else
		 {//else if */
		   data2 = static_cast< short >( round(  _values[ row ][ col ] *
				                            _scaleFactor ) );
/*		 }//else if */
                 gzwrite( fp, (void*)&data2, 2 );
	     }//for col
             gzwrite( fp, buffer, 4 );
	  }//for row
                 
     }//if _numberOfBytes
     else if( _numberOfBytes == 4 )
     {// else if _numbrOfBytes
	  for( int row = 0; row < _colrows.second; row++ )
	  {//for row
             gzwrite( fp, buffer, 4 );
	     for( int col = 0; col < _colrows.first; col++ )
	     {//for col
/*                 if ( eq( _values[ row ][ col ], _NO_DATA_VALUE ) )
		 {//if
			 if ( numeric_limits< float >::max() 
					 < fabs( _NO_DATA_VALUE ) )
			 {//if
				 _NO_DATA_VALUE = 
                                        numeric_limits< float >::max();
			 }//

			 data = _NO_DATA_VALUE;
		 }//if
		 else
		 {//else if */
		   data =  _values[ row ][ col ] * _scaleFactor;
/*		 }//else if */
                 gzwrite( fp, (void*)&data, 4 );
	     }//for col
             gzwrite( fp, buffer, 4 );
	  }//for row
     }//else if _numberOFbytes
     else
     {//else
	     cerr << " Warning: wrong num of bytes -- data not writting!" 
		     << endl;
     }//else 

     // cleaning up
     gzclose( fp );
}//writeWithNodatavalue

//write to XMRG precipitation format withnot considering a smaller( real number)
// cell size
void XMRG::toXMRGBinary( string filename, bool bigendian )
{//toXMRGBinary
     filename += ".gz";
     gzFile fp = gzopen( filename.c_str(), "wb");
     void *buffer = (void*)NULL;
     int numBytes ;
     int tempNumber;
     short data2;
     void *buffer2 = (void*)NULL;

     bool swap = ( little_endian() && bigendian ) || 
                 ( big_endian() && !bigendian );
 
     buffer = &tempNumber;
     //first record, 16 X Y col row 16,  24 bytes totally 
     numBytes = 16; 
     tempNumber = numBytes;
     if( swap ) swap_4_bytes( buffer );
     gzwrite( fp, buffer, 4 );

     int x = static_cast< int >( _origin.x );
     int y = static_cast< int >( _origin.y );
     tempNumber = x;
     if( swap ) swap_4_bytes( buffer );
     gzwrite( fp, buffer, 4 );

     tempNumber = y;
     if( swap ) swap_4_bytes( buffer );
     gzwrite( fp, buffer, 4 );

     tempNumber = _colrows.first;
     if( swap ) swap_4_bytes( buffer );
     gzwrite( fp, buffer, 4 );

     tempNumber = _colrows.second;
     if( swap ) swap_4_bytes( buffer );
     gzwrite( fp, buffer, 4 );

     tempNumber = numBytes;
     if( swap ) swap_4_bytes( buffer );
     gzwrite( fp, buffer, 4 );

     //second record, 66 bytes, 
     //
     numBytes = 66; 
     tempNumber = numBytes;
     if( swap ) swap_4_bytes( buffer );
     gzwrite( fp, buffer, 4 );
     //
     //oper sys "HP" or "LX"
     //
     std::string opersys;
     if ( !bigendian )
     {
        opersys = "LX";
     } 
     else 
     {
        opersys = "HP";
     }
     gzwrite( fp, opersys.c_str(), 2 );
     //
     // user id
     //
     char userid[ 9 ];
     getlogin_r( userid, 9 );
     gzwrite( fp, userid, 8 );
     //
     // save date time
     // 
     std::string savedatetime( 20, ' ' );
     savedatetime.replace( 0, 19, 
         boost::posix_time::to_iso_extended_string( 
          boost::posix_time::second_clock::universal_time() ), 0, 19 );
     savedatetime.replace( 10, 1, " " );
     gzwrite( fp, savedatetime.c_str(), 20 );
     //
     // process flag 
     // AXA01 (for AsctoXmrg, Automatic, one hour)
     std::string processflag( 8, ' ' );
     processflag.replace( 0, 5, "AXA01" ); 
     gzwrite( fp,  processflag.c_str(), 8 );
     //
     // validate date
     //  
     std::string validatedate( 20, ' ' );
     fs::path xmrgfile( filename );
     std::string fileleaf = xmrgfile.leaf();
     //valide from 1900 to 2099
     //format for only mmddyyyyhh
     static const boost::regex e ("[[:print:]]*((?:0[1-9]|1[012])(?:0[1-9]|[12][0-9]|3[01])(?:19|20)\\d\\d(?:[01]\\d|2[0123]))[[:print:]]*\\.gz$");
     boost::smatch m;
     if ( boost::regex_match( fileleaf, m, e ) )
     {
        validatedate.replace(0, 4, m[1], 4, 4);
        validatedate.replace(4, 1, "-" );
        validatedate.replace(5, 2, m[1], 0, 2);
        validatedate.replace(7, 1, "-" );
        validatedate.replace(8, 2, m[1], 2, 2);
        validatedate.replace(11, 2, m[1], 8, 2);
        validatedate.replace(13, 6, ":00:00");
     } 
     else
     {
        validatedate.replace(0, 20, "not a validate date");
     }
     gzwrite( fp, validatedate.c_str(), 20 );
     //
     //maximum value
     //
     int maximumvalue = static_cast< int >( maxMignitude() );
     tempNumber = maximumvalue;
     if( swap ) swap_4_bytes( buffer );
     gzwrite( fp, buffer, 4 );
     //
     //AWIPS build number
     //
     void* real4buffer = (void*)NULL; 
     float versionnumber( 9.2f );
     real4buffer = &versionnumber;
     if( swap ) swap_4_bytes( real4buffer );
     gzwrite( fp, real4buffer, 4 );
 
//     char second_rec[] = "Generated by OHD Hydro Group Zhengtao";
//     gzwrite( fp, static_cast< void* >( second_rec ), 38 );
     tempNumber = numBytes;
     if( swap ) swap_4_bytes( buffer );
     gzwrite( fp, buffer, 4 );

     buffer2 = &data2;
     //data
     numBytes = sizeof( short ) * _colrows.first;
     tempNumber = numBytes;
     if( swap ) swap_4_bytes( buffer );
     equal_to< float > eq;
     for( int row = 0; row < _colrows.second; row++ )
	  {//for row
             gzwrite( fp, buffer, 4 );
	     for( int col = 0; col < _colrows.first; col++ )
	     {//for col
/*                 if( eq( _values[ row ][ col ], _NO_DATA_VALUE ) )
		 {//if 
			 data2 = static_cast< short > ( round( _NO_DATA_VALUE 
					             * 100.f ) );
		 }//if 
		 else
		 {//else */
		       data2 = static_cast< short >( 
                            round( _values[ row ][ col ] * 100.f ) );
/*		 }//else */

                 if( swap ) swap_2_bytes( buffer2 );

                 gzwrite( fp, buffer2, 2 );

	     }//for col
             gzwrite( fp, buffer, 4 );
     }//for row
                 
     // cleaning up
     gzclose( fp );

}//toXMRGBinary

//load arc/info ASCII  file
void XMRG::loadASC( const char* file )
{//loadASC
	//type defines
	typedef char                    char_t;
	typedef file_iterator<char_t>   iterator_t;
	typedef scanner<iterator_t>     scanner_t;
	typedef rule<scanner_t>         rule_t;

        float sizeInHrap;
	float nodata_value = -1.f;
	vector< float > values;

        // Create a file iterator for this file
        iterator_t first( file );

	assert( first );

	iterator_t last = first.make_end();

	real_parser< float, real_parser_policies< float > > float_p;

        rule_t r =
                   *blank_p >> as_lower_d[ "ncols" ] >> +blank_p >>
                           int_p[ assign_a( _colrows.first ) ] >> +space_p >>

                   *blank_p >> as_lower_d[ "nrows" ] >> +blank_p >>
                            int_p[ assign_a( _colrows.second ) ] >> +space_p >>

                   *blank_p >> as_lower_d[ "xllcorner" ] >> +blank_p >>
                              float_p[ assign_a( _origin.x ) ]  >> +space_p >>

                   *blank_p >> as_lower_d[ "yllcorner" ] >> +blank_p >>
                              float_p[ assign_a( _origin.y ) ] >> +space_p >>

                   *blank_p >> as_lower_d[ "cellsize" ] >> +blank_p >>
                              float_p[ assign_a( sizeInHrap ) ]  >> +space_p >>

                   *blank_p >> !( as_lower_d[ "nodata_value" ] >> +blank_p >>
                              float_p[ assign_a( nodata_value ) ] >> +space_p
                                ) >>

                   *blank_p >> float_p[ push_back_a( values ) ] 
		            >> *( +space_p >> float_p[ push_back_a( values ) ] )
                    >> +space_p ;
        // Parse
        parse_info <iterator_t> info = parse( first, last, r );

	assert( info.full && 
 	       static_cast< int >( values.size() ) ==
	        _colrows.first * _colrows.second );

	_source = string( file );

	this->setCellSizeInHRAP( sizeInHrap );

	_NO_DATA_VALUE = nodata_value;

	//resize _values to proper size
	_values.resize( _colrows.second );

//       This implementation doesn't work on GCC 4.1
//
//        for_each( _values.begin(), _values.end(), 
//			bind2nd( mem_fun_ref( &vector< float >::resize ),
//			         _colrows.first ) );
        //
        // to be compatible with GCC 4.1
        //
        for ( vector< vector< float > >::iterator itr = _values.begin();
              itr != _values.end(); ++itr )
        {
             itr->resize( _colrows.first );
        }

	for( int row = _colrows.second - 1; row >= 0 ; row-- )
	{//fo ir 	

		copy( values.begin() + ( row * _colrows.first ),
		      values.begin() + ( row * _colrows.first ) 
		                     + _colrows.first,
                //      _values[ row - ( _colrows.second - 1 ) ].begin() );
                      _values[ ( _colrows.second - 1 ) - row ].begin() );
	}//fo ir 	
	
        _scaleFactor       = 1;
        _numberOfBytes     = sizeof( float );

}//loadASC

bool XMRG::empty()
{
     return _values.empty();
}
