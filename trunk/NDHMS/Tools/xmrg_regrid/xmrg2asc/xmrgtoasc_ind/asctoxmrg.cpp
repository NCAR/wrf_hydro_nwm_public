/** File Name: main.cpp
  * Author   : Zhengtao Cui
  * Created on : 15/08/07
  * Description: A tool to covert Arc/Info ASCII Raster to XMRG file.
  */

#include<iostream>
#include<fstream>
#include<string>
#include "boost/program_options.hpp"
#include "boost/algorithm/string.hpp"
#include "boost/format.hpp"
#include "XMRG.h"

using namespace std;
namespace po=boost::program_options;
int main(int argc, char** argv ){

    // Declare the supported options.
    po::options_description desc("Convert Arc/Info ASCII Raster file" 
                                 " to XMRG file");
    desc.add_options()
    ("help", "Print this message(optional)")
    ("input,i", po::value<string>(), "input filename")
    ("output,o", po::value<string>(), "output filename")
    ("proj,p", po::value<string>()->default_value( "HRAP" ),
                               "projection key word,"
                               " can be \"ster\" or \"HRAP\", default = HRAP")
    ("digits,s", po::value<int>()->default_value( 2 ), 
                               "number of digits to keep for smallest value,"
                                  " default = 2" )
    ("format,f", po::value<string>()->default_value( "xmrg" ),
                               "Format of the binary file, can be \"xmrg\", "
                               "\"par\", or \"oldpar\", default is \"xmrg\", "
                        "\"xmrg\" will also set \"--digits=2\" automatically" )
   ;

    po::positional_options_description p;
    p.add("input", -1);

    po::variables_map vm;
    po::store(po::command_line_parser( argc, argv ).
                  options( desc ).positional( p ).run(), vm );
    po::notify(vm);    

//    HRAP< float > hrpoint;
//    hrpoint.setByLonLat( -80.004f, 42.057f );
//    cerr << hrpoint << endl;

//    HRAP< float > hrpoint( 367, 263);
//    cerr << "Lon = " <<  hrpoint.LonLat().first << endl;
//    cerr << "Lat = " <<  hrpoint.LonLat().second << endl;
////    HRAP< float > elsmp( 367, 263 );
//    HRAP< float > elsmp;
//    elsmp.setByLonLat( -80.004, 42.057);
 //   cerr << "Lon = " <<  elsmp.LonLat().first << endl;
//    cerr << "Lat = " <<  elsmp.LonLat().second << endl;
//    cerr << "HRAP = " <<  elsmp << endl;
//    cerr << "STER X = " << boost::format("%+10f") % elsmp.ster().first << endl;
//    cerr << "STER Y = " << boost::format("%+10f") % elsmp.ster().second << endl;


    if ( argc < 2 ||  vm.count("help")) 
    {
       cout << desc << "\n";
       return 1;
    }

    if ( !vm.count( "input" ) )
    {
      cout << "Input file was not set.\n";
      return 1;
    }
    else
    {
       if ( !boost::iends_with( vm[ "input" ].as< string >(), ".asc" ) )
       {
          cout << " input file name has to be ending with \".asc\"!" << endl;
          return 1;
       }
    }

    string outputfile;
    if ( !vm.count( "output" ) )
    {
       outputfile = vm[ "input" ].as< string >();
       boost::ierase_last( outputfile, ".asc" );
    }
    else
    {
       outputfile = vm[ "output" ].as< string >();
    }
   
    XMRG x( vm[ "input" ].as< string >() );
    x.resetScale( vm[ "digits" ].as< int >() );
    if ( vm[ "proj" ].as< string >() == "ster" )
    {
       x.stertohrap();
    }
    else if ( vm[ "proj" ].as< string >() != "HRAP" )
    {
      cout << "Wrong projection type.\n";
      return 1;
    }

    if ( vm[ "format" ].as< string >() == "xmrg" )
    {
      x.toXMRGBinary( outputfile, false );
    }
    else if ( vm[ "format" ].as< string >() == "par" )
    {
      x.writeWithNodatavalue( outputfile );
    }
    else if ( vm[ "format" ].as< string >() == "oldpar" )
    {
      x.toOldPar( outputfile );
    }
    else
    {
      cout << "Wrong xmrg type.\n";
      return 1;
    }
    return 0;
}//main
