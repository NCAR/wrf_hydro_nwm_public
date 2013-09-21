#include<iostream>
#include<fstream>
#include<string>
#include "boost/program_options.hpp"
#include "XMRG.h"

using namespace std;
namespace po=boost::program_options;
int main(int argc, char** argv ){
    // Declare the supported options.
    po::options_description desc("Convert XMRG file to Arc/Info ASCII Raster"
                                 " file" );
    desc.add_options()
    ("help", "Print this message(optional)")
    ("input,i", po::value<string>(), "input XMRG filename")
    ("output,o", po::value<string>(), "output ASCII filename")
    ("proj,p", po::value<string>()->default_value( "HRAP" ),
                               "projection key word of the ASCII file,"
                               " can be \"ster\" or \"HRAP\", default = HRAP")
    ("nodata,n", po::value<float>()->default_value( -1.f ), 
                               "NODATAVALUE of the XMRG file"
                                  " default = -1.0" )
    ("fillmissing,m", 
	          po::value< bool >()->default_value( false ),
	       "whether fill missing valus,"
	       """true"" or ""false"","
	      " default is false")
    ;

    po::positional_options_description p;
    p.add("input", -1);

    po::variables_map vm;
    po::store(po::command_line_parser( argc, argv ).
                  options( desc ).positional( p ).run(), vm );

    po::notify(vm);    

    if ( argc < 2 || vm.count("help")) 
    {
       cout << desc << "\n";
       return 1;
    }

    if ( !vm.count( "input" ) )
    {
      cout << "Input XMRG file was not set.\n";
      return 1;
    }

    string outputfile;

    if ( !vm.count( "output" ) )
    {
      outputfile = vm[ "input" ].as< string >() + ".asc";
    }
    else
    {
      outputfile = vm[ "output" ].as< string >();
    }

    XMRG x( vm[ "input" ].as< string >(), false, false,
            vm[ "nodata" ].as< float >()  );

    if ( vm["fillmissing"].as< bool >() )
    {
       x.fillMissing();
    }

    if ( vm[ "proj" ].as< string >() == "ster" )
    {
	x.toASC( outputfile, "ster" );
    }
    else if ( vm[ "proj" ].as< string >() == "HRAP" )
    {
	x.toASC( outputfile, "HRAP" );
    }
    else
    {
       cout << "Wrong projection type!" << endl;
       return 1;
    }

    return 0;
   
}//main
