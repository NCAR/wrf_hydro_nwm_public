#!/bin/csh -f
if(! -e asc) mkdir asc
foreach file(xmrg*z)
  ./xmrgtoasc $file
  mv *.asc asc
end
