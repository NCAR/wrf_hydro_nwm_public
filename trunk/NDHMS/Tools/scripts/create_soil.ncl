load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"

begin

outfile = addfile("soil_properties_testgrid.nc","c")

static = addfile("wrfinput_d01.nc","r")
soiltyp = static->ISLTYP

filedims = dimsizes(static->SMOIS)
number_soil_layers = filedims(1)
dim_sn = filedims(2)
dim_we = filedims(3)

;;;;;;;;;;;;;;
; Define the table values
;
; Texture class: 
;           1        2        3        4        5        6        7        8        9         10       11       12      13       14     15       16        17      18       19  
;        ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  bvec = (/2.79    ,4.26    ,4.74    ,5.33    ,5.33    ,5.25    ,6.77    ,8.72    ,8.17    ,10.73   ,10.39   ,11.55   ,5.25    ,0.0   ,2.79    ,4.26    ,11.55   ,2.79    ,2.79     /) 
dryvec = (/0.010   ,0.028   ,0.047   ,0.084   ,0.084   ,0.066   ,0.067   ,0.120   ,0.103   ,0.100   ,0.126   ,0.138   ,0.066   ,0.0   ,0.006   ,0.028   ,0.030   ,0.006   ,0.01     /)
maxvec = (/0.339   ,0.421   ,0.434   ,0.476   ,0.476   ,0.439   ,0.404   ,0.464   ,0.465   ,0.406   ,0.468   ,0.468   ,0.439   ,1.0   ,0.20    ,0.421   ,0.468   ,0.200   ,0.339    /)
refvec = (/0.236   ,0.383   ,0.383   ,0.360   ,0.383   ,0.329   ,0.314   ,0.387   ,0.382   ,0.338   ,0.404   ,0.412   ,0.329   ,0.0   ,0.17    ,0.283   ,0.454   ,0.17    ,0.236    /)
wltvec = (/0.010   ,0.028   ,0.047   ,0.084   ,0.084   ,0.066   ,0.067   ,0.120   ,0.103   ,0.100   ,0.126   ,0.138   ,0.066   ,0.0   ,0.006   ,0.028   ,0.030   ,0.006   ,0.01     /)
 dkvec = (/4.660E-5,1.410E-5,5.230E-6,2.810E-6,2.810E-6,3.380E-6,4.450E-6,2.030E-6,2.450E-6,7.220E-6,1.340E-6,9.740E-7,3.380E-6,0.0   ,1.410E-4,1.410E-5,9.740E-7,1.410E-4,4.660E-5 /)
 dwvec = (/0.608E-6,0.514E-5,0.805E-5,0.239E-4,0.239E-4,0.143E-4,0.990E-5,0.237E-4,0.113E-4,0.187E-4,0.964E-5,0.112E-4,0.143E-4,0.0   ,0.136E-3,0.514E-5,0.112E-4,0.136E-3,0.608E-6 /)
psivec = (/0.069   ,0.036   ,0.141   ,0.759   ,0.759   ,0.355   ,0.135   ,0.617   ,0.263   ,0.098   ,0.324   ,0.468   ,0.355   ,0.0   ,0.069   ,0.036   ,0.468   ,0.069   ,0.069    /)
qtzvec = (/0.92    ,0.82    ,0.60    ,0.25    ,0.10    ,0.40    ,0.60    ,0.10    ,0.35    ,0.52    ,0.10    ,0.25    ,0.05    ,0.60  ,0.07    ,0.25    ,0.60    ,0.52    ,0.92     /)

 default_refdk  = 2.0E-6    ;  Reference Soil Conductivity
 default_refkdt = 1.0       ;  Soil Infiltration Parameter

vartmp3D = new((/1,number_soil_layers,dim_sn,dim_we/),"float")
vartmp3D!0 = "Time"
vartmp3D!1 = "soil_layers_stag"
vartmp3D!2 = "south_north"
vartmp3D!3 = "west_east"
 
vartmp2D = new((/1,dim_sn,dim_we/),"float")
vartmp2D!0 = "Time"
vartmp2D!1 = "south_north"
vartmp2D!2 = "west_east"
 
;;;;;;;;;;;;;;
; Clapp-Hornberger B exponent

if(False) then
  print("soil 0: "+num(soiltyp.lt.1))
  print("soil 1: "+num(soiltyp.eq.1))
  print("soil 2: "+num(soiltyp.eq.2))
  print("soil 3: "+num(soiltyp.eq.3))
  print("soil 4: "+num(soiltyp.eq.4))
  print("soil 5: "+num(soiltyp.eq.5))
  print("soil 6: "+num(soiltyp.eq.6))
  print("soil 7: "+num(soiltyp.eq.7))
  print("soil 8: "+num(soiltyp.eq.8))
  print("soil 9: "+num(soiltyp.eq.9))
  print("soil 10: "+num(soiltyp.eq.10))
  print("soil 11: "+num(soiltyp.eq.11))
  print("soil 12: "+num(soiltyp.eq.12))
  print("soil 13: "+num(soiltyp.eq.13))
  print("soil 14: "+num(soiltyp.eq.14))
  print("soil 15: "+num(soiltyp.eq.15))
  print("soil 16: "+num(soiltyp.eq.16))
  print("soil 17: "+num(soiltyp.eq.17))
  print("soil 18: "+num(soiltyp.eq.18))
  print("soil 19: "+num(soiltyp.eq.19))
  print("soil 20: "+num(soiltyp.gt.19))
end if

vartmp3D@description = "Clapp-Hornberger B exponent"
vartmp3D = -9999.

do isoil=1,19
do ilayer=0,number_soil_layers - 1
  vartmp3D(:,ilayer,:,:) = where(soiltyp.eq.isoil, bvec(isoil-1), vartmp3D(:,ilayer,:,:))
end do
end do

outfile->bexp = vartmp3D

;;;;;;;;;;;;;;
; SMC dry

vartmp3D@description = "Soil Moisture Limit: Dry"
vartmp3D = -9999.

do isoil=1,19
do ilayer=0,number_soil_layers - 1
  vartmp3D(:,ilayer,:,:) = where(soiltyp.eq.isoil, dryvec(isoil-1), vartmp3D(:,ilayer,:,:))
end do
end do

outfile->smcdry = vartmp3D

;;;;;;;;;;;;;;
; SMC max

vartmp3D@description = "Soil Moisture Limit: Max"
vartmp3D = -9999.

do isoil=1,19
do ilayer=0,number_soil_layers - 1
  vartmp3D(:,ilayer,:,:) = where(soiltyp.eq.isoil, maxvec(isoil-1), vartmp3D(:,ilayer,:,:))
end do
end do

outfile->smcmax = vartmp3D

;;;;;;;;;;;;;;
; SMC ref

vartmp3D@description = "Soil Moisture Limit: Reference"
vartmp3D = -9999.

do isoil=1,19
do ilayer=0,number_soil_layers - 1
  vartmp3D(:,ilayer,:,:) = where(soiltyp.eq.isoil, refvec(isoil-1), vartmp3D(:,ilayer,:,:))
end do
end do

outfile->smcref = vartmp3D

;;;;;;;;;;;;;;
; SMC wlt

vartmp3D@description = "Soil Moisture Limit: Wilt"
vartmp3D = -9999.

do isoil=1,19
do ilayer=0,number_soil_layers - 1
  vartmp3D(:,ilayer,:,:) = where(soiltyp.eq.isoil, wltvec(isoil-1), vartmp3D(:,ilayer,:,:))
end do
end do

outfile->smcwlt = vartmp3D

;;;;;;;;;;;;;;
; Dk SAT

vartmp3D@description = "Saturated Soil Conductivity"
vartmp3D = -9999.

do isoil=1,19
do ilayer=0,number_soil_layers - 1
  vartmp3D(:,ilayer,:,:) = where(soiltyp.eq.isoil, dkvec(isoil-1), vartmp3D(:,ilayer,:,:))
end do
end do

outfile->dksat = vartmp3D

;;;;;;;;;;;;;;
; Dw SAT

vartmp3D@description = "Saturated Soil Diffusivity"
vartmp3D = -9999.

do isoil=1,19
do ilayer=0,number_soil_layers - 1
  vartmp3D(:,ilayer,:,:) = where(soiltyp.eq.isoil, dwvec(isoil-1), vartmp3D(:,ilayer,:,:))
end do
end do

outfile->dwsat = vartmp3D

;;;;;;;;;;;;;;
; Psi SAT

vartmp3D@description = "Saturated Matric Potential"
vartmp3D = -9999.

do isoil=1,19
do ilayer=0,number_soil_layers - 1
  vartmp3D(:,ilayer,:,:) = where(soiltyp.eq.isoil, psivec(isoil-1), vartmp3D(:,ilayer,:,:))
end do
end do

outfile->psisat = vartmp3D

;;;;;;;;;;;;;;
; Quartz

vartmp3D@description = "Soil Quartz Content"
vartmp3D = -9999.

do isoil=1,19
do ilayer=0,number_soil_layers - 1
  vartmp3D(:,ilayer,:,:) = where(soiltyp.eq.isoil, qtzvec(isoil-1), vartmp3D(:,ilayer,:,:))
end do
end do

outfile->quartz = vartmp3D

;;;;;;;;;;;;;;
; RefDK

vartmp2D@description = "Reference Soil Conductivity"
vartmp2D = -9999.

vartmp2D = (/ default_refdk /)

outfile->refdk = vartmp2D

;;;;;;;;;;;;;;
; RefKDT

vartmp2D@description = "Soil Infiltration Parameter"
vartmp2D = -9999.

vartmp2D = (/ default_refkdt /)

outfile->refkdt = vartmp2D

end
