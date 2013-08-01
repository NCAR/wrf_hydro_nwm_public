;----------------------------------------------------------------------
; ESMF_genWeight_for_WRF2NDHMS.ncl
;
; This is a file for use with ESMF regridding. This is script generates
; weight file (nc) for regridding the WRF04km grids to NDHMS grid space.
; Both grid spaces are curvliniar. Method used for regridding is 
; "conserve".
;
; There are four main steps to regrid data from one grid to another:
;
;   1. Generate a description file (SCRIP or ESMF) for the source grid.
;
;   2. Generate a description file (SCRIP or ESMF) for the destination
;      grid.
;
;   3. Generate the weights file, using the source and destination
;      files created in #1 and #2.
;
;
;----------------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;load "./ESMF_regridding.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
;----------------------------------------------------------------------
begin
;---Input file that contains lat/lon grid of source grid
   ;srcFileName = "/glade/proj2/ral/RHAP/asd000/HW2010.2/geo_em.d01-36km.nc"
    srcFileName = "xmrg_netcdf.nc"


;---Input file that contains lat/lon grid of destination grid, if applicable.
    dstFileName = "wrf_geo_em.nc"


;
; Output (and input) files for grid descriptions and weight file
; These can be whatever names you want.
;
   ;srcGridName = "src_SCRIP_conserve.nc"     ; "src_ESMF.nc"
   ;dstGridName = "dst_SCRIP_conserve.nc"     ; "dst_ESMF.nc"
   ;wgtFileName = "WRF04KM_to_NDHMS_weight_conserve.nc"   ; this is just an example
    srcGridName = "src_SCRIP_bilinear.nc"     ; "src_ESMF.nc"
    dstGridName = "dst_SCRIP_bilinear.nc"     ; "dst_ESMF.nc"
    wgtFileName = "xmrg2WRF_weight_file.nc"   ; this is just an example

;---Set to True if you want to skip any of these steps
    SKIP_SRC_GEN = False
    SKIP_DST_GEN = False
    SKIP_WGT_GEN = False

;----------------------------------------------------------------------
; 1. Generate a description file (SCRIP or ESMF) for the source grid.
;----------------------------------------------------------------------
    sfile = addfile(srcFileName,"r")
   ;x     = sfile->X            ; You may need to subscript this.

    if(.not.SKIP_SRC_GEN) then
      Opt                = True
      Opt@ForceOverwrite = True
      Opt@PrintTimings   = True
      Opt@Title          = "XMRG Grid"   ; An example

;---If we have 1D lat/lon coordinate arrays.
;     rectilinear_to_SCRIP(srcGridName,x&lat,x&lon,Opt)

;---If we have 2D lat/lon arrays.
       ;curvilinear_to_SCRIP(srcGridName,sfile->XLAT_M(0,:,:),sfile->XLONG_M(0,:,:),Opt)
       curvilinear_to_SCRIP(srcGridName,sfile->xlat,sfile->xlon,Opt)

;---If we have unstructured data (for example, X, Y, Z, data)
;     unstructured_to_ESMF(srcGridName,sfile->xlat,sfile->xlon,Opt)

;---Clean up
      delete(Opt)
   end if

;----------------------------------------------------------------------
; 2. Generate a description file (SCRIP or ESMF) for the destination
;    grid.
;----------------------------------------------------------------------

;---If applicable, open file that contains destination grid.
    dfile = addfile(dstFileName,"r")

    if(.not.SKIP_DST_GEN) then
      Opt                = True
      Opt@ForceOverWrite = True
      Opt@PrintTimings   = True

;---If we want to generate the destination grid from scratch
;     latlon_to_SCRIP(dstGridName,"5x5",Opt)

;---If we have 1D lat/lon coordinate arrays.
;     rectilinear_to_SCRIP(dstGridName,dfile->lat,dfile->lon,Opt)

;---If we have 2D lat/lon arrays.
      curvilinear_to_SCRIP(dstGridName,dfile->XLAT_M(0,:,:),dfile->XLONG_M(0,:,:),Opt)

;---If we have unstructured data (for example, X, Y, Z, data)
;     unstructured_to_ESMF(dstGridName,dfile->lat,dfile->lon,Opt)

;---Clean up
      delete(Opt)
    end if

;----------------------------------------------------------------------
; 3. Generate the weights file, using the source and destination
;    files created in #1 and #2.
;----------------------------------------------------------------------
    if(.not.SKIP_WGT_GEN) then
      Opt                      = True
      Opt@InterpMethod         = "bilinear"  ; "patch", "conserve"
      Opt@ForceOverWrite       = True
      Opt@PrintTimings         = True


      ESMF_regrid_gen_weights(srcGridName,dstGridName,wgtFileName,Opt)

;---Clean up
      delete(Opt)
    end if


end
