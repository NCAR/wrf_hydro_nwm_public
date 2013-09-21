;*************************************************
; NDHMS_sub_domain_forcing_regridder.ncl
;
; Usage   : ncl 'srcfilename="wrfout_d01_2007*.nc"' NDHMS_sub_domain_forcing_regridder_ESMFregrid.ncl
;             srcfilename = filename pattern of the souce wrfout files.
;                           e.g., "wrfout_d01_2007-09-01_00_00_00.nc", "wrfout_d01_2007*.nc"
;                                 "wrfout_d01_2007-09-0[1-3].nc"
;
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/ut_string.ncl"
;load "./ESMF_regridding.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
;************************************************
begin
;************************************************
; read in netCDF files
;************************************************


; Read in lat/lon of source data to be regridded...(this is the original geogrid file...)
  xmrg_src_file = "xmrg_netcdf.nc"
  srcfile = addfile( xmrg_src_file ,"r")
  slon2d=srcfile->xlat
  slat2d=srcfile->xlon
  var2d = srcfile->pcp


; Read destination geogrid file to define final domain and grid size...
  dstfilename = "wrf_geo_em.nc"
  dstfile     = addfile( dstfilename ,"r")
  dlon3d=dstfile->XLONG_M
  dlat3d=dstfile->XLAT_M

  dlon2d=dlon3d(0,:,:)
  dlat2d=dlat3d(0,:,:)

  dims=dimsizes(dlat2d)

  ;---Options to pass to ESMF_regrid--------------------;
   opt                = True

   ;----- for generating weight file -----;
   opt@InterpMethod   = "conserve"
;   opt@SrcFileName    = "src_test.nc"
;   opt@DstFileName    = "dst_test.nc"
;   opt@WgtFileName    = "xmrgtoWRF_d02_weight_bilinear.nc"
;   opt@ForceOverwrite = True

   opt@SrcGridLat     = slat2d
   opt@SrcGridLon     = slon2d
   opt@DstGridLat     = dlat2d
   opt@DstGridLon     = dlon2d
   opt@SrcRegional    = True    ; Default is False (periodic coord). True = non-periodic
   opt@DstRegional    = True

   ;------------------------;

   opt@CopyVarAtts    = True
   opt@CopyVarCoords  = False

  ;---Debug information
    opt@PrintTimings   = True
    opt@Debug          = False

  ; handle missing value
    opt@Mask2D = where(var2d .ge. 0, 1, 0)
  ;-----------------------------------------------------;
  ;    pcp = ESMF_regrid( var2d, opt)

    wgtFile     = "xmrg2WRF_weight_file.nc"
    pcp = ESMF_regrid_with_weights(var2d,wgtFile,opt)

; end Wei Yu
     ncfile = "xmrg2wrf_out.nc"
     ncdf= addfile(ncfile,"c")
     ncdf->lat = dlat2d   ;output lat
     ncdf->lon = dlon2d   ;output lon
     ncdf->pcp = pcp      ; output pcp
end
