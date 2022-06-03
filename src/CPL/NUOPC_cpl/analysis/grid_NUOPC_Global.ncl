;--------------------------------------------------
; Output Grid on Global Map
;--------------------------------------------------

;--------------------------------------------------
;
; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;---Read netCDF file
  file_suffix = get_file_suffix(grid,0)
  print ("Grid File="+grid)
  print ("Output File="+file_suffix@fBase+"_CONUS.png")

  in   = addfile(grid,"r")

;---Start the graphics.
  wks = gsn_open_wks("png",file_suffix@fBase)      ; send graphics to PNG file

  res               = True                        ; plot mods desired

  res@gsnMaximize   = True                        ; maximize plot size
  res@gsnDraw       = False                       ; don't draw yet
  res@gsnFrame      = False                       ; don't advance frame
  res@tiMainString  = str_sub_str(file_suffix@fBase,"_"," ")

  res@pmTickMarkDisplayMode = "Always"            ; nice tickmark labels

;---Create map (don't draw it yet)
  map = gsn_csm_map_ce(wks,res)

;---Load coordinates.
  hgt       = in->lon_center(:,:)
  hgt@lat2d = in->lat_center(:,:)
  hgt@lon2d = in->lon_center(:,:)

;---Call function to attach lat/lon lines. Only use every third line.
  pres                   = True
  pres@gsnCoordsAsLines  = True                   ; Default is points
  gsn_coordinates(wks,map,hgt,pres)
end
