      program testlsm
c
c ------------ simple code to test lsm model
c
      parameter (r_gas=287.04, cp=1004.5, rl=2.5e+6, r_cp=r_gas/cp)
      parameter (sigma=5.67e-8)
c
      REAL*4        lsn         ! local solar noon (standard time)
      REAL*4        xcoor       ! X-coordinate in real numbers
      REAL*4        ycoor       ! Y-coordinate in real numbers
      INTEGER*2     ltm         ! local time meridian
c
      integer min
      integer jday
      integer hr
c
      COMMON /iheader/lsn,ltm
      COMMON /xheader/xcoor,ycoor,elev
c
      real sldpth(4), stc(4), smc(4), sh2o(4)
      real a_stc(4), a_smc(4), a_sh2o(4)
c
      data sldpth /0.05,0.20,0.60,1.00/
c
c ------- day 193 little washita 11:30am
      jday_beg = 193
      data stc    /298.227,297.680, 297.190,295.210/
      data smc    /0.2653,0.3270,0.3723,0.3366/
      data sh2o   /0.2653,0.3270,0.3723,0.3366/
c
c ------- horizontally averaged case
c     jday_beg = 202
c     data stc    /310.9785,303.2265,300.101,296.6095/
c     data smc    /0.2663835,0.3166285,0.334938,0.34099/
c     data sh2o   /0.2663835,0.3166285,0.334938,0.34099/
c
c ------- 7days from spin up of day 193
c     jday_beg = 202
c     data stc /0.310917E+03,0.303405E+03,0.300584E+03,0.296606E+03/
c     data smc /0.275518E+00,0.304518E+00,0.330153E+00,0.342163E+00/
c     data sh2o/0.275518E+00,0.304518E+00,0.330153E+00,0.342163E+00/
c
c ------- time average of noontime after spinup from day 193
c     jday_beg = 202
c     data stc /0.310990E+03,0.303406E+03,0.300561E+03,0.296668E+03/
c     data smc /0.271252E+00,0.307852E+00,0.330841E+00,0.341791E+00/
c     data sh2o/0.271252E+00,0.307852E+00,0.330841E+00,0.341791E+00/
c
c ------- time average of 11am-2pm after spinup from day 193
c     jday_beg = 202
c     data stc /0.311643E+03,0.303794E+03,0.300569E+03,0.296673E+03/
c     data smc /0.269827E+00,0.307796E+00,0.330796E+00,0.341774E+00/
c     data sh2o/0.269827E+00,0.307796E+00,0.330796E+00,0.341774E+00/
c
c ------- time average of entire 14day period following spinup
c     jday_beg = 202
c     data stc /0.305299E+03,0.304050E+03,0.300616E+03,0.296673E+03/
c     data smc /0.268731E+00,0.307752E+00,0.330805E+00,0.341772E+00/
c     data sh2o/0.268731E+00,0.307752E+00,0.330805E+00,0.341772E+00/

c
      open (13,file='soil_t.ieee',form='unformatted',
     +      access='direct',recl=5*4)
      open (14,file='soil_m.ieee',form='unformatted',
     +      access='direct',recl=5*4)
      open (15,file='soil_flx.ieee',form='unformatted',
     +      access='direct',recl=4*4)
c
c     t2        = 301.7
c     t1        = 312.3
c     q2        = 0.756477E-02
c     q1        = 0.892356E-02
c
      t2        = 300.0
      t1        = 300.0
      q2        = 0.001
      q1        = 0.001
c
c ------------------------------------------------------------------------
c
c ---- all the following should remain the same between the three cases
c
      dt        = 1800.0	! seconds
      z1        = 10.0		! height of first atm layer
      nsoil     = 4		! number of soil layers
c
      ice       = 0
      prcp      = 0.0
      snoalb    = 0.0
      ptu_i     = 0.0
      ivegtyp   = 7
      isoiltyp  = 2
      islptyp   = 1
      shdfac    = 0.0
      tbot      = 294.762	! K
      sneqv     = 0.0
      snowh     = 0.0
      cmc_i     = 0.0
      p_ref     = 1013.0
      p_lev2    = 976.
      czil      = 0.1
      albedo    = 0.2
      alb       = 0.2
      soldn     = 700.0
c
      sfctmp    = t2/(p_ref/p_lev2)**r_cp
      r_lwdn    = sigma*sfctmp**4
      sfcprs    = p_lev2*100.0
      wind_m    = 0.00
      grav      = 9.81
      t00       = 300.0
      batag     = grav/t00
      zo        = 0.10143
      zi_pbl    = 1000.0
c
      xcoor      = -98	! longitude
      ltm        =  90	! local time meridian (mult of 15deg)
      ycoor      =  35	! latitude
      solar_cnst = 1370.! latitude
      transmis   = 0.6  ! latitude
c
      do is = 1,nsoil
         a_stc(is)  = 0.
         a_smc(is)  = 0.
         a_sh2o(is) = 0.
      enddo
      a_hflx = 0.
      a_eflx = 0.
      a_sflx = 0.
c
      it = 0
      icnt = 0
      jcnt = 0
c
c ------ run for what days?
c
      do jday = jday_beg,205
c
c ------ correct for starting at different times of day
c
         if (jday_beg .eq. 193 .and. jday .eq. 193) then
            ih_init = 11
            im_init = 2
         elseif (jday_beg .eq. 202 .and. jday .eq. 202) then
            ih_init = 12
            im_init = 1
         else
            ih_init = 1
            im_init = 1
         endif
c
c ------ run for a full day
c
         do ih = ih_init,24
            hr = float(ih-1)
            do im = im_init,2
              it = it + 1
              min = 30.*float(im-1) 
              call sunpos(jday,hr,min,szenrad)
              scos = cos(szenrad)
              if (scos .lt. 0.) scos = 0.
              soldn = solar_cnst * transmis * scos
c
              t1v       = t1*(1.0 + 0.61*q1)
              t2v       = t2*(1.0 + 0.61*q2)
c
              call sfcdif(batag,z1,zo,zi_pbl,
     +                  t1v,t2v,wind_m,czil,cd,ch,
     +                  rlmo_inv,ustar)
c             write(6,1000) cd, ch, ustar, 1.0/rlmo_inv
 1000         format(' cd = ',e15.6,' ch = ',e15.6,/,
     +               ' ustar = ',e15.6,' L = ',e15.6)
c
              call qdatap1(sfctmp,sfcprs,q2sat)
              if (q2 .lt. 0.1e-5) q2 = 0.1e-5
              if (q2 .ge.  q2sat) q2 = q2sat*0.99
              t2v    = sfctmp*(1.0 + 0.61*q2)
c
c ---------- calculate slope of sat specific humidity curve for penman: dqsdt2
c
              dqsdt2 = dqsdt (sfctmp, sfcprs)
c
              call sflx(ice, dt, z1, nsoil, sldpth(1),
     +          r_lwdn, soldn, sfcprs, prcp,
     +          sfctmp, t2, q2, q2sat, dqsdt2,
     +          ivegtyp,isoiltyp,islopetyp,
     +          shdfac, ptu, tbot, alb, snoalb,
     +          wind_m, cmc, t1,
     +          stc(1), smc(1), sh2o(1),
     +          snowh, sneqv, ch,
     +          etp, eta,h, s,
     +          runoff1, runoff2, q1, snmax, albedo,
     +          soil_wt, soil_mt)
c
              r_lwup    = sigma*t1**4
              fnet      = soldn*(1.0 - albedo) + r_lwdn - r_lwup
c
c             write(6,6000) it, h, eta, s, fnet, h+eta-s 
 6000         format(i6,' h = ',e15.6,' eta = ',e15.6,' s = ',e15.6,/,
     +       ' input = ',e15.6,' h+eta-s = ',e15.6)
c             write(6,6100) t1, q1
 6100         format(' t1 = ',e15.6,' q1 = ',e15.6)
c
c             if (jday .gt. 196) then
c             icnt = icnt+1
c             do is = 1,nsoil
c                a_stc(is)  = a_stc(is) + stc(is)
c                a_smc(is)  = a_smc(is) + smc(is)
c                a_sh2o(is) = a_sh2o(is) + sh2o(is)
c             enddo
c             endif
c
c
c             if (ih .ge. 11 .and. ih .le. 14) then
              if (ih .eq. 12 .and. im .eq. 1) then

              if (h .gt. 365.0 .and. h .lt. 375.0) then
                 write(6,2345)jday,ih,im,it,h,eta,s
 2345            format(4i6,/,
     +                  ' hflx = ',f15.6,/,
     +                  ' eflx = ',f15.6,/,
     +                  ' sflx = ',f15.6)
                 write(6,2346)
     +               (stc(is),is=1,nsoil),(smc(is),is=1,nsoil),
     +               (sh2o(is),is=1,nsoil)
 2346            format(
     + ('      data stc_i    /',f7.3,',',f7.3,',',f7.3,',',f7.3,'/'),/,
     + ('      data smc_i    /',f8.6,',',f8.6,',',f8.6,',',f8.6,'/'),/,
     + ('      data sh2o_i   /',f8.6,',',f8.6,',',f8.6,',',f8.6,'/'))
              endif
c
                 if (jday .gt. 197) then
c                if (jday .gt. 196) then
                 icnt = icnt+1
                 do is = 1,nsoil
                    a_stc(is)  = a_stc(is) + stc(is)
                    a_smc(is)  = a_smc(is) + smc(is)
                    a_sh2o(is) = a_sh2o(is) + sh2o(is)
                 enddo
                 a_hflx = a_hflx + h
                 a_eflx = a_eflx + eta
                 a_sflx = a_sflx + s
                 endif
c
                 
                 if (jday .eq. 197) then
                 write(6,6200) jday,ih,im,it,
     +               (stc(is),is=1,nsoil),(smc(is),is=1,nsoil),
     +               (sh2o(is),is=1,nsoil),h,eta,s
 6200        format(4i6,/,' soil t ',5x,' soil m ',5x,' % liquid ',/,
     + ('      data stc_w    /',f7.3,',',f7.3,',',f7.3,',',f7.3,'/'),/,
     + ('      data smc_w    /',f8.6,',',f8.6,',',f8.6,',',f8.6,'/'),/,
     + ('      data sh2o_w   /',f8.6,',',f8.6,',',f8.6,',',f8.6,'/'),/,
     + ('      data hflx     /',f15.6,'/'),/,
     + ('      data eflx     /',f15.6,'/'),/,
     + ('      data sflx     /',f15.6,'/'))
                 endif
                 if (jday .eq. 204) then
                 write(6,6210) jday,ih,im,it,
     +               (stc(is),is=1,nsoil),(smc(is),is=1,nsoil),
     +               (sh2o(is),is=1,nsoil),h,eta,s
 6210        format(4i6,/,' soil t ',5x,' soil m ',5x,' % liquid ',/,
     + ('      data stc_d    /',f7.3,',',f7.3,',',f7.3,',',f7.3,'/'),/,
     + ('      data smc_d    /',f8.6,',',f8.6,',',f8.6,',',f8.6,'/'),/,
     + ('      data sh2o_d   /',f8.6,',',f8.6,',',f8.6,',',f8.6,'/'),/,
     + ('      data hflx     /',f15.6,'/'),/,
     + ('      data eflx     /',f15.6,'/'),/,
     + ('      data sflx     /',f15.6,'/'))
                 endif
c
              endif
c
              temp = (3600.*float(hr) + 60.*float(min))/86400.
              time = float(jday) 
     +            + (3600.*float(hr) + 60.*float(min))/86400.
c             write(*,*)it,float(jday),temp,time
c
              write(13) time,(stc(is),is=1,nsoil)
              write(14) time,(100.*smc(is),is=1,nsoil)
              write(15) time,h,eta,s
              jcnt=jcnt+1
c
            enddo
         enddo
      enddo
      write(*,*) 'num in file: ',jcnt
c
      do is = 1,nsoil
         a_stc(is)  = a_stc(is) / float(icnt)
         a_smc(is)  = a_smc(is) / float(icnt)
         a_sh2o(is) = a_sh2o(is)/ float(icnt)
      enddo
      a_hflx = a_hflx / float(icnt)
      a_eflx = a_eflx / float(icnt)
      a_sflx = a_sflx / float(icnt)
c
      write(6,6300) it,(a_stc(is),a_smc(is),a_sh2o(is),is=1,nsoil)
 6300 format(i4,/,' a_soil t ',5x,' a_soil m ',5x,'a_ % liquid ',/,
     +                (3e15.6))
c
                 write(6,6220) 
     +               (a_stc(is),is=1,nsoil),(a_smc(is),is=1,nsoil),
     +               (a_sh2o(is),is=1,nsoil),a_hflx,a_eflx,a_sflx
 6220            format(' soil t ',5x,' soil m ',5x,' % liquid ',/,
     + ('      data stc_i    /',f7.3,',',f7.3,',',f7.3,',',f7.3,'/'),/,
     + ('      data smc_i    /',f8.6,',',f8.6,',',f8.6,',',f8.6,'/'),/,
     + ('      data sh2o_i   /',f8.6,',',f8.6,',',f8.6,',',f8.6,'/'),/,
     + ('      data a_hflx   /',f15.6,'/'),/,
     + ('      data a_eflx   /',f15.6,'/'),/,
     + ('      data a_sflx   /',f15.6,'/'))
c
      stop
      end
c
* ==================================================================== *
      subroutine sunpos(jday,hr,min,szenrad)

*   PROGRAM SUNPOS calculates the position of the sun as azimuth, al-  *
*   titude, declination. It also lets you calculate the "mass" of the  *
*   atmosphere for a given solar altitud angle, and enables to calcu-  *
*   late the attenuation of the solar beam due to the specific solar   *
*   altitude for a given atmospheric transmittivity.                   *
*** currently there is not every feature implemented (no transmittance etc...***


*
* common header
*
      REAL*4        lsn         ! local solar noon (standard time)
      REAL*4        xcoor       ! X-coordinate in real numbers
      REAL*4        ycoor       ! Y-coordinate in real numbers
      INTEGER*2     elev        ! station's elevation (in meters)
      INTEGER*2     ltm         ! local time meridian
*
      COMMON /iheader/lsn,ltm
      COMMON /xheader/xcoor,ycoor,elev
*
* common time
*
      INTEGER*2     mth         ! month
      INTEGER*2     day         ! day
      INTEGER*2     jday        ! Julian Day
      INTEGER*2     hr          ! hour (local standard time)
      INTEGER*2     min         ! minute (local standard time)
      INTEGER*2     hangle      ! hourangle (local standard time)   
*
c     COMMON /time/mth,day,jday,hr,hangle
*
* common solar
*
      REAL*4        pi          ! PI
      REAL*4        d2r         ! degree to radians conversion
      REAL*4        r2d         ! radians to degree conversion
      REAL*4        mass        ! mass of the atmosphere for given solaralt
      REAL*4        decrad      ! declination of the sun
      REAL*4        latrad      ! latitude of site in radians
      REAL*4        saltrad     ! solar altitude in radians
      REAL*4        saltdeg     ! solar altitude in degrees
      REAL*4        szenrad     ! solar zenith angle in radians
      REAL*4        szendeg     ! solar zenith angle in degrees
      REAL*4        sazirad     ! solar azimuth angle in radians
      REAL*4        sazideg     ! solar azimuth angle in degrees
      REAL*4        tbbase      ! base function of beam related transmittance
      REAL*4        tbfac       ! factor to multiply beam transmittance function
      REAL*4        basemax     ! maximum base function for vertical sun pos.
*
c     COMMON /solar/pi,d2r,r2d,mass,decrad,latrad,saltrad,saltdeg,
c    +              szenrad,szendeg,sazirad,sazideg,tbbase,
c    +              tbfac,basemax
* 
* local variables
*
      REAL*4        ha          ! hour (floating from hr & min)
      REAL*4        harad       ! hourangle in radians
      REAL*4        decdeg      ! declination in degrees

* -------------------------------------------------------------------- *
*
      pi      = 3.14159265358979
      d2r     = pi/180.0
      r2d     = 1./d2r
c     lsn     = 12.0+((FLOAT(ltm)-xcoor)/15.0)
      lsn     = ((FLOAT(ltm)-xcoor)/15.0)
      latrad  = ycoor*d2r
      decrad  = 23.45*d2r*sin(d2r*360.*(284.+FLOAT(jday))/365.)
      decdeg  = decrad*r2d
*
      ha      = FLOAT(hr)+(FLOAT(min)/60.0) !convert to floating
      hangle  = (lsn-ha)*60.0               !solrad is given for the hour preceeding the time given
      harad   = hangle*0.0043633            !convert hangle (in minutes) into radians
*                                           !This is the same as multiplying the #hrs by 15 (deg./hr),
*                                           !and convert d2r (*0.017453292)
*
      saltrad = asin((sin(latrad)*sin(decrad))+(cos(latrad)*cos(decrad)
     +               *cos(harad)))
      saltdeg = saltrad * r2d
      sazirad = asin(cos(decrad)*sin(harad)/cos(saltrad))
      sazideg = sazirad * r2d
      
      IF (saltdeg.LT.0.0 .OR. saltrad.GT.180.0) THEN    ! sun is below horizon
         saltdeg = 0.0
         saltrad = 0.0
         szendeg = 90.0
         szenrad = 90.0*d2r
         mass    = 1229**.5             ! if solaralt=0 -> sin(0)=0
      ELSE
         szendeg = 90.-saltdeg
         szenrad = szendeg*d2r
         mass = (1229.0+(614.0*sin(saltrad))**2)**.5-(614.*sin(saltrad))
      ENDIF
      tbbase     = exp(-.65*mass)+exp(-.09*mass)
      return
      END
*
* ==================================================================== *


      subroutine qdatap1(t,p,qs) 
      parameter (eps=0.622 )
c
      es = esat(t)
      qs = 0.622 * es /(p - (1.-0.622)*es)
c
      return
      end
      function esat(t) 
      implicit none
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc  purpose:  to calculate values of sat. vapor pressure (e)
cc            substitutes look-up tables associated with the data
cc            block  /vapprs/ .
cc            formulas and constants from rogers and yau, 1989.
cc
cc                         added by pablo j. grunmann, 7/9/97.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real lw
      real t
      real esat
c
ck    real eps
ck    real cp
ck    real cv
ck    real cvv
      real cpv
      real rv
      real cw
      real eso
      real to
c
ck    parameter (eps = 0.622)
ck    parameter  (cp = 1005.)
ck    parameter  (cv = 718.)
ck    parameter (cvv = 1410.)
      parameter (cpv = 1870.)
      parameter  (rv = 461.5)
      parameter  (cw = 4187.)
      parameter (eso = 611.2)
      parameter  (to = 273.15)      
c 
c     about the parameters:
c      
c     eps ---------- water - dry air molecular mass ratio, epsilon
c      
c   values for specific heat capacity and individual gas constants 
c   in [joules/(kg*kelvin)] units.
c
c     dry air: 
c             cp, cv
c     water vapor:
c                 cvv = 1410. 
c                 cpv = 1870.
c                 rv  =  461.5
c     liquid water:
c                  cw = 4187.
c
c     eso = es(to) = sat. vapor pressure (in pascal) at t=to
c      to = 273.15
c_______________________________________________________________________
c
c     clausius-clapeyron: des/dt = l*es/(rv*t**2)
c     
c    
          lw = 2.501000d6 - ( cw - cpv ) * ( t - to )
          esat = eso*exp (lw*(1/to - 1/t)/rv)  
c
          return
          end 
      function dqsdt ( sfctmp, sfcprs )
      implicit none
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to retrieve the appropriate value of dqsdt (the change
cc    =======   of the saturation mixing ratio with respect to the 
cc              change in temperature) from: 
cc
cc               formulas introduced in new function dqs 
cc                                  (modified by pablo grunmann, 7/9/97).
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real sfctmp
      real sfcprs
      real dqs
      real dqsdt
c
      if ((sfctmp .ge. 173.0) .and. (sfctmp  .le.  373.0)) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       if the input sfc air temp is btwn 173 k and 373 k, use
c       function dqs to determine the slope of sat.mix ratio function
c                                 -adapted to use new dqs, 7/9/97.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        dqsdt = dqs (sfctmp) / sfcprs

      else

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       otherwise, set dqsdt equal to zero
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        dqsdt = 0.0
      endif
c
      return
      end
      function dqs (t) 
      implicit none
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc  purpose:  to calculate values of vapor pressure (e)
cc            and p * dqs/dt (p times chg in sat mxg ratio with respect
cc            to the chg in temp) in substitution to the look-up tables.
cc
cc            substitutes look-up tables associated with the data
cc            block  /chmxr/ .
cc
cc            formulas and constants from rogers and yau, 1989.
cc
cc                         added by pablo j. grunmann, 6/30/97.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real desdt
      real dqs
ck    real esd
      real lw
      real t
      real es
c
ck    real cp
ck    real cv
ck    real cvv
      real cpv
      real rv
      real cw
      real eps
      real eso
      real to
c
ck    parameter  (cp = 1005.)
ck    parameter  (cv = 718.)
ck    parameter (cvv = 1410.)
      parameter (cpv = 1870.)
      parameter  (rv = 461.5)
      parameter  (cw = 4187.)
      parameter (eps = 0.622)
      parameter (eso = 611.2)
      parameter  (to = 273.15)
c 
c     about the parameters:
c      
c     eps ---------- water - dry air molecular mass ratio, epsilon
c      
c   values for specific heat capacity and individual gas constants 
c   in [joules/(kg*kelvin)] units.
c
c     dry air: 
c             cp, cv
c     water vapor:
c                 cvv = 1410. 
c                 cpv = 1870.
c                 rv  =  461.5
c     liquid water:
c                  cw = 4187.
c
c     eso = es(t=273.15 k) = sat. vapor pressure (in pascal) at t=to
c      to = 273.15
c      
c     sat. mixing  ratio: qs ~= eps*es/p
c     clausius-clapeyron: des/dt = l*es/(rv*t**2)
c     @qs/@t =  (eps/p)*des/dt
c    
          lw = 2.501000e6 - ( cw - cpv ) * ( t - to )
          es = eso*exp (lw*(1/to - 1/t)/rv)  
          desdt = lw*es/(rv*t*t)
c
c      for insertion in dqsdt function: 
c      dqsdt = dqs/p , where dqs = eps*desdt  
c
          dqs = eps*desdt
c     
          return
          end
c
      subroutine sflx(ice,dt,z,nsoil,sldpth,
     i lwdn,soldn,sfcprs,prcp,sfctmp,th2,q2,q2sat,dqsdt2,
     i vegtyp,soiltyp,slopetyp,
     i shdfac,ptu,tbot,alb,snoalb,
     i sfcspd,
     2 cmc,t1,stc,smc,sh2o,snowh,sneqv,
     o ch,etp,eta,h,s,runoff1,runoff2,q1,snmax,albedo,
     o soilw,soilm)
c
      implicit none
cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  sub-driver for family of physics subroutines of a
cc              soil/veg/snowpack land-surface model to update soil 
cc              moisture, soil ice, soil temperature, skin temperature, 
cc              snowpack water content,snowdepth, and all terms
cc              of the surface energy balance and surface water
cc              balance (excluding input atmospheric forcings of 
cc              downward radiation and precip)
cc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc
c ------------    frozen ground version     -------------------------------
c     new states: sh2o(nsoil) - unfrozen soil moisture
c                 snowh       - snow depth
c
c -------------------------------------------------------------------------
c
c  note on snow state variables:
c    snowh = actual physical snow depth in m
c    sneqv = liquid water-equivalent snow depth in m
c            (time-dependent snow density is obtained from sneqv/snowh)
c
c  note on albedo fractions:
c     input:
c       alb    = baseline snow-free albedo, for julian day of year 
c                (usually from temporal interpolation of monthly mean values)
c                (calling prog may or may not include diurnal sun angle effect)
c       snoalb = upper bound on maximum albedo over deep snow
c                (e.g. from  robinson and kukla, 1985, j. clim. & appl. meteor.)
c     output:
c       albedo = computed albedo with snowcover effects 
c                (computed using alb, snoalb, sneqv, and shdfac->>veg greenness)
c
c                  argument list in the call sflx
c
c 1. calling statement
c
c         call sflx(ice,lsaturated,dt,z,nsoil,nroot,sldpth,
c  i lwdn,soldn,sfcprs,prcp,sfctmp,th2,q2,q2sat,dqsdt2,
c  i ivegtyp,soiltyp,slopetyp,
c  i shdfac,xlai,ptu,refkdt,tbot,alb,snoalb,
c  i z0,sfcspd,czil,
c  2 cmc,t1,stc,smc,sh2o,snowh,sneqv,
c  o ch,etp,eta,h,s,runoff1,runoff2,q1,snmax,albedo,
c  o soilw,soilm)
c
c 2. input
c                  *** general parameters ***
c
c          ice: sea-ice flag  (=1: sea-ice, =0: land)
c    saturated: atmospheric saturation flag 
c              (true: saturation -> etp=0.0 in sflx
c               false: otherwise)
c           dt: timestep (sec)
c            z: height (m) above ground of atmospheric forcing variables
c        nsoil: number of soil layers
c        nroot: number of root-zone layers
c       sldpth: the thickness of each soil layer (m) 
c
c                  *** atmospheric variables ***
c
c         lwdn: lw downward radiation (w m-2; positive, if downward)
c        soldn: solar downward radiation (w m-2; positive, if downward)
c       sfcprs: pressure at height z above ground(pascals)
c         prcp: precip rate (kg m-2 s-1)
c       sfctmp: air temperature (k) at height z above ground 
c          th2: air potential temperature (k) at ground surface 
c           q2: mixing ratio at height z above ground (kg kg-1)
c        q2sat: sat mixing ratio at height z above ground (kg kg-1)
c       dqsdt2: slope of sat specific humidity curve at t=sfctmp (kg kg-1 k-1)
c
c                  *** canopy/soil characteristics ***
c
c       vegtyp: vegetation type (integer index)
c       soiltyp: soil type (integer index)
c     slopetyp: class of bottom layer slope (integer index)
c        fxexp: bare soil evaporation exponent used in devap

c       shdfac: areal fractional coverage of green vegetation
c         xlai: leaf-area index (unitless, in the range 0.-5.)
c        sbeta: soil heat flux vegetation-reduction coefficient
c          ptu: photo thermal unit (plant phenology for annuals/crops)
c       refkdt: baseline value for runoff parameter kdt
c         tbot: bottom soil temperature (local yearly-mean air temperature) 
c          alb: backround snow-free surface albedo (fraction)
c       snoalb: albedo upper bound over deep snow (fraction)
c
c 3. input and output state variables
c
c       !!! *********** state variables **************  !!!
c
c         cmc: canopy moisture content (m)
c          t1: ground (and/or snow) skin temperature (k)

c  stc(nsoil): soil temp (k)
c  smc(nsoil): total soil moisture content (volumetric fraction)
c sh2o(nsoil): unfrozen soil moisture content (volumetric fraction)

c       snowh: snow depth (m)
c       sneqv: water-equivalent snow depth (m)
c      albedo: surface albedo including snow effect (unitless fraction)
c    *** note: tstate variables are input and output***
c
c 4. output
c           ch: sfc exch coef for heat and moisture (m s-1)
c          etp: potential evaporation (w m-2)
c          eta: actual latent heat flux (w m-2: negative, if upward from surface)
c            h: sensible heat flux (w m-2: negative, if upward from surface)
c            s: soil heat flux (w m-2: negative, if downward from surface)
c      runoff1: surface runoff (m s-1)
c      runoff2: subsurface runoff (m s-1)
c           q1: effective mixing ratio at grnd sfc ( kg kg-1) 
c        snmax: snow melt (m) (water equivalent)
c       albedo: surface albedo including snow effect (unitless fraction)
c        soilm: total soil column water content (m)
c        soilw: available soil moisture (unitless fraction between 
c               soil saturation and wilting point) 

      integer nsold
      parameter (nsold = 20)
c
      logical snowng
      logical frzgra
      logical saturated
c
      integer k
      integer kz
      integer ice
      integer nsoil,vegtyp,soiltyp,nroot
      integer slopetyp
c
      real r
      real cp
c
      parameter ( r = 287.04, cp = 1004.5 )

      real b
      real cfactr
c                  ch is sfc exchange coef for heat/moist
c                  cm is sfc momentum drag (not needed in sflx)
      real ch
      real cm
c
      real cmc
      real cmcmax
      real csoil
      real czil
      real df1
      real dksat
      real dt
      real dwsat
      real epsca
      real sneqv
      real eta
      real etp
      real f
      real f1
      real fxexp
      real kdt
      real lwdn
      real pc
      real prcp
      real ptu
      real q1
      real q2
      real q2sat
      real rch
      real refkdt
      real rr
      real rtdis (nsold)
      real s
      real sbeta
      real sfcprs
      real sfcspd
      real sfctmp
      real shdfac
      real smc(nsoil),stc(nsoil),sldpth(nsoil),sh2o(nsoil)
      real smcdry
      real smcmax
      real smcref
      real smcwlt
cc..      real sneqv
      real snowh
      real t1
      real t1v
      real t1min
      real t24
      real t2v
      real tbot
      real th2
      real th2v
      real z
      real z0
      real zsoil  ( nsold )
      real zbot
c
c new snow heat flux calc      
      real dsoil
      real dtot
      real expsno
      real expsoi
      real ssnow
c
      real topt, tfreez, soldn, dqsdt2, xlai, runoff1, runoff2
      real snmax, soilw, rgl, soilm
      real beta, drip, ec, edir, ett, flx1, flx2, flx3, runof, dew
      real rib, runoff3, rsmax, albedo, rcmin, hs, psisat
      real slope, snup,salp,snoalb, alb
      real quartz, frzx, sndens, sncond 
      real csnow, rsnow, snofac
      real sn_new, prcp1, sice, rc, edir1, ec1, ett1, soilwm
      real soilww
      real h

      parameter (tfreez = 273.15)
      
cmic$ taskcommon rite
!$omp threadprivate(/rite/)
      common/rite/ beta,drip,ec,edir,ett,flx1,flx2,flx3,runof,
     &             dew,rib,runoff3
c
c   initialization

      runoff1 = 0.0
      runoff2 = 0.0
      runoff3 = 0.0
      snmax = 0.0

      if(ice .eq. 1) then

c sea-ice layers are equal thickness and sum to 3 meters
        do kz = 1, nsoil
          zsoil(kz)=-3.*float(kz)/float(nsoil)
        end do

      else

c calculate depth (negative) below ground from top skin sfc to 
c bottom of each soil layer
        zsoil(1)=-sldpth(1)
        do kz = 2, nsoil
          zsoil(kz)=-sldpth(kz)+zsoil(kz-1)
        end do

      endif
         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc
cc   next is crucial call to set the land-surface parameters, 
cc   including soil-type and veg-type dependent parameters.
cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc
        call redprm(vegtyp,soiltyp, slopetyp, 
     +    cfactr, cmcmax, rsmax, topt, refkdt, kdt, sbeta,
     o    shdfac, rcmin, rgl, hs, zbot, frzx, psisat, slope, 
     +    snup, salp, b, dksat, dwsat, smcmax, smcwlt, smcref,
     o    smcdry, f1, quartz, fxexp, rtdis, sldpth, zsoil,
     +    nroot, nsoil, z0, czil, xlai, csoil)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc
cc  next call routine sfcdif to calculate 
cc    the sfc exchange coef (ch) for heat and moisture
cc
cc  note  note  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cc    
cc          comment out call sfcdif, if sfcdif already called
cc          in calling program (such as in coupled atmospheric model)
cc
cc  note !!  do not call sfcdif until after above call to redprm, 
cc             in case alternative values of roughness length (z0) and 
cc              zilintinkevich coef (czil) are set there via namelist i/o
cc
cc   note !! routine sfcdif returns a ch that represents the wind spd
cc          times the "original" nondimensional "ch" typical in literature.
cc          hence the ch returned from sfcdif has units of m/s.
cc          the important companion coefficient of ch, carried here as "rch",
cc          is the ch from sfcdif times air density and parameter "cp".
cc         "rch" is computed in "call penman" and carried in common/rite/.
cc          rch rather than ch is the coeff usually invoked late in eqns.
cc
cc   note !!  the companion coefficient
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
          
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c calc virtual temps and potential temps from air temp
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c th2 is passed in
c.....th2  = sfctmp + ( 0.0098 * z )

      t2v  = sfctmp * (1.0 + 0.61 * q2 )
c     t1v  =     t1 * (1.0 + 0.61 * q2 )
      t1v  =     t1 * (1.0 + 0.61 * q1 )
      th2v =    th2 * (1.0 + 0.61 * q2 )
c
c ------------ for LES coupling call in the main routine
c
c     call sfcdif ( z, z0, t1v, th2v, sfcspd, czil, cm, ch )
c
c
c prt      print*,' 1st dt=', dt
c prt      print*,' ***********   input parameters   *************'
c prt      print*,'                                               '
c prt           print*,'vegtyp= ',vegtyp
c prt           print*,'soiltyp= ',soiltyp
c prt           print*,'slopetyp= ',slopetyp
c prt           print*,'cfactr= ',cfactr
c prt           print*,'cmcmax= ',cmcmax
c prt           print*,'rsmax= ',rsmax
c prt           print*,'topt= ',topt
c prt           print*,'refkdt= ',refkdt
c prt           print*,'kdt= ',kdt
c prt           print*,'z0= ',z0
c prt           print*,'czil= ',czil
c prt           print*,'ch= ',ch
c prt           print*,'shdfac= ',shdfac
c prt           print*,'rcmin= ',rcmin
c prt           print*,'rgl= ',rgl
c prt           print*,'hs= ',hs
c prt           print*,'zbot= ',zbot
c prt           print*,'frzx= ',frzx
c prt           print*,'psisat= ',psisat
c prt           print*,'slope= ',slope
c prt           print*,'snup= ',snup
c prt           print*,'salp= ',salp
c prt           print*,'b= ',b
c prt           print*,'dksat= ',dksat
c prt           print*,'dwsat= ',dwsat
c prt           print*,'smcmax= ',smcmax
c prt           print*,'smcwlt= ',smcwlt
c prt           print*,'smcref= ',smcref
c prt           print*,'smcdry= ',smcdry
c prt           print*,'f1= ',f1
c prt           print*,'quartz= ',quartz
c prt           print*,'rtdis= ',rtdis
c prt           print*,'sldpth= ',sldpth
c prt           print*,'zsoil= ',zsoil
c prt           print*,'nroot= ',nroot
c prt      print*,'nsoil= ',nsoil

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  initialize misc variables.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      s = 0.0
      snowng = .false.
      frzgra = .false.

c if sea-ice case,        assign default water-equiv snow on top
      if(ice .eq. 1) then
        sneqv = 0.01
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     determine if it's precipitating and what kind of precip it is.
c     if it's prcping and the air temp is colder than 0 c, it's snowing!
c     if it's prcping and the air temp is warmer than 0 c, but the grnd
c     temp is colder than 0 c, freezing rain is presumed to be falling.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( prcp .gt. 0.0 ) then
        if ( sfctmp .le. tfreez ) then
          snowng = .true.
        else
          if ( t1 .le. tfreez ) frzgra = .true.
        endif
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     if either prcp flag is set, convert the prcp rate from
c     kg m-2 s-1 to a liquid equiv snow depth (meters) and accum it.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if(sneqv .eq. 0.0) then
        sndens = 0.0
        snowh = 0.0
        sncond = 1.0
      else
        sndens=sneqv/snowh
        sncond = csnow (sndens) 
      endif     

      if ( ( snowng ) .or. ( frzgra ) ) then
      
c ----------   frozen ground modification   ----------------------
c          snow density & conductivity estimation
c 
c                                ...1st: new snowfall amount
      sn_new = prcp * dt * 0.001
c                                ...2nd:  update snow density using 
c                                                 old and new snow
      call snow_new (sfctmp,sn_new,snowh,sndens)
c
c **  ....csnow is function to calculate snow conductivity
      sncond = csnow (sndens) 
c
c ----------------------------------------------------------------      
                   
        sneqv = sneqv + prcp * dt * 0.001
        prcp1 = 0.0
      else
        prcp1 = prcp
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c check: if sneqv < 1.e-6 m then snow zeroed and residual is snowmelt
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if(sneqv.lt.1.e-6) then
         snmax=sneqv
         sneqv = 0.0
         
         snowh = 0.0
         
         snowng = .false.
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc grnd heat flux for snow pack case (will effectively
c     calc the heat flux below the snow pack).  remember, if
c     t1 > 273.15 k, there will be snow melt.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( sneqv .gt. 0.0 ) then
        t1min = min ( t1, tfreez )

c  ----   frozen ground modification using computed snow density ---
c         s = sncond * (t1min - stc(1) ) /snowh
c new soil heat flux under snow calculation, 10 may 1999, m. ek
c 1st soil thermal conductivity
        call tdfcnd ( df1, smc(1), b, f1,quartz,smcmax,sh2o )
c next veg effect in thermal cond. (reduction), 10 may 1999, m. ek
        df1 = df1 * exp(sbeta*shdfac)
c finally snow effect in thermal cond., 10 may 1999, m. ek
        dsoil = -(0.5 * zsoil(1))
        dtot = snowh + dsoil
        expsno = snowh/dtot
        expsoi = dsoil/dtot
c 1. harmonic mean (series flow)
c        df1 = (sncond*df1)/(expsoi*sncond+expsno*df1)
c 2. arithmetic mean (parallel flow)
c        df1 = expsno*sncond + expsoi*df1
c 3. geometric mean
        df1 = (sncond**expsno)*(df1**expsoi)
c
c use t1 rather than tfreez
        s = df1 * (t1 - stc(1) ) / dtot
        ssnow = sncond * (t1 - stc(1) ) /snowh
c        write(*,*)
c        write(*,*)'sflx1: s,ssnow,snowh',s,ssnow,snowh

c ------------------------------------------------------------------
           
cc........s = 0.35 * ( t1min - stc(1) ) / ( 10.0 * sneqv )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  bound the heat flux magnitude under snow pack to 100 w/m-2
c       set the minimum flux within snow to -200 w m-2
c       for numerical reasons related to very shallow snow depth
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c turn off bounds on soil heat flux under snow
c        s = min ( max ( s,-100.0 ), 100.0 )

      else

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calculate grnd heat flux for case with no snow pack
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        call tdfcnd ( df1, smc(1), b, f1,quartz,smcmax,sh2o )
c veg effect in thermal cond. (reduction), 7 may 1999, m. ek
        df1 = df1 * exp(sbeta*shdfac)

c ---------   frozen ground version    ---------------------------
c         soil thermal diffusivity correction
c
        sice = smc(1) - sh2o(1)
c__________________________________________________________________
c this if-statement is no longer used if peters-lidard thermal
c conductivity is used for all conditions (frozen,unfrozen):
c        if (sice .ne. 0.0) call frzcnd ( df1, sice )

c ----------------------------------------------------------------        
c snow effect in thermal cond., 10 may 1999, m. ek
        dsoil = -(0.5 * zsoil(1))
        dtot = snowh + dsoil
        expsno = snowh/dtot
        expsoi = dsoil/dtot
c 1. harmonic mean (series flow)
c        df1 = (sncond*df1)/(expsoi*sncond+expsno*df1)
c 2. arithmetic mean (parallel flow)
c        df1 = expsno*sncond + expsoi*df1
c 3. geometric mean
        df1 = (sncond**expsno)*(df1**expsoi)
c
c use t1 rather than tfreez
        s = df1 * (t1 - stc(1) ) / dtot

c        s = df1 * ( stc(1) - t1 ) / ( 0.5 * zsoil(1) )
        
      endif

c
c  modify albedo if snowcover  
c
      if ( (sneqv .eq. 0.0) .or. (alb .ge. snoalb) ) then
      
        albedo = alb
         
      else
c                                   snow effect is snowdepth dependent
            if (sneqv .lt. snup) then
              rsnow = sneqv/snup
              snofac = 1. - ( exp(-salp*rsnow) - rsnow*exp(-salp))
            else
              snofac = 1.0 
            endif
                   
         albedo = alb + (1.0-shdfac)*snofac*(snoalb-alb)
      
       endif
       
c  calculate downward radiation
           
          f = soldn*(1.0-albedo) + lwdn

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     call penman subroutine to calculate potential evaporation (etp)
c     (and other partial products and sums save in common/rite/ for 
c       later calculations)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call penman ( sfctmp,sfcprs,ch,t2v,th2,prcp,f,t24,s,q2,
     &              q2sat,etp,rch,epsca,rr,snowng,frzgra,dqsdt2)
c       if(saturated) etp = 0.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     call canres to calculate the canopy resistance and convert it 
c     into pc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if(shdfac .gt. 1.e-6) then
      
c  ----  frozen ground extension: smc was replaced by sh2o  -----
c      
        call canres(soldn,ch,sfctmp,q2,sfcprs,sh2o,zsoil,nsoil,
     &            smcwlt,smcref,rcmin,rc,pc,nroot,q2sat,dqsdt2,
     &            topt,rsmax,rgl,hs,xlai)
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       if point is to be fully processed....
c       based on whether or not there is a snow pack, call snopac or
c       nopac to calculate an estimate of actual evapotranspiration,
c       update soil moisture, and update soil temperature.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        if ( sneqv .eq. 0.0 ) then

          call nopac ( etp, eta, prcp, smc, smcmax, smcwlt,
     &                 smcref,smcdry, cmc, cmcmax, nsoil, dt, shdfac,
     &                 sbeta,q1,q2,t1,sfctmp,t24,th2,f,f1,s,stc,
     &                 epsca, b, pc, rch, rr,  cfactr,
     +                 sh2o, slope, kdt, frzx, psisat,
     &                 zsoil, dksat, dwsat, tbot,runoff1,runoff2,
     &                 runoff3, edir1, ec1, ett1,nroot,ice,rtdis,
     &                 quartz, fxexp,csoil)

        else

          call snopac ( etp,eta,prcp,prcp1,snowng,smc,smcmax,smcwlt,
     &                smcref, smcdry, cmc, cmcmax, nsoil, dt, 
     &                sbeta,q1,
     &                q2,t1,sfctmp,t24,th2,f,f1,s,stc,epsca,sfcprs,
     &                b, pc, rch, rr, cfactr, salp, sneqv,
     +                snowh, sh2o, slope, kdt, frzx, psisat, snup,
     &                zsoil, dwsat, dksat, tbot, shdfac,runoff1,
     &                runoff2,runoff3,edir1,ec1,ett1,nroot,snmax,ice,
     &                rtdis,quartz, fxexp,csoil)
        
        endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  modify albedo with updated snow (for return to parent model radiation 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( (sneqv .eq. 0.0) .or. (alb .ge. snoalb) ) then
      
        albedo = alb
         
      else
c                                   snow effect is snowdepth dependent
            if (sneqv .lt. snup) then
              rsnow = sneqv/snup
              snofac = 1. - ( exp(-salp*rsnow) - rsnow*exp(-salp))
            else
              snofac = 1.0 
            endif
                   
         albedo = alb + (1.0-shdfac)*snofac*(snoalb-alb)
      
       endif
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   prepare sensible heat (h) for return to parent model
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c       t2v = sfc air virtual temperature
c       rho = sfc air density (kg m-3)
c.....  rho =  sfcprs/(r * t2v)
c.....chkff =  (ch * cp * rho )
c.....    h = -(chkff        ) * ( th2 - t1 )
c
          h = -(ch * cp * sfcprs)/(r * t2v) * ( th2 - t1 )
          
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  convert units and/or sign of total evap (eta), potential evap (etp),
c      subsurface heat flux (s), and runoffs for
c        what parent model expects
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  convert eta from kg m-2 s-1 to w m-2
c
      eta = eta*2.5e+6
      etp = etp*2.5e+6

c convert the sign of soil heat flux so that:
c         s>0: warm the surface  (night time)
c         s<0: cool the surface  (day time)

      s=-1.0*s      
c
c  convert runoff3 (internal layer runoff from supersat) from m to m s-1
c  and add to subsurface runoff/drainage/baseflow
c
      runoff3 = runoff3/dt
      runoff2 = runoff2+runoff3
c
c total column soil moisture in meters (soilm) and root-zone 
c soil moisture availability (fraction) relative to porosity/saturation

      soilm=-1.0*smc(1)*zsoil(1)
      
      do k = 2, nsoil
        soilm=soilm+smc(k)*(zsoil(k-1)-zsoil(k))
      end do
      soilwm=-1.0*(smcmax-smcwlt)*zsoil(1)
      soilww=-1.0*(smc(1)-smcwlt)*zsoil(1)
      do k = 2, nroot
        soilwm=soilwm+(smcmax-smcwlt)*(zsoil(k-1)-zsoil(k))
        soilww=soilww+(smc(k)-smcwlt)*(zsoil(k-1)-zsoil(k))
      end do
      soilw=soilww/soilwm
c
      return
      end
      subroutine redprm(vegtyp, soiltyp, slopetyp,
     +     cfactr, cmcmax, rsmax, topt, refkdt, kdt, sbeta,
     +     shdfac, rcmin, rgl, hs, zbot, frzx, psisat, slope,
     +     snup, salp, b, dksat, dwsat, smcmax, smcwlt, smcref,
     +     smcdry, f1, quartz, fxexp, rtdis, sldpth, zsoil,
     +     nroot, nsoil, z0, czil, lai, csoil)

      implicit none

c  this subroutine reads all the soil and vegetation parameters
c  required for the execusion of the noah - lsm
c  old version f. chen 3/15/95
c  new version dag lohmann, ncep/emc, may 1999
c  optional non-default parameters can be read in, accommodating up
c  to 30 soil, veg, or slope classes, if the default max number of 
c  soil, veg, and/or slope types is reset.

c  set maximum number of soil-, veg-, and slopetyp in data statement

      integer max_soiltyp
      integer max_vegtyp
      integer max_slopetyp
      parameter (max_soiltyp  = 30)
      parameter (max_vegtyp   = 30)
      parameter (max_slopetyp = 30)

c  number of defined soil-, veg-, and slopetyps used

      integer defined_veg
      integer defined_soil
      integer defined_slope
      data defined_veg/13/
      data defined_soil/9/
      data defined_slope/9/

c  set-up soil parameters for given soil type
c  input: soltyp: soil type (integer index)
c  output: soil parameters:

c    maxsmc: max soil moisture content (porosity)
c    refsmc: reference soil moisture (onset of soil moisture
c            stress in transpiration)
c    wltsmc: wilting pt soil moisture contents
c    drysmc: air dry soil moist content limits
c    satpsi: saturated soil potential
c    satdk:  saturated soil hydraulic conductivity
c    bb:     the 'b' parameter
c    satdw:  saturated soil diffusivity
c    f11:    used to compute soil diffusivity/conductivity
c    quartz:  soil quartz content
c
c note: satdw = bb*satdk*(satpsi/maxsmc)
c  f11 = alog10(satpsi) + bb*alog10(maxsmc) + 2.0
c  refsmc1=maxsmc*(5.79e-9/satdk)**(1/(2*bb+3)) 5.79e-9 m/s= 0.5 mm/day
c  refsmc=refsmc1+1./3.(maxsmc-refsmc1)
c  wltsmc1=maxsmc*(200./satpsi)**(-1./bb)  (wetzel and chang, 1987)
c  wltsmc=wltsmc1-0.5*wltsmc1
c
c soil types   zobler (1986)      cosby et al (1984) (quartz cont.(1))
c  1        coarse            loamy sand         (0.82)
c  2        medium            silty clay loam    (0.10)
c  3        fine              light clay         (0.25)
c  4        coarse-medium     sandy loam         (0.60)
c  5        coarse-fine       sandy clay         (0.52)
c  6        medium-fine       clay loam          (0.35)
c  7        coarse-med-fine   sandy clay loam    (0.60)
c  8        organic           loam               (0.40)
c  9        glacial land ice  loamy sand         (na using 0.82)

      real bb(max_soiltyp)
      real drysmc(max_soiltyp)
      real f11(max_soiltyp)
      real maxsmc(max_soiltyp)
      real refsmc(max_soiltyp)
      real satpsi(max_soiltyp)
      real satdk(max_soiltyp)
      real satdw(max_soiltyp)
      real wltsmc(max_soiltyp)
      real qtz(max_soiltyp)

      real b
      real dksat
      real dwsat
      real smcmax
      real smcwlt
      real smcref
      real smcdry
      real f1
      real quartz

      data maxsmc/0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
     &            0.404, 0.439, 0.421, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data satpsi/0.04, 0.62, 0.47, 0.14, 0.10, 0.26,
     &            0.14, 0.36, 0.04, 0.00, 0.00, 0.00,
     &            0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &            0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &            0.00, 0.00, 0.00, 0.00, 0.00, 0.00/
      data satdk /1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
     &            0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5, 0.00,
     &            0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &            0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &            0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &            0.00   , 0.00   , 0.00   , 0.00   , 0.00/
      data bb    /4.26,  8.72, 11.55, 4.74, 10.73,  8.17,
     &            6.77,  5.25,  4.26, 0.00,  0.00,  0.00,
     &            0.00,  0.00,  0.00, 0.00,  0.00,  0.00,
     &            0.00,  0.00,  0.00, 0.00,  0.00,  0.00,
     &            0.00,  0.00,  0.00, 0.00,  0.00,  0.00/
      data refsmc/0.283, 0.387, 0.412, 0.312, 0.338, 0.382,
     &            0.315, 0.329, 0.283, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data wltsmc/0.029, 0.119, 0.139, 0.047, 0.020, 0.103,
     &            0.069, 0.066, 0.029, 0.283, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data drysmc/0.029, 0.119, 0.139, 0.047, 0.020, 0.103,
     &            0.069, 0.066, 0.029, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data satdw /5.71e-6, 2.33e-5, 1.16e-5, 7.95e-6, 1.90e-5,
     &            1.14e-5, 1.06e-5, 1.46e-5, 5.71e-6, 0.00,
     &            0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &            0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &            0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &            0.00   , 0.00   , 0.00   , 0.00   , 0.00/
      data f11  /-0.999, -1.116, -2.137, -0.572, -3.201, -1.302,
     &           -1.519, -0.329, -0.999,  0.000,  0.000,  0.000,
     &            0.000,  0.000,  0.000,  0.000,  0.000,  0.000,
     &            0.000,  0.000,  0.000,  0.000,  0.000,  0.000,
     &            0.000,  0.000,  0.000,  0.000,  0.000,  0.000/
      data qtz   /0.82, 0.10, 0.25, 0.60, 0.52, 0.35,
     &            0.60, 0.40, 0.82, 0.00, 0.00, 0.00,
     &            0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &            0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &            0.00, 0.00, 0.00, 0.00, 0.00, 0.00/

c***********************************************************************

c  set-up vegetation parameters for a given vegetaion type
c
c  input: vegtyp = vegetation type (integer index)
c  ouput: vegetation parameters
c         shdfac: vegetation greenness fraction
c         rcmin:  mimimum stomatal resistance
c         rgl:    parameter used in solar rad term of
c                 canopy resistance function
c         hs:     parameter used in vapor pressure deficit term of
c                 canopy resistance function
c         snup:   threshold snow depth (in water equivalent m) that
c                 implies 100% snow cover
c
c  ssib vegetation types (dorman and sellers, 1989; jam)
c
c   1:   broadleaf-evergreen trees  (tropical forest)
c   2:   broadleaf-deciduous trees
c   3:   broadleaf and needleleaf trees (mixed forest)
c   4:   needleleaf-evergreen trees
c   5:   needleleaf-deciduous trees (larch)
c   6:   broadleaf trees with groundcover (savanna)
c   7:   groundcover only (perennial)
c   8:   broadleaf shrubs with perennial groundcover
c   9:   broadleaf shrubs with bare soil
c  10:   dwarf trees and shrubs with groundcover (tundra)
c  11:   bare soil
c  12:   cultivations (the same parameters as for type 7)
c  13:   glacial (the same parameters as for type 11)

      integer nroot_data(max_vegtyp)
      real    rsmtbl(max_vegtyp)
      real    rgltbl(max_vegtyp)
      real    hstbl(max_vegtyp)
      real    snupx(max_vegtyp)
      real    z0_data(max_vegtyp)
      real    lai_data(max_vegtyp)

      integer nroot
      real    shdfac
      real    rcmin
      real    rgl
      real    hs
      real    frzfact
      real    psisat
      real    snup
      real    z0
      real    lai

      data nroot_data /4,4,4,4,4,4,3,3,3,3,0,3,0,0,0,
     *                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data rsmtbl /150.0, 100.0, 125.0, 150.0, 100.0, 70.0,
     *              40.0, 300.0, 400.0, 150.0, 999.0, 40.0,
     *             999.0,   0.0,   0.0,   0.0,   0.0,  0.0,
     *               0.0,   0.0,   0.0,   0.0,   0.0,  0.0,
     *               0.0,   0.0,   0.0,   0.0,   0.0,  0.0/
      data rgltbl /30.0,  30.0,  30.0,  30.0,  30.0,  65.0,
     *            100.0, 100.0, 100.0, 100.0, 999.0, 100.0,
     *            999.0,   0.0,   0.0,   0.0,   0.0,   0.0,
     *              0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
     *              0.0,   0.0,   0.0,   0.0,   0.0,   0.0/
      data hstbl /41.69, 54.53, 51.93, 47.35,  47.35, 54.53,
     *            36.35, 42.00, 42.00, 42.00, 999.00, 36.35,
     *           999.00,  0.00,  0.00,  0.00,   0.00,  0.00,
     *             0.00,  0.00,  0.00,  0.00,   0.00,  0.00,
     *             0.00,  0.00,  0.00,  0.00,   0.00,  0.00/
      data snupx  /0.080, 0.080, 0.080, 0.080, 0.080, 0.080,
     *             0.040, 0.040, 0.040, 0.040, 0.025, 0.040,
     *             0.025, 0.000, 0.000, 0.000, 0.000, 0.000,
     *             0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     *             0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data z0_data /2.653, 0.826, 0.563, 1.089, 0.854, 0.856,
     *              0.035, 0.238, 0.065, 0.076, 0.011, 0.035,
     *              0.011, 0.000, 0.000, 0.000, 0.000, 0.000,
     *              0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     *              0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data lai_data /3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
     *               3.0, 3.0, 3.0, 3.0, 0.0, 3.0,
     *               0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     *               0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     *               0.0, 0.0, 0.0, 0.0, 0.0, 0.0/

c***********************************************************************

c  class parameter 'slopetyp' was included to estimate
c  linear reservoir coefficient 'slope' to the baseflow runoff
c  out of the bottom layer. lowest class (slopetyp=0)means
c  highest slope parameter= 1
c  definition of slopetyp from 'zobler' slope type
c  slope class      percent slope
c  1                0-8
c  2                8-30
c  3                > 30
c  4                0-30
c  5                0-8 & > 30
c  6                8-30 & > 30
c  7                0-8, 8-30, > 30
c  9                glacial ice
c  blank            ocean/sea
c  note:  class 9 from 'zobler' file should be replaced by 8
c  and 'blank'  9

      real slope
      real slope_data(max_slopetyp)
      data slope_data /0.1,  0.6, 1.0, 0.35, 0.55, 0.8,
     *                 0.63, 0.0, 0.0, 0.0,  0.0,  0.0,
     *                 0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,
     *                 0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,
     *                 0.0 , 0.0, 0.0, 0.0,  0.0,  0.0/

c***********************************************************************

c  set namelist file name

      character*50 namelist_name

c***********************************************************************

c set universal parameters (not dependent on soil, veg, slope type)

      integer vegtyp
      integer soiltyp
      integer slopetyp

      integer nsoil
      integer i

      integer bare
      data    bare /11/

      logical lparam
      data    lparam /.true./

      logical lfirst
      data    lfirst /.true./

c  parameter used to calculate roughness length of heat
      real czil, czil_data
      data czil_data /0.2/

c  parameter used to caluculate vegetation effect on soil heat flux
      real sbeta, sbeta_data
      data sbeta_data /-2.0/

c bare soil evaporation exponent used in devap

      real fxexp, fxexp_data
      data fxexp_data /2.0/

c soil heat capacity [j/m**3/k]

      real csoil, csoil_data
      data csoil_data /1.26e+6/

c  specify snow distribution shape parameter
c  salp   - shape parameter of distribution function
c  of snow cover. from anderson's data (hydro-17)
c  best fit is when salp = 2.6
      real salp, salp_data
      data salp_data /2.6/

c  kdt is defined by reference refkdt and dksat
c  refdk=2.e-6 is the sat. dk. value for the soil type 2
      real refdk, refdk_data
      data refdk_data /2.0e-6/

      real refkdt, refkdt_data
      data refkdt_data /3.0/

      real kdt
      real frzx

c  frozen ground parameter, frzk, definition
c  frzk is ice content threshold above which frozen soil is impermeable
c  reference value of this parameter for the light clay soil (type=3)
c  frzk = 0.15 m
      real frzk, frzk_data
      data frzk_data /0.15/

      real rtdis(nsoil)
      real sldpth(nsoil)
      real zsoil(nsoil)

c  set two canopy water parameters
      real cfactr, cfactr_data
      real cmcmax, cmcmax_data
      data cfactr_data /0.5/
      data cmcmax_data /0.5e-3/

c  set max. stomatal resistance
      real rsmax, rsmax_data
      data rsmax_data /5000.0/

c  set optimum transpiration air temperature
      real topt, topt_data
      data topt_data /298.0/

c  specify depth[m] of lower boundary soil temperature
      real zbot, zbot_data
      data zbot_data /3.0/

c***********************************************************************

c  namelist definition

c     namelist /soil_veg/ slope_data, rsmtbl, rgltbl, hstbl, snupx,
c    &     bb, drysmc, f11, maxsmc, refsmc, satpsi, satdk, satdw,
c    &     wltsmc, qtz, lparam, zbot_data, salp_data, cfactr_data,
c    &     cmcmax_data, sbeta_data, rsmax_data, topt_data,
c    &     refdk_data, frzk_data, bare, defined_veg, defined_soil,
c    &     defined_slope, fxexp_data, nroot_data, refkdt_data, z0_data,
c    &     czil_data, lai_data, csoil_data

c  read namelist file to override default parameters
c  only once.

c ----       if (lfirst) then
c ----          open(58, file = 'namelist_filename.txt')
c ---- c namelist_name must be 50 characters or less.
c ----          read(58,'(a)') namelist_name
c ----          close(58)
c ----          write(*,*) 'namelist filename is ', namelist_name
c ----          open(59, file = namelist_name)
c ----  50      continue
c ----          read(59, soil_veg, end=100)
c ----          if (lparam) goto 50
c ----  100     continue
c ----          close(59)
c ----          write(*,nml=soil_veg)
c ----          lfirst = .false.
c ----          if (defined_soil .gt. max_soiltyp) then
c ----             write(*,*) 'warning: defined_soil too large in namelist'
c ----             stop 222
c ----          end if
c ----          if (defined_veg .gt. max_vegtyp) then
c ----             write(*,*) 'warning: defined_veg too large in namelist'
c ----             stop 222
c ----          end if
c ----          if (defined_slope .gt. max_slopetyp) then
c ----             write(*,*) 'warning: defined_slope too large in namelist'
c ----             stop 222
c ----          end if
c ----       end if

      if (soiltyp .gt. defined_soil) then
         write(*,*) 'warning: too many soil types'
         stop 333
      end if
      if (vegtyp .gt. defined_veg) then
         write(*,*) 'warning: too many veg types'
         stop 333
      end if
      if (slopetyp .gt. defined_slope) then
         write(*,*) 'warning: too many slope types'
         stop 333
      end if

c  set-up universal parameters 
c (not dependent on soiltyp, vegtyp or slopetyp)
      zbot   = zbot_data
      salp   = salp_data
      cfactr = cfactr_data
      cmcmax = cmcmax_data
      sbeta  = sbeta_data
      rsmax  = rsmax_data
      topt   = topt_data
      refdk  = refdk_data
      frzk   = frzk_data
      fxexp  = fxexp_data
      refkdt = refkdt_data
      czil   = czil_data
      csoil  = csoil_data

c  set-up soil parameters
      b       = bb(soiltyp)
      smcdry  = drysmc(soiltyp)
      f1      = f11(soiltyp)
      smcmax  = maxsmc(soiltyp)
      smcref  = refsmc(soiltyp)
      psisat  = satpsi(soiltyp)
      dksat   = satdk(soiltyp)
      dwsat   = satdw(soiltyp)
      smcwlt  = wltsmc(soiltyp)
      quartz  = qtz(soiltyp)
      frzfact = (smcmax / smcref) * (0.412 / 0.468)
      kdt     = refkdt * dksat/refdk

c  to adjust frzk parameter to actual soil type: frzk * frzfact

      frzx = frzk * frzfact

c  set-up vegetation parameters
      nroot = nroot_data(vegtyp)
      snup  = snupx(vegtyp)
      rcmin = rsmtbl(vegtyp)
      rgl   = rgltbl(vegtyp)
      hs    = hstbl(vegtyp)
      z0    = z0_data(vegtyp)
      lai   = lai_data(vegtyp)
      if(vegtyp .eq. bare) shdfac = 0.0

      if (nroot .gt. nsoil) then
         write(*,*) 'warning: too many root layers'
         stop 333
      end if

c  calculate root distribution
c  present version assumes uniform distribution based on soil layers

      do i=1,nroot
         rtdis(i) = -sldpth(i)/zsoil(nroot)
      end do

c  set-up slope parameter
      slope = slope_data(slopetyp)

      return
      end
      subroutine sfcdif(btg,zlm,z0,hpbl,thz0,thlm,sfcspd,czil,
     +                  akms,akhs,rlmo,ustar)
      implicit none
c
c -------- /pps/ modified to take in and output les quantities
c
c     ******************************************************************
c     *                                                                *
c     *                        surface layer                           *
c     *                                                                *
c     ******************************************************************
c-----------------------------------------------------------------------

      real wwst, wwst2, g, vkrm, excm, beta, btg, elfc, wold, wnew
      real pihf, epsu2, epsust, epsit, epsa, ztmin, ztmax, hpbl, sqvisc
      real ric, rric, fhneu, rfc, rfac, zz, pslmu, pslms, pslhu, pslhs
      real xx, pspmu, yy, pspms, psphu, psphs, zlm, z0, thz0, thlm
      real sfcspd, czil, akms, akhs, zilfc, zu, zt, rdz, cxch
      real dthv, du2, btgh, wstar2, ustar, zslu, zslt, rlogu, rlogt
      real rlmo, zetalt, zetalu, zetau, zetat, xlu4, xlt4, xu4, xt4
      real xlu, xlt, xu, xt, psmz, simm, pshz, simh, ustark, rlmn, rlma

      integer itrmx, ilech, itr

                             parameter
     &(wwst=1.2,wwst2=wwst*wwst,g=9.8,vkrm=0.40,excm=0.001
     &,wold=.15,wnew=1.-wold,itrmx=25,pihf=3.14159265/2.
c-----------------------------------------------------------------------
     &,epsu2=1.e-4,epsust=0.07,epsit=1.e-4,epsa=1.e-8
     &,ztmin=-5.,ztmax=1.,sqvisc=258.2)

      parameter
     &(ric=0.183,rric=1.0/ric,fhneu=0.8,rfc=0.191
     &,rfac=ric/(fhneu*rfc*rfc))

c-----------------------------------------------------------------------
c **** lech's surface functions ****
      pslmu(zz)=-0.96*log(1.0-4.5*zz)
      pslms(zz)=zz*rric-2.076*(1.-1./(zz+1.))
      pslhu(zz)=-0.96*log(1.0-4.5*zz)
      pslhs(zz)=zz*rfac-2.076*(1.-1./(zz+1.))

c-----------------------------------------------------------------------
c **** paulson's surface functions *****
      pspmu(xx)=-2.*log((xx+1.)*0.5)-log((xx*xx+1.)*0.5)+2.*atan(xx)
     &          -pihf
      pspms(yy)=5.*yy
      psphu(xx)=-2.*log((xx*xx+1.)*0.5)
      psphs(yy)=5.*yy
c
      ilech=0
c***********************************************************************
c-----------------------------------------------------------------------
c ztfc: ratio of zoh/zom  less or equal than 1
cc......ztfc=0.1
c czil: constant c in zilitinkevich, s. s.1995,:note about zt
c commented out to allow argument-passed value
c      czil=0.2
      zilfc=-czil*vkrm*sqvisc
      elfc = vkrm*btg
c
c-----------------------------------------------------------------------
      zu=z0
cc.......zt=z0*ztfc
c-----------------------------------------------------------------------
      rdz=1./zlm
      cxch=excm*rdz
c-----------------------------------------------------------------------
      dthv=thlm-thz0
c
      du2=max(sfcspd*sfcspd,epsu2)
c--------------beljars correction of ustar------------------------------
      btgh=btg*hpbl
      wstar2=wwst2*abs(btgh*akhs*dthv)**(2./3.)
      ustar=max(sqrt(akms*sqrt(du2+wstar2)),epsust)
c--------------zilitinkevitch approach for zt--------------------------------
      zt=exp(zilfc*sqrt(ustar*z0))*z0
c-----------------------------------------------------------------------
        zslu=zlm+zu
        zslt=zlm+zt
c
        rlogu=log(zslu/zu)
        rlogt=log(zslt/zt)
c
        rlmo=elfc*akhs*dthv/ustar**3
c-----------------------------------------------------------------------
            do 100 itr=1,itrmx
c--------------1./monin-obukkhov length-scale---------------------------
        zetalt=max(zslt*rlmo,ztmin)
        rlmo=zetalt/zslt
        zetalu=zslu*rlmo
c
        zetau=zu*rlmo
        zetat=zt*rlmo
c-----------------------------------------------------------------------
         if(ilech.eq.0) then
          if(rlmo.lt.0.)then
            xlu4=1.-16.*zetalu
            xlt4=1.-16.*zetalt
            xu4 =1.-16.*zetau
            xt4 =1.-16.*zetat
c
            xlu=sqrt(sqrt(xlu4))
            xlt=sqrt(sqrt(xlt4))
            xu =sqrt(sqrt(xu4))
            xt =sqrt(sqrt(xt4))
c
            psmz=pspmu(xu)
            simm=pspmu(xlu)-psmz+rlogu
            pshz=psphu(xt)
            simh=psphu(xlt)-pshz+rlogt
          else
            zetalu=min(zetalu,ztmax)
            zetalt=min(zetalt,ztmax)
c
            psmz=pspms(zetau)
            simm=pspms(zetalu)-psmz+rlogu
            pshz=psphs(zetat)
            simh=psphs(zetalt)-pshz+rlogt
          endif
         else
c-----------------------------------------------------------------------
c ***** lech's functions ****
          if(rlmo.lt.0.)then
            psmz=pslmu(zetau)
            simm=pslmu(zetalu)-psmz+rlogu
            pshz=pslhu(zetat)
            simh=pslhu(zetalt)-pshz+rlogt
          else
            zetalu=min(zetalu,ztmax)
            zetalt=min(zetalt,ztmax)
c
            psmz=pslms(zetau)
            simm=pslms(zetalu)-psmz+rlogu
            pshz=pslhs(zetat)
            simh=pslhs(zetalt)-pshz+rlogt
          endif
         endif
c--------------beljaars correction for ustar----------------------------
        ustar=max(sqrt(akms*sqrt(du2+wstar2)),epsust)   !EGP
c--------------zilitinkevitch fix for zt--------------------------------
        zt=exp(zilfc*sqrt(ustar*z0))*z0

        zslt=zlm+zt
        rlogt=log(zslt/zt)
c-----------------------------------------------------------------------
        ustark=ustar*vkrm
        akms=max(ustark/simm,cxch)
        akhs=max(ustark/simh,cxch)
c-----------------------------------------------------------------------
        wstar2=wwst2*abs(btgh*akhs*dthv)**(2./3.)       ! EGP now no wstar2
c       ufree=0.07*abs(btgh*akhs*dthv)**(1./3.)         ! only use ufree
c
        rlmn=elfc*akhs*dthv/ustar**3
c-----------------------------------------------------------------------
        rlma=rlmo*wold+rlmn*wnew
c-----------------------------------------------------------------------
c      if(abs((rlmn-rlmo)/rlma).lt.epsit)    go to 110
c-----------------------------------------------------------------------
        rlmo=rlma
c-----------------------------------------------------------------------
  100   continue
c-----------------------------------------------------------------------
  110   continue
c-----------------------------------------------------------------------
        return
        end
      function csnow ( dsnow )
      
      implicit none

      real c
      real dsnow
      real csnow

      real unit
      parameter ( unit=0.11631 ) 
                                         
c   ****  simulation of termal snow conductivity                   
c   ****  simulation units of csnow is cal/(cm*hr* c) 
c   ****  and it will be returnd in w/(m* c)
c   ****  basic version is dyachkova equation                                

c *****   dyachkova equation (1960), for range 0.1-0.4

      c=0.328*10**(2.25*dsnow)
      csnow=unit*c

       
c *****    de vaux equation (1933), in range 0.1-0.6
c       csnow=0.0293*(1.+100.*dsnow**2)
      
c     *****   e. andersen from flerchinger
c     csnow=0.021+2.51*dsnow**2        
      
      return                                                      
      end  
      subroutine snow_new ( t,p,hc,ds )
      
      implicit none
      
c     calculating snow depth and densitity to account for the new snowfall
c     t - air temperature, k
c     p - new snowfall, m 
c     hc - snow depth, m
c     ds - snow density 
c     new values of snow depth & density will be returned   
      
      real hc
      real t 
      real p
      real ds
      real h
      real px
      real tx
      real ds0
      real hnew
      
c     **  conversion into simulation units   ************************* 
      
      h=hc*100.
      px=p*100.
      tx=t-273.15
      
c     ----------------------------------------------------------------------
c     ***  calculating new snowfall density depending on temperature      **
c     ***  equation from gottlib l. 'a general runoff model for snowcovered
c     ***  and glacierized basin', 6th nordic hydrological conference, 
c     ***  vemadolen, sweden, 1980, 172-177pp.
c-----------------------------------------------------------------------
      
      if(tx .le. -15.) then
         ds0=0.05
      else                                                      
         ds0=0.05+0.0017*(tx+15.)**1.5
      endif
      
c     **   adjustment of snow density depending on new snowfall      
      
      hnew=px/ds0
      if ((h+hnew) .gt. 0.0) then
         ds=(h*ds+hnew*ds0)/(h+hnew)
         h=h+hnew
      else
         ds=0.0
         h=0.0
      end if
      
      hc=h*0.01
      
      return
      end
      subroutine tdfcnd ( df, smc, b, f1, q,  smcmax, sh2o)

      implicit none

c               included new vars here: ,quartz,smcmax,sh2o
c______________________________________________________________
c   new calling arguments in sflx, hrt   10/98
c  (remove call tdfcnd (df, smc, bb(isoil), f11(isoil))
c
c           call tdfcnd (df, smc, bb(isoil), f11(isoil),
c     &                  quartz(isoil), smcmax(isoil), sh2o)
c--------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate thermal diffusivity and conductivity of
cc    =======   the soil for a given point and time.
cc
cc    version:  peters-lidard approach (peters-lidard et al., 1998)
cc    =======
cc                          added by    pablo j. grunmann, 08/1998
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       real b
       real df
       real f1
       real gammd
       real thkdry
       real ake
       real thkice
       real thko
       real thkqtz
       real thksat
       real thks
       real thkw
       real q
       real satratio
       real sh2o
       real smc
       real smcmax
       real xu
       real xunfroz


c in prmsoi:
c        data quartz /0.82, 0.10, 0.25, 0.60, 0.52, 
c     &              0.35, 0.60, 0.40, 0.82/

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     if the soil has any moisture content compute a partial sum/product
c     otherwise use a constant value which works well with most soils
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c use as in peters-lidard, 1998 (modif. from johansen, 1975).
c
c                                  pablo grunmann, 08/17/98
c refs.:
c      farouki, o.t.,1986: thermal properties of soils. series on rock 
c              and soil mechanics, vol. 11, trans tech, 136 pp.
c      johansen, o., 1975: thermal conductivity of soils. ph.d. thesis,
c              university of trondheim,
c      peters-lidard, c. d., et al., 1998: the effect of soil thermal 
c              conductivity parameterization on surface energy fluxes
c              and temperatures. journal of the atmospheric sciences,
c              vol. 55, pp. 1209-1224.
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c thkw  water thermal conductivity
c   thkqtz  thermal conductivity for quartz
c thko  thermal conductivity for other soil components
c thks  thermal conductivity for the solids combined(quartz + other)
c   thkice  ice thermal conductivity
c   smcmax  porosity (= smcmax)
c        q  quartz content(soil type)
c

c  needs parameters
c porosity(soil type):
c      poros = smcmax
c saturation ratio:
      satratio = smc/smcmax
c      print *, 'satratio=',satratio
c     parameters  w/(m.k)
      thkice = 2.2
      thkw = 0.57
      thko = 2.0
c      if (q .le. 0.2) thko = 3.0
      thkqtz = 7.7
c  solids' conductivity      
      thks = (thkqtz**q)*(thko**(1.- q))
c      print *, 'thks = ',thks
c  unfrozen fraction (from 1.0, i.e., 100%liquid, to 0.0 (100% frozen))
      xunfroz=(sh2o + 1.e-9)/(smc + 1.e-9)
c      print *, '   '
c      print *, 'xunfroz = ',xunfroz
c      print *, '    '
c  unfrozen volume for saturation (porosity*xunfroz)
      xu=xunfroz*smcmax 
c  saturated thermal conductivity
      thksat = thks**(1.-smcmax)*thkice**(smcmax-xu)*thkw**(xu)
c      print *, 'thksat = ',thksat
c  dry density in kg/m3
      gammd = (1. - smcmax)*2700.
c      print *, 'gammd = ',gammd
c  dry thermal conductivity in w.m-1.k-1
      thkdry = (0.135*gammd + 64.7)/(2700. - 0.947*gammd)
c      print *, 'thkdry = ',thkdry
c range of validity for the kersten number
      if ( satratio .gt. 0.1 ) then

c    kersten number (fine formula, at least 5% of particles<(2.e-6)m)
           if ( (xunfroz + 0.0005) .lt. smc ) then
c    frozen
              ake = satratio
           else
c    unfrozen
              ake = log10(satratio) + 1.0
           endif

      else
        
c use k = kdry
        ake = 0.0
c        print *, 'ake (else) = ',ake
      endif
c  thermal conductivity
       df = ake*(thksat - thkdry) + thkdry

      return
      end
      subroutine penman(sfctmp,sfcprs,ch,t2v,th2,prcp,f,t24,s,q2,
     &                  q2sat,etp,rch,epsca,rr,snowng,frzgra,dqsdt2)

      implicit none

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate potential evaporation for the current point.
cc    =======   various partial sums/products are also calculated and
cc              passed back to the calling routine for later use.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      logical snowng
      logical frzgra

      real a
      real beta
      real ch
      real cp
      real cph2o
      real cpice
      real delta
      real dew
      real drip
      real ec
      real edir
      real elcp
      real epsca
      real etp
      real ett
      real f
      real flx1
      real flx2
      real flx3
      real fnet
      real lsubc
      real lsubf
      real prcp
      real q2
      real q2sat
      real r
      real rad
      real rch
      real rho
      real rib
      real rr
      real runoff,runoxx3
      real s
      real sfcprs
      real sfctmp
      real sigma
      real t24
      real t2v
      real th2
      real dqsdt2

cmic$ taskcommon rite
!$omp threadprivate(/rite/)
      common/rite/ beta,drip,ec,edir,ett,flx1,flx2,flx3,runoff,
     &             dew,rib,runoxx3

      parameter(cp=1004.6,cph2o=4.218e+3,cpice=2.106e+3,r=287.04,
     &   elcp=2.4888e+3,lsubf=3.335e+5,lsubc=2.5e+6,sigma=5.67e-8)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     executable code begins here...
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      flx2 = 0.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     prepare partial quantities for penman equation.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      delta = elcp * dqsdt2
      t24 = sfctmp * sfctmp * sfctmp * sfctmp
      rr = t24 * 6.48e-8 / ( sfcprs * ch ) + 1.0
      rho = sfcprs / ( r * t2v )
      rch = rho * cp * ch

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     adjust the partial sums / products with the latent heat
c     effects caused by falling precipitation.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( .not. snowng ) then
        if ( prcp .gt. 0.0 ) rr = rr + cph2o * prcp / rch
      else
        rr = rr + cpice * prcp / rch
      endif

      fnet = f - sigma * t24 - s

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     include the latent heat effects of frzng rain converting to
c     ice on impact in the calculation of flx2 and fnet.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( frzgra ) then
        flx2 = -lsubf * prcp
        fnet = fnet - flx2
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     finish penman equation calculations.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      rad = fnet / rch + th2 - sfctmp
      a = elcp * ( q2sat - q2 )
      epsca = ( a * rr + rad * delta ) / ( delta + rr )
      etp = epsca * rch / lsubc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    !!!!!!!!!!!! test drainage -> etp = 0.0 !!!!!!!!!!!!!!!!
c      etp = 0.0
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      return
      end
      subroutine canres(solar,ch,sfctmp,q2,sfcprs,smc,zsoil,nsoil,
     &                  smcwlt,smcref,rcmin,rc,pc,nroot,q2sat,dqsdt2, 
     &                  topt,rsmax,rgl,hs,xlai)

      implicit none

c **********************************************************************
c                        subroutine canres
c                        -----------------
c       this routine calculates the canopy resistance which depends
c       on incoming solar radiation, air temperature, atmospheric
c       water vapor pressure deficit at the lowest model level, ans soil
c       moisture.
c ----------------------------------------------------------------------
c        source:  jarvis (1976), jacquemin and noilhan (1990 blm)
c ----------------------------------------------------------------------
c ----------------------------------------------------------------------
c        input:   solar: incoming solar radiation
c                ch: surface exchange coefficient for heat and moisture
c                sfctmp: air temperature at 1st level
c                q2:  air humidity at 1st level
c                sfcprs: surface pressure
c                smc: volumetric soil moisture 
c                zsoil: soil depth
c                nsoil: number of soil layers
c                smcwlt: wilting point
c                smcref: reference soil moisture (field capacity)
c
c        output:  pc: plant coefficient
c                rc: canopy resistance
c ----------------------------------------------------------------------
c **********************************************************************

      integer   nsold, nroot, nsoil, k
      parameter (nsold = 20)

      real sigma, rd, cp, slv
      real solar, ch, sfctmp, q2, sfcprs 
      real smc(nsoil), zsoil(nsoil), part(nsold) 
      real smcwlt, smcref, rcmin, rc, pc, q2sat, dqsdt2
      real topt, rsmax, rgl, hs, xlai, rcs, rct, rcq, rcsoil, ff
      real p, qs, gx, tair4, st1, slvcp, rr, delta

      parameter (sigma=5.67e-8, rd=287.0, cp=1004.5, slv=2.5e6)

      rcs = 0.0
      rct = 0.0
      rcq = 0.0
      rcsoil = 0.0
      rc = 0.0

c ----------------------------------------------------------------------
c contribution due to incoming solar radiation.
c ----------------------------------------------------------------------

cc/98/01/05/       ff = 0.55*2.0*solar/rgl
      ff = 0.55*2.0*solar/(rgl*xlai)
      rcs = (ff + rcmin/rsmax) / (1.0 + ff)
      rcs = max(rcs,0.0001)

c ----------------------------------------------------------------------
c contribution due to the temperature at the first model level.
c ----------------------------------------------------------------------

      rct = 1.0 - 0.0016*((topt-sfctmp)**2.0)
      rct = max(rct,0.0001)

c ----------------------------------------------------------------------
c contribution due to vapor pressure deficit at the first model level.
c ----------------------------------------------------------------------

      p = sfcprs
      qs = q2sat
c rcq expression from ssib 
      rcq = 1.0/(1.0+hs*(qs-q2))
      rcq = max(rcq,0.01)

c ----------------------------------------------------------------------
c contribution due to soil moisture availability.
c determine contribution from each soil layer, then add them up.
c ----------------------------------------------------------------------

      gx = (smc(1) - smcwlt) / (smcref - smcwlt)
      if (gx .gt. 1.) gx = 1.
      if (gx .lt. 0.) gx = 0.

c****   using soil depth as weighting factor
      part(1) = (zsoil(1)/zsoil(nroot)) * gx

c**** using root distribution as weighting factor
cc      part(1) = rtdis(1) * gx
      
      do 10 k = 2, nroot
        gx = (smc(k) - smcwlt) / (smcref - smcwlt)
        if (gx .gt. 1.) gx = 1.
        if (gx .lt. 0.) gx = 0.
c****   using soil depth as weighting factor        
        part(k) = ((zsoil(k)-zsoil(k-1))/zsoil(nroot)) * gx

c**** using root distribution as weighting factor
cc         part(k) = rtdis(k) * gx 
               
10    continue

      do 20 k = 1, nroot
        rcsoil = rcsoil+part(k)

20    continue
        rcsoil = max(rcsoil,0.0001)

c ----------------------------------------------------------------------
c         determine canopy resistance due to all factors.
c         convert canopy resistance (rc) to plant coefficient (pc).
c ----------------------------------------------------------------------

cc/98/01/05/          rc = rcmin/(rcs*rct*rcq*rcsoil)
          rc = rcmin/(xlai*rcs*rct*rcq*rcsoil)

          tair4 = sfctmp**4
          st1 = (4.*sigma*rd)/cp
          slvcp = slv/cp
          rr = st1*tair4/(sfcprs*ch) + 1.0
          delta = slvcp*dqsdt2

          pc = (rr+delta)/(rr*(1.+rc*ch)+delta)

          return
          end
      subroutine nopac ( etp, eta, prcp, smc, smcmax, smcwlt,
     &                   smcref,smcdry,cmc,cmcmax, nsoil, dt, shdfac,
     &                   sbeta,
     &                   q1, q2, t1, sfctmp, t24, th2, f, f1, s, stc,
     &                   epsca, b, pc, rch, rr,  cfactr, 
     &                   sh2o, slope, kdt, frzfact, psisat,
     &                   zsoil, dksat, dwsat, tbot, runoff1, runoff2,
     &                   runoff3, edir1, ec1, ett1, nroot, ice,rtdis,
     &                   quartz, fxexp,csoil)

      implicit none

c -----------   frozen ground version     ---------------------------
c   new states added:  sh2o,  and frozen ground correction factor
     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate soil moisture and heat flux values and update
cc    =======   soil moisture content and soil heat content values for
cc              the case when no snow pack is present.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsoil

      real b
      real beta
      real cfactr
      real cmc
      real cmcmax
      real cp
      real csoil
      real dew
      real df1
      real dksat
      real drip
      real dt
      real dwsat
      real ec
      real edir
      real epsca
      real eta
      real eta1
      real etp
      real etp1
      real ett
      real f
      real f1
      real fxexp
      real flx1
      real flx2
      real flx3
      real kdt
      real pc
      real prcp
      real prcp1
      real q2
      real rch
      real rib
      real rr
      real rtdis (nsoil)
      real runoff,runoxx3
      real s
      real sbeta
      real sfctmp
      real shdfac
      real sigma
      real smc   ( nsoil )
      real sh2o  ( nsoil )
      real smcdry
      real smcmax
      real smcref
      real smcwlt
      real stc   ( nsoil )
      real t1
      real t24
      real tbot
      real th2
      real yy
      real yynum
      real zsoil ( nsoil )
      real zz1

      real q1, slope, frzfact, psisat, runoff1, runoff2, runoff3
      real edir1, ec1, ett1, quartz, sice1

      integer nroot, ice

cmic$ taskcommon rite
!$omp threadprivate(/rite/)
      common/rite/ beta,drip,ec,edir,ett,flx1,flx2,flx3,runoff,
     &             dew,rib,runoxx3
      parameter(cp=1004.5, sigma=5.67e-8)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     executable code begins here.....
c     convert etp from kg m-2 s-1 to ms-1 and initialize dew.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      prcp1 = prcp * 0.001
      etp1 = etp * 0.001
      dew = 0.0

      if ( etp .gt. 0.0 ) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       convert prcp from  kg m-2 s-1  to  m s-1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c ------------    frozen ground version    --------------------------
c (+) new states added: sh2o,  and frozen ground correction factor
c
           call smflx ( eta1,smc,nsoil,cmc,etp1,dt,prcp1,zsoil,
     +             sh2o, slope, kdt, frzfact,
     &          smcmax,b,pc,smcwlt,dksat,dwsat,smcref,shdfac,
     &          cmcmax,smcdry,cfactr, runoff1,runoff2, runoff3, 
     &          edir1, ec1, ett1, sfctmp,q2,nroot,rtdis, fxexp)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       convert modeled evapotranspiration fm  m s-1  to  kg m-2 s-1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        eta = eta1 * 1000.0

      else

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       if etp < 0, assume dew forms (transform etp1 into dew
c       and reinitialize etp1 to zero)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        dew = -etp1
        etp1 = 0.0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       convert prcp from  kg m-2 s-1  to  m s-1  and add dew amt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        prcp1 = prcp1 + dew
c
        
c ------------    frozen ground version    --------------------------
c (+) new states added: sh2o,  and frozen ground correction factor
c
        call smflx ( eta1,smc,nsoil,cmc,etp1,dt,prcp1,zsoil,
     +             sh2o, slope, kdt, frzfact,
     &          smcmax,b,pc,smcwlt,dksat,dwsat,smcref,shdfac,
     &          cmcmax,smcdry,cfactr, runoff1,runoff2, runoff3, 
     &          edir1, ec1, ett1, sfctmp, q2, nroot,rtdis, fxexp)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       convert modeled evapotranspiration fm  m s-1  to  kg m-2 s-1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        eta = eta1 * 1000.0

      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     based on etp and e values, determine beta
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( etp .le. 0.0 ) then
        beta = 0.0
        if ( etp .lt. 0.0 ) then
          beta = 1.0
          eta = etp
        endif
      else
        beta = eta / etp
      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc mixing ratio at grnd level (skin)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccc....q1 = q2 + eta * cp / rch
c EGP
      q1 = q2 + eta * cp / rch

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     get soil thermal diffuxivity/conductivity for top soil lyr,
c     calc adjusted top lyr soil temp and adjusted soil flux, then
c     call shflx to compute/update soil heat flux and soil temps.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c pps added subscript to sh2o
      call tdfcnd ( df1, smc(1), b, f1,quartz,smcmax,sh2o(1) )
c veg effect in thermal cond. (reduction), 7 may 1999, m. ek
      df1 = df1 * exp(sbeta*shdfac)

c --------------    frozen ground version     ----------------------
c   thermal conductivity adjustment
c
      sice1 = smc(1) - sh2o(1)
c__________________________________________________________________
c this if-statement is no longer used if peters-lidard thermal
c conductivity is used for all conditions (frozen,unfrozen):
c      if ( sice1 .gt. 0.0 ) call frzcnd ( df1, sice1 )            

      yynum = f - sigma * t24
      yy = sfctmp + (yynum/rch+th2-sfctmp-beta*epsca) / rr
      zz1 = df1 / ( -0.5 * zsoil(1) * rch * rr ) + 1.0

c ------------    frozen ground version    --------------------------
c (+) new states added: sh2o & parameter smcwlt & psisat
c
      call shflx ( s,stc,smc,smcmax,nsoil,t1,dt,yy,zz1,zsoil,tbot,
     +             smcwlt, psisat, sh2o,
     &             b,f1,df1, ice, 
     &             quartz,csoil)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     set flx1, and flx3 to zero since they are not used.  flx2
c     was similarly initialized in the penman routine.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      flx1 = 0.0
      flx3 = 0.0
c
      return
      end
      function devap ( etp1, smc, zsoil, shdfac, smcmax, b,
     &                 dksat, dwsat, smcdry, smcref, smcwlt, fxexp)

      implicit none

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    name:  direct evaporation (devap) function  version: n/a
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real b
      real devap
      real dksat
      real dwsat
      real etp1
      real fx
      real fxexp
      real shdfac
      real smc
      real smcdry
      real smcmax
ccccc real wcnd
ccccc real wdf
      real zsoil
      real smcref
      real smcwlt

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     get soil h2o diffusivity and hydraulic conductivity given smc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c fx      call wdfcnd ( wdf, wcnd, smc, smcmax, b, dksat, dwsat )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     fx represents soil moisture flux / atmospheric demand
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c fx     fx = ( -2. * wdf * ( smc - smcdry ) / zsoil - wcnd ) / etp1
c*** test of beta methode, f. chen 7/11/96 ***
c change 1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! formulation to reduce 
c excessive evaporation under little green vegetation:

c      fx = (smc - smcwlt) / (smcref - smcwlt)
c      fx = (smc - smcwlt) / (smcmax - smcwlt)
c      fx = (smc - smcdry) / (smcmax - smcdry)
ccccc  fxexp = 2.0
c      fx = ( (smc - smcdry) / (smcmax - smcdry) )**fxexp
      fx = ( (smc - smcwlt) / (smcmax - smcwlt) )**fxexp

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     fx > 1 represents demand control
c     fx < 1 represents flux control
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      fx = max ( min ( fx, 1. ) ,0. )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     allow for the direct-evap-reducing effect of shade
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      devap = fx * ( 1.0 - shdfac ) * etp1
      return
      end
      subroutine transp (et,nsoil,etp1,smc,cmc,zsoil,shdfac,smcwlt,
     &      cmcmax,pc,cfactr,smcref,sfctmp,q2,nroot,rtdis)

      implicit none

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate transpiration from the vegtyp for this pt.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer i, k
      integer nsoil
      integer nroot

      real cfactr
      real cmc
      real cmcmax
      real et ( nsoil )
      real etp1
      real etp1a
      real gx (7)
c      real part (nsoil)
      real pc
      real rtdis (nsoil)
      real shdfac
      real smc    ( nsoil )
      real smcref
      real smcwlt
      real zsoil  ( nsoil )

      real sfctmp, q2, sgx, denom, rtx

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       initialize  plant transp to zero for all soil layers.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do 20 k = 1, nsoil
         et(k) = 0.
 20   continue

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calc an 'adjusted' potntl transpiration
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      etp1a = shdfac * pc * etp1 * (1.0 - (cmc /cmcmax) ** cfactr)
                  
      sgx = 0.0
      do i = 1, nroot
         gx(i) = ( smc(i) - smcwlt ) / ( smcref - smcwlt )
         gx(i) = max ( min ( gx(i), 1. ), 0. )
         sgx = sgx + gx (i)
      end do
      sgx = sgx / nroot
      
      denom = 0.
      do i = 1,nroot
         rtx = rtdis(i) + gx(i) - sgx
         gx(i) = gx(i) * max ( rtx, 0. )
         denom = denom + gx(i)
      end do   
      if ( denom .le. 0.0) denom = 1.
      
      do i = 1, nroot
         et(i) = etp1a * gx(i) / denom
      end do  
      
c     ************    below is old code    ************
c     et(1) = ( zsoil(1) / zsoil(nroot) ) * gx * etp1a
c     c        et(1) = ( zsoil(1) / zsoil(nroot) ) * etp1a
      
c***  using root distribution as weighting factor
c     c        et(1) = rtdis(1) * etp1a
c     c       et(1) =  etp1a*part(1)
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     loop down thru the soil layers repeating the operation above,
c     but using the thickness of the soil layer (rather than the
c     absolute depth of each layer) in the final calculation.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
c     c        do 10 k = 2, nroot
      
c     gx = ( smc(k) - smcwlt ) / ( smcref - smcwlt )
c     gx = max ( min ( gx, 1. ), 0. )
c     test canopy resistance
c     gx = 1.0
      
c     et(k) = ((zsoil(k)-zsoil(k-1))/zsoil(nroot))*gx*etp1a
c     c         et(k) = ((zsoil(k)-zsoil(k-1))/zsoil(nroot))*etp1a
      
c***  using root distribution as weighting factor
c     c         et(k) = rtdis(k) * etp1a
c     c         et(k) = etp1a*part(k)
c     c  10    continue
      
      return
      end
      subroutine srt (rhstt,runoff,edir,et,sh2o,sh2oa,nsoil,pcpdrp,
     &                 zsoil,dwsat,dksat,smcmax,b, runoff1, 
     +                 runoff2,dt,smcwlt,slope,kdt,frzx,sice)      

      implicit none

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate the right hand side of the time tendency
cc    =======   term of the soil water diffusion equation.  also to
cc              compute ( prepare ) the matrix coefficients for the
cc              tri-diagonal matrix of the implicit time scheme.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      logical isfrozen
      
      integer nsold
      parameter ( nsold = 20 )
      integer k
      integer nsoil
      integer cvfrz
      real ai     ( nsold )
      real b
      real bi     ( nsold )
      real ci     ( nsold )
      real dmax   ( nsold )
      real ddz
      real ddz2
      real denom
      real denom2
      real dksat
      real dsmdz
      real dsmdz2
      real dwsat
      real edir
      real et     ( nsoil )
      real infmax
      real kdt
      real mxsmc
      real mxsmc2
      real numer
      real pcpdrp
      real pddum
      real rhstt  ( nsoil )
      real runoff
      
      real sh2o   ( nsoil )
      real sh2oa  ( nsoil )
      real sice   ( nsoil )
      
      real smcmax
      real wcnd
      real wcnd2
      real wdf
      real wdf2
      real zsoil  ( nsoil )

      real runoff1, runoff2, dt, smcwlt, slope, frzx, dt1
      real smcav, dice, dd, val, ddt, px, fcr, acrt, sum
      real sstt, slopx

      integer iohinf, ks, ialp1, j, jj

cmic$ taskcommon abci
!$omp threadprivate(/abci/)
      common /abci/ ai, bi, ci

c -----------     frozen ground version    -------------------------
c   reference frozen ground parameter, cvfrz, is a shape parameter of
c   areal distribution function of soil ice content which equals 1/cv.
c   cv is a coefficient of spatial variation of soil ice content. 
c   based on field data cv depends on areal mean of frozen depth, and it
c   close to constant = 0.6 if areal mean frozen depth is above 20 cm.
c   that is why parameter cvfrz = 3 (int{1/0.6*0.6})  
c
        parameter ( cvfrz = 3 )
c ------------------------------------------------------------------
     
c        print*,'in srt, declaration -----------------------'
c        print*,'nsoil=' , nsoil
c        print*,'nsold=' , nsold
        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     determine rainfall infiltration rate and runoff
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c ************  include the infiltration formule from schaake and koren 
c           model
c
cc    modified by q duan
cc    replace the maximum infiltration equation in osu model
cc    by schaake/koren expression
cc
cc    initialize parameters and variables
cc      
      iohinf=1

c let isfrozen be "true" if any frozen water content within soil layers.
      isfrozen = .false.
      do ks=1,nsoil
       if (sice(ks) .gt. 0.0001) isfrozen = .true.
      end do

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     determine rainfall infiltration rate and runoff
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      pddum = pcpdrp
      runoff1 = 0.0
      if ( pcpdrp .ne. 0.0 ) then

cc++  modified by q. duan, 5/16/94

c        if (iohinf .eq. 1) then
  
          dt1 = dt/86400.
          smcav = smcmax - smcwlt
          dmax(1)=-zsoil(1)*smcav

c -----------     frozen ground version    ------------------------
c
          dice = -zsoil(1) * sice(1)
c-------------------------------------------------------------------
          
          dmax(1)=dmax(1)*(1.0 - (sh2oa(1)+sice(1)-smcwlt)/smcav)
          dd=dmax(1)
      do ks=2,nsoil
          
c -----------     frozen ground version    ------------------------
c
           dice = dice + ( zsoil(ks-1) - zsoil(ks) ) * sice(ks)
c------------------------------------------------------------------- 
         
           dmax(ks)=(zsoil(ks-1)-zsoil(ks))*smcav
           dmax(ks)=dmax(ks)*(1.0 - (sh2oa(ks)+sice(ks)-smcwlt)/smcav)
           dd=dd+dmax(ks)
      end do
cc          val = (1.-exp(-kdt*sqrt(dt1)))
c remove the sqrt by f. chen, 07/08/96
          val = (1.-exp(-kdt*dt1))
          ddt = dd*val
          px = pcpdrp*dt  
          if(px.lt.0.0) px = 0.0
          infmax = (px*(ddt/(px+ddt)))/dt
          
c -----------     frozen ground version    --------------------------
c    reduction of infiltration based on frozen ground parameters
c
         fcr = 1. 
         if ( dice .gt. 1.e-2) then 
           acrt = cvfrz * frzx / dice 
           sum = 1.
           ialp1 = cvfrz - 1 
           do j = 1,ialp1
              k = 1
              do jj = j+1, ialp1
                k = k * jj
              end do   
              sum = sum + (acrt ** ( cvfrz-j)) / float (k) 
           end do 
           fcr = 1. - exp(-acrt) * sum 
         end if 
         infmax = infmax * fcr
c -------------------------------------------------------------------

c ************    correction of infiltration limitation    **********
c     if infmax .le. hydrolic conductivity assign infmax the 
c     value of hydrolic conductivity
c
c         mxsmc = max ( sh2oa(1), sh2oa(2) ) 
        mxsmc = sh2oa(1)

c        print*,'srt, before wdfcnd - 1 ------------------------------'
c        print*,'mxsmc,smcmax=' , mxsmc,smcmax
c        print*,'b,dksat,dwsat=' , b,dksat,dwsat
 
         call wdfcnd ( wdf,wcnd,mxsmc,smcmax,b,dksat,dwsat,
     &isfrozen )
            infmax = max(infmax, wcnd)
            infmax= min(infmax,px)

c        print*,'srt, after wdfcnd - 1 ------------------------------'
c        print*,'wdf,wcnd=' , wdf,wcnd
c        print*,'mxsmc,smcmax=' , mxsmc,smcmax
c        print*,'b,dksat,dwsat=' , b,dksat,dwsat
 
c  ******************************************************************
c          infmax = min(infmax, dksat)
 
c        else

c          infmax = -dwsat*(smcmax-smca(1))/(0.5*zsoil(1))+dksat
c          if (smca(1).ge.smcmax) infmax = dksat
c
c        end if
c
c*************** runoff1: surface runoff *******
c
          if ( pcpdrp .gt. infmax ) then
            runoff1 = pcpdrp - infmax
            pddum = infmax
          end if

      end if
c
c*********   below is the original osu model
c
c      pddum = pcpdrp
c      print*,' ************** in srt *******************'
c      print*,' pcpdrp=', pcpdrp
c      runoff1 = 0.0
c      if ( pcpdrp .ne. 0.0 ) then
c        infmax = - dwsat * (smcmax - smca(1))/(0.5 * zsoil(1)) + dksat
c        if ( smca(1) .ge. smcmax ) infmax = dksat
c        if ( pcpdrp .gt. infmax ) then
c          runoff1 = pcpdrp - infmax
c
c*************** runoff1: surface runoff *******
c
c          pddum = infmax
c        end if
c      end if


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     determine soil moisture diffusivity (wdf) & conductivity (wcnd)
c     coefs for top soil lyr using the higher of smc for the top
c     soil layer or smc for the next lower soil layer.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c_________________________________________________________________
c comments on august 98 changes: 
c
c this approach disregarded the actual soil moisture in 
c the upper layer if it was less that that of the next, for 
c hydraulic conductivity and diffussivity purposes (supposedly
c to solve problems with the numerical discretization on a very 
c coarse vertical resolution setting - two layer model).
c in our experience, this would actually allow the upper 
c layer to go very dry at a fast rate while the next layer
c keeps  unchanged (except for diffusivity effect)
c
c                                      pablo j. grunmann
c 
c      mxsmc = max( sh2oa(1), sh2oa(2) )
c___________________________________________________________
c test a less drastic version of mxsmc: !!!!!!!!!!!!!!!!!!!!
c      mxsmc = max( sh2oa(1), (sh2oa(2)+sh2oa(1))/2.0 )
c___________________________________________________________
c_________________________________________________________________
c new approach: dif/conductivity parameters on upper layer
c depend on upper layer only
        mxsmc =  sh2oa(1)

c        print*,'srt, before wdfcnd - 2'
c        print*,'mxsmc,smcmax=' , mxsmc,smcmax
c        print*,'b,dksat,dwsat=' , b,dksat,dwsat
 

      call wdfcnd ( wdf,wcnd,mxsmc,smcmax,b,dksat,dwsat,
     &isfrozen )


c        print*,'srt, after wdfcnd - 2'
c        print*,'wdf,wcnd=' , wdf,wcnd
c        print*,'mxsmc,smcmax=' , mxsmc,smcmax
c        print*,'b,dksat,dwsat=' , b,dksat,dwsat
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc the matrix coefficients ai, bi, and ci for the top layer
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz = 1. / ( -.5 * zsoil(2) )
      ai(1) = 0.0
      bi(1) = wdf * ddz / ( -zsoil(1) )
      ci(1) = -bi(1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc rhstt for the top layer after calc'ng the vertical soil
c     moisture gradient btwn the top and next to top layers.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      dsmdz = ( sh2o(1) - sh2o(2) ) / ( -.5 * zsoil(2) )
      rhstt(1) = (wdf * dsmdz + wcnd - pddum + edir + et(1))/zsoil(1)
      sstt = wdf * dsmdz + wcnd + edir + et(1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     initialize ddz2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz2 = 0.0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     loop thru the remaining soil layers, repeating the abv process
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do 10 k = 2 , nsoil
        denom2 = ( zsoil(k-1) - zsoil(k) )
        if ( k .ne. nsoil ) then

        slopx = 1.
        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       retrieve the soil water diffusivity for the wetter of the
c       current or the next lower soil layer
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c          mxsmc2 = max ( smca(k), smca(k+1) )

cc           mxsmc2 = max ( sh2oa(k), sh2oa(k+1) )
 
c ************ pilps test *************
          mxsmc2 =  sh2oa(k)

c        print*,'srt, before wdfcnd - 3'
c        print*,'mxsmc2,smcmax=' , mxsmc2,smcmax
c        print*,'b,dksat,dwsat=' , b,dksat,dwsat
c        print*,'k=' , k
 

      call wdfcnd ( wdf2,wcnd2,mxsmc2,smcmax,b,dksat,dwsat,
     &isfrozen )


c        print*,'srt, after wdfcnd - 3'
c        print*,'wdf2,wcnd2=' , wdf2,wcnd2
c        print*,'mxsmc2,smcmax=' , mxsmc2,smcmax
c        print*,'b,dksat,dwsat=' , b,dksat,dwsat
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calc some partial products for later use in calc'ng rhstt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          denom = ( zsoil(k-1) - zsoil(k+1) )
          dsmdz2 = ( sh2o(k) - sh2o(k+1) ) / ( denom * 0.5 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         calc the matrix coef, ci, after calc'ng its partial product
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          ddz2 = 2.0 / denom
          ci(k) = -wdf2 * ddz2 / denom2
        else

c   slope of bottom layer is introduced     ************
c
          slopx = slope
c--------------------------------------------------------
          
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         retrieve the soil water diffusivity and hydraulic
c         conductivity for this layer
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c        print*,'srt, before wdfcnd - 4'
c        print*,'sh2oa(nsoil),smcmax=' , sh2oa(nsoil),smcmax
c        print*,'b,dksat,dwsat=' , b,dksat,dwsat
c        print*,'k=' , k
 
      call wdfcnd ( wdf2,wcnd2,sh2oa(nsoil),smcmax,
     &b,dksat,dwsat,isfrozen )

c        print*,'srt, after wdfcnd - 4'
c        print*,'wdf2,wcnd2=' , wdf2,wcnd2
c        print*,'sh2oa(nsoil),smcmax=' , sh2oa(nsoil),smcmax
c        print*,'b,dksat,dwsat=' , b,dksat,dwsat
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         calc a partial product for later use in calc'ng rhstt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          dsmdz2 = 0.0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         set matrix coef ci to zero
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          ci(k) = 0.0
        end if

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calc rhstt for this layer after calc'ng its numerator
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        numer = (wdf2 * dsmdz2) + slopx * wcnd2 - (wdf * dsmdz) 
     +          - wcnd + et(k)
        rhstt(k) = numer / (-denom2)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calc matrix coefs, ai, and bi for this layer
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ai(k) = -wdf * ddz / denom2
        bi(k) = -( ai(k) + ci(k) )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       reset values of wdf, wcnd, dsmdz, and ddz for loop to next lyr
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        if(k.eq.nsoil) then
c*************** runoff2: ground water runoff ***********
           runoff2 = slopx * wcnd2
        endif

        if ( k .ne. nsoil ) then
          wdf = wdf2
          wcnd = wcnd2
          dsmdz = dsmdz2
          ddz = ddz2
        end if
 10   continue

c        print*,'srt, final runoff'
c        print*,'runoff1=' , runoff1
c        print*,'runoff2=' , runoff2
 
      return
      end
      subroutine wdfcnd ( wdf,wcnd,smc,smcmax,b,dksat,dwsat,
     &isfrozen )

      implicit none

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate soil water diffusivity and soil
cc    =======   hydraulic conductivity.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical isfrozen
      
      real b
      real dksat
      real dwsat
      real expon
      real factr1
      real factr2
      real smc
      real smcmax
      real wcnd
      real wdf

c        print*,'------------ in wdfcnd -------------------------------'
c        print*,'before wdfcnd'
c        print*,'b=',b
c        print*,'dksat=',dksat
c        print*,'dwsat=',dwsat
c        print*,'expon=',expon
c        print*,'factr2=',factr2
c        print*,'smc=',smc
c        print*,'smcmax=',smcmax
c        print*,'wcnd=',wcnd
c        print*,'wdf=',wdf
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc the ratio of the actual to the max psbl soil h2o content
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      smc = smc
      smcmax = smcmax
      factr1 = 0.2 / smcmax
      factr2 = smc / smcmax

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     prep an expntl coef and calc the soil water diffusivity
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      expon = b + 2.0
      wdf = dwsat * factr2 ** expon
c  drainage tests !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      wdf = 0.0

c frozen drainage problem !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c comment the if-statement below out to run original vkoren
c frozen soil hydraulic diffusivity.
c                                         pablo grunmann, 06/15/99.

c version d_10cm: ........  factr1 = 0.2/smcmax
      if (isfrozen)  wdf = dwsat*factr1**expon

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     reset the expntl coef and calc the hydraulic conductivity
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      expon = ( 2.0 * b ) + 3.0
      wcnd = dksat * factr2 ** expon

c        print*,' wdfcnd results --------------------------------'
c        print*,'b=',b
c        print*,'dksat=',dksat
c        print*,'dwsat=',dwsat
c        print*,'expon=',expon
c        print*,'factr2=',factr2
c        print*,'smc=',smc
c        print*,'smcmax=',smcmax
c        print*,'wcnd=',wcnd
c        print*,'wdf=',wdf
c      print*,' smc         wdf           wcnd             b'
c      print*,smc,wdf,wcnd,b

      return
      end
      subroutine sstep ( sh2oout, sh2oin, cmc, rhstt, rhsct, dt,
     &              nsoil, smcmax, cmcmax, runoff3, zsoil,smc,sice )

      implicit none

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate/update the soil moisture content values
cc    =======   and the canopy moisture content values.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsold, i
      parameter ( nsold = 20 )
      integer k 
      integer kk11
      integer nsoil
      real ai     ( nsold )
      real bi     ( nsold )
      real ci     ( nsold )
      real cmc
      real cmcmax
      real dt
      real rhsct
      real rhstt  ( nsoil )

c --------     frozen ground corrections     ------------------      
      real sh2oin  ( nsoil )
      real sh2oout ( nsoil )
      real sice    ( nsoil )
      real smc     ( nsoil )
c --------------------------------------------------------------
      
      real smcmax
      real zsoil(nsoil)

      real runoff3, runofs, wplus, ddz, stot, wfree, dplus


cmic$ taskcommon abci
!$omp threadprivate(/abci/)
      common /abci/ ai, bi, ci

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     create 'amount' values of variables to be input to the
c     tri-diagonal matrix routine.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do 10 k = 1 , nsoil
        rhstt(k) = rhstt(k) * dt
        ai(k) = ai(k) * dt
        bi(k) = 1. + bi(k) * dt
        ci(k) = ci(k) * dt
 10   continue

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     call rosr12 to solve the tri-diagonal matrix
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      call rosr12 ( ci, ai, bi, ci, rhstt, rhstt, nsoil )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     sum the previous smc value and the matrix solution to get a
c     new value.  min allowable value of smc will be 0.02.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c   ****************** runoff3: runoff within soil layers *******

c ***********   water balance calculations were wrong    ********
c   it was corrected during during including frozen ground   ****
c   
      runofs = 0.0
      wplus = 0.0
      runoff3 = 0.
      ddz = - zsoil(1)
      
      do 20 k = 1 , nsoil
        if ( k .ne. 1 ) ddz = zsoil(k - 1) - zsoil(k)
        sh2oout(k) = sh2oin(k) + ci(k) + wplus / ddz
        
c        print*,'in sstep'
c        print*,'sh2oout=', sh2oout
        
        stot = sh2oout(k) + sice(k)
        if ( stot .gt. smcmax ) then
           if ( k .eq. 1 ) then
              ddz = -zsoil(1)
           else
              kk11 = k - 1
              ddz = -zsoil(k) + zsoil(kk11)
           end if
           wplus = ( stot - smcmax ) * ddz
         else
           wplus = 0.
         end if
         smc(k) = max ( min( stot, smcmax ), 0.02 )

         sh2oout(k) = max ( (smc(k) - sice(k)), 0.0 )

   20 continue

c  ***  v. koren   9/01/98    ******
c     water balance checking upward
c
      if(wplus .gt. 0.) then
       do i=nsoil-1,1,-1
        if(i .eq. 1) then
         ddz=-zsoil(1)
        else
         ddz=-zsoil(i)+zsoil(i-1)
        endif
        wfree=(smcmax-sh2oout(i)-sice(i))*ddz
        dplus=wfree-wplus
        if(dplus .ge. 0.) then
         sh2oout(i)=sh2oout(i)+wplus/ddz
         smc(i)=sh2oout(i)+sice(i)
         wplus=0.
           
        else
         sh2oout(i)=sh2oout(i)+wfree/ddz
         smc(i)=sh2oout(i)+sice(i)
         wplus=-dplus
        endif
       end do
30     runoff3=wplus
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     convert rhsct to an 'amount' value and add to previous
c     cmc value to get new cmc value.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      cmc = cmc + dt * rhsct
c      cmc = min ( max ( cmc, 0. ), cmcmax )
c
c code changed 8-26-98 by manikin (on original source)
c
      if (cmc .lt. 1.e-20) cmc=0.0
      cmc = min(cmc,cmcmax)


      return
      end
      subroutine rosr12 ( p, a, b, c, d, delta, nsoil )

      implicit none

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to invert (solve) the tri-diagonal matrix problem shown
cc    =======   below:
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer k
      integer kk
      integer nsoil
      integer nsold
      parameter ( nsold = 20 )
      
      real p(nsold)
      real a(nsold)
      real b(nsold)
      real c(nsold)
      real d(nsold)
      real delta(nsold)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     initialize eqn coef c for the lowest soil layer.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      c(nsoil) = 0.0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     solve the coefs for the 1st soil layer
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      p(1) = -c(1) / b(1)
      delta(1) = d(1) / b(1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     solve the coefs for soil layers 2 thru nsoil
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do 10 k = 2 , nsoil
        p(k) = -c(k) * ( 1.0 / (b(k) + a (k) * p(k-1)) )
        delta(k) = (d(k)-a(k)*delta(k-1))*(1.0/(b(k)+a(k)*p(k-1)))
 10   continue

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     set p to delta for lowest soil layer.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      p(nsoil) = delta(nsoil)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     adjust p for soil layers 2 thru nsoil
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do 20 k = 2 , nsoil
        kk = nsoil - k + 1
        p(kk) = p(kk) * p(kk+1) + delta(kk)
 20   continue

      return
      end
      subroutine shflx(s,stc,smc,smcmax,nsoil,t1,dt,yy,zz1,zsoil,tbot,
     +             smcwlt, psisat, sh2o, b,f1,df1,ice,quartz,csoil)

      implicit none

c ------------    frozen ground version    --------------------------
c   new states added: sh2o & parameter smcwlt & psisat
c
c ------------------------------------------------------------------- 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate soil heat flux.  soil temperature content
cc    =======   (an array containing soil temperatures) is also updated.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsold
      parameter ( nsold = 20 )
      integer nsoil
      real b
      real df1
      real csoil
      real dt
      real f1
      real rhsts ( nsold )
      real s
      real smc   ( nsoil )
      real sh2o  ( nsoil )
      real smcmax
      real smcwlt
      real stc   ( nsoil )
      real stcout ( nsold )
      real t1
      real tbot
      real yy
      real zsoil ( nsoil )
      real zz1

      real psisat, quartz

      integer ice, ifrz, i

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     hrt routine calcs the right hand side of the soil temp dif eqn
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if(ice.eq.1) then
       call hrtice(rhsts,stc,smc,smcmax,nsoil,zsoil,yy,zz1,tbot,b,
     *             f1,df1, csoil)
       call hstep ( stc, stc, rhsts,dt,nsoil )
      else
      
       call hrt(rhsts,stc,smc,smcmax,nsoil,zsoil,yy,zz1,tbot,
       
c -------------    frozen ground version    -----------------------
c
     +          smcwlt, psisat, sh2o, dt,
c -----------------------------------------------------------------                  
     +          b,f1,df1,quartz,csoil)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     hstep routine calcs/updates soil temps based on rhsts
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        call hstep ( stcout,stc,rhsts,dt,nsoil )

c ----------------     frozen ground version    --------------------
c   double calling hrt & hstep to reduce noise fluctuation, the same 
c    as soil moisture calculation. double call will be only if there
c    is a ice on at least one soil layer
c
        ifrz = 0
        do i = 1,nsoil
          if( (smc(i) - sh2o(i) ) .gt. 0.0 ) ifrz = 1
        end do
        
        if ( ifrz .eq. 1 ) then     
        do i = 1,nsoil
         stc(i) = 0.5 * ( stcout(i) + stc(i) )
        end do
        
        call hrt(rhsts,stc,smc,smcmax,nsoil,zsoil,yy,zz1,tbot,
       
c -------------    frozen ground version    -----------------------
c
     +          smcwlt, psisat, sh2o, dt,
c -----------------------------------------------------------------                  
     +          b,f1,df1,quartz,csoil)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     hstep routine calcs/updates soil temps based on rhsts
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        call hstep ( stc,stc,rhsts,dt,nsoil )
       else
        do i = 1,nsoil
          stc(i) = stcout(i)
        end do
       endif                  
c ------------------------------------------------------------------
        
       endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc the grnd (skin) temperature
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      t1 = (yy + (zz1 - 1.0) * stc(1)) / zz1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc the sfc soil heat flux
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      s = df1 * (stc(1) - t1) / (0.5 * zsoil(1))

      return
      end
      subroutine smflx ( eta1,smc,nsoil,cmc,etp1,dt,prcp1,zsoil,
     &     sh2o, slope, kdt, frzfact,
     &     smcmax,b,pc,smcwlt,dksat,dwsat,smcref,shdfac,cmcmax,
     &     smcdry,cfactr, runoff1,runoff2, runoff3, edir1, ec1, 
     &     ett1, sfctmp,q2,nroot,rtdis, fxexp)

      implicit none

c ------------    frozen ground version    --------------------------
c   new states added: sh2o, and frozen groud correction factor, frzfact
c   and parameter slope 
c

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate soil moisture flux.  the soil moisture
cc    =======   content (smc - a per unit volume measurement) is a
cc              dependent variable that is updated with prognostic eqns.
cc              the canopy moisture content (cmc) is also updated.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsold
      parameter ( nsold = 20 )
      integer k
      integer nsoil
      real b
      real beta
      real cfactr
      real cmc
      real cmcmax
      real dew
      real dksat
      real drip
      real dt
      real dwsat
      real ec
      real edir
      real et     ( nsold )
      real eta1
      real etp1
      real ett
      real excess
      real fxexp
      real flx1
      real flx2
      real flx3
      real kdt
      real pc
      real pcpdrp
      real prcp1
      real rhsct
      real rhstt  ( nsold )
      real rib
      real rtdis (nsoil)
      real runof
      real runoff,runoxx3
      real shdfac
      real smc    ( nsoil )

c ---------------    frozen ground version     ---------------------
      
      real sh2o   ( nsoil )
      real sice   ( nsold )
      real sh2oa  ( nsold )
      real sh2ofg ( nsold )
c -------------------------------------------------------------------
           
      real smcdry
      real smcmax
      real smcref
      real smcwlt
      real trhsct
      real zsoil  ( nsoil )

      real slope, frzfact, runoff1, runoff2, runoff3, edir1, ec1
      real ett1, sfctmp, q2, dummy, cmc2ms, devap

      integer nroot, i

cmic$ taskcommon rite
!$omp threadprivate(/rite/)
      common/rite/ beta,drip,ec,edir,ett,flx1,flx2,flx3,runof,
     &             dew,rib,runoxx3

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     executable code begins here....if the potential evapotrans-
c     piration is greater than zero...
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dummy=0.
      edir = 0.
      ec = 0.
      ett = 0.
      do 10 k = 1, nsoil
        et ( k ) = 0.
   10 continue

      if ( etp1 .gt. 0.0 ) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       retrieve direct evaporation from soil surface
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c --------------     frozen ground version     ---------------------
c   smc states were replaced by sh2o states
c
        edir = devap ( etp1, sh2o(1), zsoil(1), shdfac, smcmax,
c ------------------------------------------------------------------        
c        edir = devap ( etp1, smc(1), zsoil(1), shdfac, smcmax,

     &                b, dksat, dwsat, smcdry,smcref, smcwlt, fxexp)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       initialize plant total transpiration, retrieve plant
c       transpiration, and accumulate it for all soil layers.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ett = 0.

        if(shdfac.gt.0.0) then
        
c --------------     frozen ground version     ---------------------
c   smc states were replaced by sh2o states
c
          call transp ( et,nsoil,etp1,sh2o,cmc,zsoil,shdfac,smcwlt,
c ------------------------------------------------------------------
c          call transp (et,nsoil,etp1,smc,cmc,zsoil,shdfac,smcwlt,
     &         cmcmax,pc,cfactr,smcref,sfctmp,q2,nroot,rtdis)

          do 20 k = 1 , nsoil
            ett = ett + et ( k )
   20     continue
        endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calculate canopy evaporation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ec = shdfac * ( ( cmc / cmcmax ) ** cfactr ) * etp1

c********  ec should be limited by the total amount of available
c          water on the canopy. modified by f.chen on 10/18/94
c********

        cmc2ms = cmc / dt
        ec = min ( cmc2ms, ec )

      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     total up evap and transp types to obtain actual evapotransp
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        edir1=edir
        ec1=ec
        ett1=ett

      eta1 = edir + ett + ec

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     compute the right hand side of the canopy eqn term ( rhsct )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      rhsct = shdfac * prcp1 - ec

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     convert rhsct (a rate) to trhsct (an amt) and add it to existing
c     cmc. if resulting amt exceeds max capacity, it becomes drip
c     and will fall to the grnd.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      drip = 0.
      trhsct = dt * rhsct
      excess =  cmc + trhsct
      if ( excess .gt. cmcmax ) drip = excess - cmcmax

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     pcpdrp is the combined prcp1 and drip (from cmc) that
c     goes into the soil
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      pcpdrp = (1. - shdfac) * prcp1 + drip / dt

cc      print*,' **************** smlx ******************'
cc      print*,' pcpdrp=', pcpdrp, ' edir=', edir,' et=', et,
cc     *      'smc(1)=', smc(1), 'smc(2)=', smc(2), ' prcp1=', prcp1

c ---------------     frozen ground version     --------------------
c    store ise content at each layer before calling srt & sstep
c
          do i = 1,nsoil
            sice(i) = smc(i) - sh2o(i)
          end do
c ------------------------------------------------------------------
            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     call subroutines srt and sstep to solve the soil moisture
c     tendency equations. iterate twice thru the calls. this eliminates
c     2-delta-t oscillations in the smc values even with timesteps of
c     up to 1 hour. the 1st set of calls yields 1st guess smc values,
c     the 2nd set of calls yields final smc values.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( pcpdrp .gt. 0.0 ) then

c ---------------    frozen ground version       ---------------------
c    smc states replaced by sh2o states in srt subr.
c    sh2o & sice states included in sstep subr.
c    frozen ground correction factor, frzfact, added
c    all water balance calculations using unfrozen water
c
        call srt ( rhstt,runoff,edir,et,sh2o,sh2o,nsoil,pcpdrp,zsoil,
     &             dwsat,dksat,smcmax, b, runoff1, 
     +             runoff2,dt,smcwlt,slope,kdt,frzfact, sice)
             
        call sstep ( sh2ofg,sh2o,dummy,rhstt,rhsct,dt,nsoil,smcmax,
     &               cmcmax, runoff3, zsoil, smc, sice )
        
        do 30 k = 1, nsoil
          sh2oa(k) = ( sh2o(k) + sh2ofg(k) ) * 0.5
   30   continue
        
        call srt ( rhstt,runoff,edir,et,sh2o,sh2oa,nsoil,pcpdrp,zsoil,
     &             dwsat,dksat,smcmax, b, runoff1,
     +             runoff2,dt,smcwlt,slope,kdt,frzfact, sice)

        call sstep ( sh2o,sh2o,cmc,rhstt,rhsct,dt,nsoil,smcmax,
     &               cmcmax, runoff3, zsoil,smc,sice)
     
      else

        call srt ( rhstt,runoff,edir,et,sh2o,sh2o,nsoil,pcpdrp,zsoil,
     &             dwsat,dksat,smcmax, b, runoff1,
     +             runoff2,dt,smcwlt,slope,kdt,frzfact, sice)
     
        
        call sstep ( sh2o,sh2o,cmc,rhstt,rhsct,dt,nsoil,smcmax,
     &               cmcmax, runoff3, zsoil,smc,sice)
     
c -------------------------------------------------------------------- 

c  *****    below is old code    *************************************
c        call srt ( rhstt,runoff,edir,et,smc,smc,nsoil,pcpdrp,zsoil,
c     &       dwsat, dksat, smcmax, b, runoff1, runoff2, dt, smcwlt)        
c        call sstep ( smcfg,smc,dummy,rhstt,rhsct,dt,nsoil,smcmax,
c     &               cmcmax, runoff3, zsoil)
c
c        do 30 k = 1, nsoil
c          smca(k) = ( smc(k) + smcfg(k) ) * 0.5
c   30   continue
c
c        call srt ( rhstt,runoff,edir,et,smc,smca,nsoil,pcpdrp,zsoil,
c     &      dwsat, dksat, smcmax, b, runoff1, runoff2, dt, smcwlt)
c        call sstep ( smc,smc,cmc,rhstt,rhsct,dt,nsoil,smcmax,
c     &               cmcmax, runoff3, zsoil)
c      else
c
c        call srt ( rhstt,runoff,edir,et,smc,smc,nsoil,pcpdrp,zsoil,
c     &        dwsat, dksat, smcmax, b, runoff1, runoff2, dt, smcwlt)
c        call sstep ( smc,smc,cmc,rhstt,rhsct,dt,nsoil,smcmax,
c     &               cmcmax, runoff3, zsoil)
c ********************************************************************

      endif

      runof = runoff
      return
      end
      subroutine hrtice (rhsts,stc,smc,smcmax,nsoil,zsoil,yy,zz1,
     &                 tbot, b, f1, df1, csoil)

      implicit none

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate the right hand side of the time tendency
cc    =======   term of the soil thermal diffusion equation.  also to
cc              compute ( prepare ) the matrix coefficients for the
cc              tri-diagonal matrix of the implicit time scheme.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsold
      parameter ( nsold = 20 )
      integer nsoil
      real ai    ( nsold )
      real bi    ( nsold )
      real cair
      real ch2o
      real ci    ( nsold )
      real csoil
      real ddz
      real ddz2
      real denom
      real dfmax1
      real df1
      real dfmaxn
      real dtsdz
      real dtsdz2
      real b
      real f1
      real hcpct
      integer k
cccc    real mxsmc
      real rhsts ( nsoil )
      real s
      real smc   ( nsoil )
      real smcmax
      real stc   ( nsoil )
      real tbot
      real yy
      real zbot
      real zsoil ( nsoil )
      real zz1
cmic$ taskcommon abci
!$omp threadprivate(/abci/)
      common /abci/ ai, bi, ci
      parameter(cair=1004.0)
      parameter(ch2o=4.2 e6)
cccc  parameter(csoil=1.26e6)

      zbot=zsoil(nsoil)
      dfmax1=2.2
      hcpct=1880.0*917.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc the matrix coefficients ai, bi, and ci for the top layer
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz = 1.0 / ( -0.5 * zsoil(2) )
      ai(1) = 0.0
      ci(1) =  ( dfmax1 * ddz ) / ( zsoil(1) * hcpct )
      bi(1) = -ci(1) + df1/( 0.5 * zsoil(1) * zsoil(1) * hcpct * zz1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc the vertical soil temp gradient btwn the top and 2nd soil
c     layers.  recalc/adjust the soil heat flux.  use the gradient
c     and flux to calc rhsts for the top soil layer.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      dtsdz = ( stc(1) - stc(2) ) / ( -0.5 * zsoil(2) )
      s = df1 * ( stc(1) - yy ) / ( 0.5 * zsoil(1) * zz1 )
      rhsts(1) = ( dfmax1 * dtsdz - s ) / ( zsoil(1) * hcpct )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     initialize ddz2
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz2 = 0.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     loop thru the remaining soil layers, repeating the above process
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do 40 k = 2, nsoil

        if ( k .ne. nsoil ) then
          dfmaxn=2.2

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         calc the vertical soil temp gradient thru this layer.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          denom = 0.5 * ( zsoil(k-1) - zsoil(k+1) )
          dtsdz2 = ( stc(k) - stc(k+1) ) / denom

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         calc the matrix coef, ci, after calc'ng its partial product
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          ddz2 = 2. / (zsoil(k-1) - zsoil(k+1))
          ci(k) = -dfmaxn * ddz2 / ((zsoil(k-1) - zsoil(k)) * hcpct)

        else
          dfmaxn=2.2

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         calc the vertical soil temp gradient thru the lowest layer
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          dtsdz2 = (stc(k)-tbot)/(.5 * (zsoil(k-1) + zsoil(k))-zbot)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         set matrix coef, ci to zero
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          ci(k) = 0.
        end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calc rhsts for this layer after calc'ng a partial product
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        denom = ( zsoil(k) - zsoil(k-1) ) * hcpct
        rhsts(k) = ( dfmaxn * dtsdz2 - dfmax1 * dtsdz ) / denom

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calc matrix coefs, ai, and bi for this layer.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ai(k) = - dfmax1 * ddz / ((zsoil(k-1) - zsoil(k)) * hcpct)
        bi(k) = -(ai(k) + ci(k))

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       reset values of dfmax1, dtsdz, and ddz for loop to next soil lyr
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        dfmax1 = dfmaxn
        dtsdz = dtsdz2
        ddz = ddz2
   40   continue
      return
      end
      subroutine hrt ( rhsts,stc,smc,smcmax,nsoil,zsoil,yy,zz1,tbot,
     +                 smcwlt, psisat, sh2o, dt, b, f1, df1, quartz,
     +                 csoil )

      implicit none

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate the right hand side of the time tendency
cc    =======   term of the soil thermal diffusion equation.  also to
cc              compute ( prepare ) the matrix coefficients for the
cc              tri-diagonal matrix of the implicit time scheme.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsold
      parameter ( nsold = 20 )
      integer nsoil
      integer k
      real ai    ( nsold )
      real bi    ( nsold )
      real cair
      real ch2o
      real ci    ( nsold )
      real csoil
      real ddz
      real ddz2
      real denom
      real dfmax1
      real df1
      real dfmaxn
      real dtsdz
      real dtsdz2
      real f1
      real hcpct
c      real mxsmc
c      real mxice
      real quartz
      real rhsts ( nsoil )
      real s
      real smc   ( nsoil )

      real sh2o  ( nsoil )
      real smcmax
      real smcwlt
      
      real stc   ( nsoil )
      real tbot
      real yy
      real zbot
      real zsoil ( nsoil )
      real zz1

      real cice, t0, tsurf, psisat, dt, b, sice, tbk, tsnsr, tbk1
      real tbnd, snksrc

      integer i

cmic$ taskcommon abci
!$omp threadprivate(/abci/)
      common /abci/ ai, bi, ci
      parameter(cair=1004.0)
      parameter(ch2o=4.2 e6)
c      parameter(csoil=1.26e6)
      parameter(zbot=-3.0)

c   this common statement is need only to print estimated soil 
c   surface temperature, and can be omitted 
      common /prn_tsoil/ tsurf
            
c ----------    frozen ground version    ----------------------------
c    cice is ice heat capacity
c
      parameter ( cice = 2.106 e6, t0 = 273.15 )       
c -------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     retrieve the thermal diffusivity for the wetter of the top soil
c     layer or the next lower soil layer
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c ----------    frozen ground version    ----------------------------
c   soil thermal conductivity adjustment if it is frozen
c
      sice = 0.
      do i = 1, nsoil
        sice = sice + smc(i) - sh2o(i)
      end do

c
c      if( smc(1) .ge. smc(2)) then
c          mxsmc = smc(1)
c          mxice = smc(1) - sh2o(1)
c      else
c          mxsmc = smc(2)
c          mxice = smc(2) - sh2o(2)
c      endif
c turn off below since we use dfmax1=df1
c      call tdfcnd ( dfmax1, smc(1), b, f1,quartz,smcmax,sh2o(1))
      dfmax1=df1
c__________________________________________________________________
c this if-statement is no longer used if peters-lidard thermal
c conductivity is used for all conditions (frozen,unfrozen):
c      if ( mxice .gt. 0.0 ) call frzcnd ( dfmax1, mxice )
c______________________________________________p.grunmann_01/99____

c ------------------------------------------------------------
c      mxsmc = max ( smc(1), smc(2) )
c      call tdfcnd ( dfmax1, mxsmc, b, f1 )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc the heat capacity of the top soil layer
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      hcpct = sh2o(1)*ch2o + (1.0-smcmax)*csoil + (smcmax-smc(1))*cair
c vk correction: smc to sh2o, first term on r.h. side.-p.grunmann 1/99

c ----------    frozen ground version    ----------------------------
c   ice heat capacuty is included
c      
     +        + ( smc(1) - sh2o(1) )*cice
c -------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc the matrix coefficients ai, bi, and ci for the top layer
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz = 1.0 / ( -0.5 * zsoil(2) )
      ai(1) = 0.0
      ci(1) =  ( dfmax1 * ddz ) / ( zsoil(1) * hcpct )
      bi(1) = -ci(1) + df1 / ( 0.5 * zsoil(1) * zsoil(1)*hcpct*zz1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc the vertical soil temp gradient btwn the top and 2nd soil
c     layers.  recalc/adjust the soil heat flux.  use the gradient
c     and flux to calc rhsts for the top soil layer.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c ------------    frozen ground version    --------------------------
c   soil surface temperature adjustment by zz1 ( the same as t1 )
c
      tsurf = ( yy + ( zz1 - 1 ) * stc(1) ) / zz1
c -------------------------------------------------------------------
      
      dtsdz = ( stc(1) - stc(2) ) / ( -0.5 * zsoil(2) )
      s = df1 * ( stc(1) - yy ) / ( 0.5 * zsoil(1) * zz1 )
      rhsts(1) = ( dfmax1 * dtsdz - s ) / ( zsoil(1) * hcpct )

c -----------    frozen ground version     --------------------------
c   calculating of layer boundary temperature using tbnd subr.
c   and sink/source term of diffusion equation by snksrc subr.
c
      if ( sice .ne. 0. .or. tsurf .lt. t0 ) then 
       tbk = tbnd ( stc(1), stc(2), zsoil, zbot, 1, nsoil )
       tsnsr = snksrc ( tsurf, stc(1),tbk, smc(1), sh2o(1), hcpct, 
     +             zsoil,nsoil,smcmax, psisat, b, f1, dt, 1, quartz)
       rhsts(1) = rhsts(1) - tsnsr / ( zsoil(1) * hcpct )
      endif 
c -------------------------------------------------------------------
            
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     initialize ddz2
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz2 = 0.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     loop thru the remaining soil layers, repeating the above process
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k = 2, nsoil

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calc this soil layer's heat capacity
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        hcpct = sh2o(k)*ch2o +(1.0-smcmax)*csoil +(smcmax-smc(k))*cair
c vk correction: smc to sh2o, first term on r.h. side.-p.grunmann 1/99

c ----------    frozen ground version    ----------------------------
c   ice heat capacuty is included
c      
     +        + ( smc(k) - sh2o(k) )*cice
c -------------------------------------------------------------------   

        if ( k .ne. nsoil ) then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         retrieve the thermal diffusivity for the wetter of the
c         current or the next lower soil layer
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c          mxsmc = max ( smc(k), smc(k+1) )

c ------------    frozen ground version    --------------------------
c   soil thermal diffusivity adjustment if soil is frozen
c
c      if ( smc(k) .ge. smc(k+1) ) then
c          mxsmc = smc(k)
c          mxice = smc(k) - sh2o(k)
c      else
c          mxsmc = smc(k+1)
c          mxice = smc(k+1) - sh2o(k+1)
c      endif
cc        call tdfcnd ( dfmaxn, mxsmc, b, f1,quartz,smcmax,sh2o(k))
      call tdfcnd ( dfmaxn, smc(k), b, f1,quartz,smcmax,sh2o(k))
cc__________________________________________________________________
c this if-statement is no longer used if peters-lidard thermal
c conductivity is used for all conditions (frozen,unfrozen):
c      if ( mxice .gt. 0.0 ) call frzcnd ( dfmaxn, mxice )
c -------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         calc the vertical soil temp gradient thru this layer.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          denom = 0.5 * ( zsoil(k-1) - zsoil(k+1) )
          dtsdz2 = ( stc(k) - stc(k+1) ) / denom

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         calc the matrix coef, ci, after calc'ng its partial product
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          ddz2 = 2. / (zsoil(k-1) - zsoil(k+1))
          ci(k) = -dfmaxn * ddz2 / ((zsoil(k-1) - zsoil(k)) * hcpct)

c -----------    frozen ground version     --------------------------
c   calculating of layer boundary temperature using tbnd subr.
c
          tbk1 = tbnd ( stc(k), stc(k+1), zsoil, zbot, k, nsoil )
c -------------------------------------------------------------------
          
        else

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         retrieve the thermal diffusivity for the lowest soil layer
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c -----------    frozen ground version     --------------------------
c   soil thermal diffusivity adjustment
c   and  calculating of layer boundary temperature using tbnd subr.
c
c          mxice = smc(k) - sh2o(k)
          call tdfcnd ( dfmaxn, smc(k), b, f1,quartz,smcmax,sh2o(k))
c__________________________________________________________________
c this if-statement is no longer used if peters-lidard thermal
c conductivity is used for all conditions (frozen,unfrozen):
c          if ( mxice .gt. 0.0 ) call frzcnd ( dfmaxn, mxice )
          
          tbk1 = tbnd ( stc(k), tbot, zsoil, zbot, k, nsoil )
c -------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         calc the vertical soil temp gradient thru the lowest layer
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          dtsdz2 = (stc(k)-tbot) / (.5 * (zsoil(k-1) + zsoil(k))-zbot)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         set matrix coef, ci to zero
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          ci(k) = 0.
        end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calc rhsts for this layer after calc'ng a partial product
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        denom = ( zsoil(k) - zsoil(k-1) ) * hcpct
        rhsts(k) = ( dfmaxn * dtsdz2 - dfmax1 * dtsdz ) / denom

c -----------    frozen ground version     --------------------------
c   sink/source term of diffusion equation by snksrc subr.
c
      if ( sice .ne. 0. .or. tbk .lt. 0. ) then
       tsnsr = snksrc ( tbk, stc(k),tbk1, smc(k), sh2o(k), hcpct, 
     +             zsoil,nsoil,smcmax, psisat, b, f1, dt, k, quartz)
       rhsts(k) = rhsts(k) - tsnsr / denom
      endif 
c -------------------------------------------------------------------
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       calc matrix coefs, ai, and bi for this layer.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ai(k) = - dfmax1 * ddz / ((zsoil(k-1) - zsoil(k)) * hcpct)
        bi(k) = -(ai(k) + ci(k))

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c reset values of dfmax1, dtsdz, and ddz for loop to next soil lyr
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c -----------    frozen ground version     --------------------------
c    remember upper layer temperature
c
        tbk = tbk1
c -------------------------------------------------------------------
        
        dfmax1 = dfmaxn
        dtsdz = dtsdz2
        ddz = ddz2
c   40   continue
      end do
      return
      end
      function tbnd (tu, tb, zsoil, zbot, k, nsoil)            

      implicit none

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc   purpose:   calculate temperature on the boundary of the layer
cc   =======    by interpolation of the middle layer temperatures
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real tbnd, t0, tu, tb, zbot, zup, zb

      integer nsoil, k
      
      parameter (t0=273.15)
      real zsoil (nsoil)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc   use surface temperature on the top of the first layer
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      if(k .eq. 1) then
        zup=0.
      else
        zup=zsoil(k-1)
      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc   use depth of the constant bottom temperature when interpolate
cc   temperature into the last layer boundary
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      if(k .eq. nsoil) then
        zb=2.*zbot-zsoil(k)
      else
        zb=zsoil(k+1)
      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc   linear interpolation between the average layer temperatures
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      tbnd=tu+(tb-tu)*(zup-zsoil(k))/(zup-zb)
      
      return
      end  
      function snksrc ( tup,tm,tdn, smc, sh2o, hcpct, zsoil,nsoil,
     +                    smcmax, psisat, b, f1, dt, k, quartz) 
     
      implicit none
     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate sink/source term of the termal diffusion
cc    =======   equation. (sh2o) is available liqued water.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer  k
      integer  nsoil
      
      real b
      real df
      real dfh2o
      real dfice
      real dh2o
      real dt
      real dz
      real dzh
      real f1
      real free
      real frh2o
      real hlice
      real psisat
      real qdn
      real qtot
      real quartz
      real qup
      real sh2o
      real sice
      real smc
      real smcmax
      real snksrc
      real t0
      real tavg
      real tdn
      real tm
      real tup
      real tz
      real x0
      real xdn
      real xh2o
      real xup
      real hcpct
      real zsoil (nsoil)
      
      parameter (hlice=3.3350 e5)
      parameter (dh2o =1.0000 e3)
      parameter (  t0 =2.7316 e2)
      
c  **************************************************************************
c  ***  option to run non-frozen version:  uncomment next two statements ****
c  *******************        warning !!!!!        **************************  
c  ***  to run frozen ground version, next two statements should be      ****
c  ***  commented                                                        **** 
c
      snksrc = 0.0
      goto 77
c  ************************************************************************** 

      sice=smc-sh2o      
      if(k.eq.1) then
        dz=-zsoil(1)
      else
        dz=zsoil(k-1)-zsoil(k)
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  calculate diffusivities of thawed and frozen ground
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  warning !!!!!, by pablo grunmann, 10/98
c attention to the calling argument:
c 
c    1. use smc instead of sh2o to use vkoren's frozen df (call 
c        and if statement commented out with c vk)
c    2. use sh2o in calling argument list for petrs-lidard (call 
c        commented out with c p-l)
c                 !!!                      !!!!
c p-l      call tdfcnd ( dfh2o, smc, b, f1,quartz,smcmax,sh2o) 
c vk       call tdfcnd ( dfh2o, smc, b, f1,quartz,smcmax, smc) 
      call tdfcnd ( dfh2o, smc, b, f1,quartz,smcmax,sh2o)
      dfice = dfh2o
c frozen conductivity for original vk approach
c vk      if ( sice .gt. 0.0 ) call frzcnd ( dfice, sice )

cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  heat flux from the top boundary of layer
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      if(tup .le. t0) then
        df=dfice
      else
        df=dfh2o
      endif
      qup=-df*(t0-tup)/(0.5*dz)
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  heat flux from the bottom bondary of layer
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if(tdn .le. t0) then
        df=dfice
      else
        df=dfh2o
      endif
      qdn=-df*(t0-tdn)/(0.5*dz)
      qtot=qup+qdn

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calculate potential reduction of liqued water content
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      xh2o=qtot*dt/(dh2o*hlice*dz) + sh2o
            
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     estimate unfrozen water at temperature tavg,
c     and check if calculated water content is reasonable 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
        
c  ****   new calculation of average temperature (tavg)   **********
c  ****   in freezing/thawing layer using up, down, and middle   ***
c  ****   layer temperatures (tup, tdn, tm)               **********
   
      dzh=dz*0.5
      if(tup .lt. 0.) then

        if(tm .lt. 0.) then

          if(tdn .lt. 0.) then
c1  ******   tup, tm, tdn < 0.  case  ************          
            tavg=(tup+2.0*tm+tdn)/4.
             
c       c111  ****   end tdn_111  then

          else
c2  ******   tup & tm < 0.,  tdn > 0. case   *****
            x0=(t0-tm)*dzh/(tdn-tm)
            tavg=0.5*(tup*dzh+tm*(dzh+x0)+t0*(2.*dzh-x0))/dz
                       
c       c112   ****  end tdn_111  else   ****
          endif      

c   c11  ***********  end tm_1 then   ***********
        else
        
          if(tdn .lt. 0.) then
c3  *******  tup < 0.  tm > 0.  tdn < 0. case   ****
            xup=(t0-tup)*dzh/(tm-tup)
            xdn=dzh-(t0-tm)*dzh/(tdn-tm)
            tavg=0.5*(tup*xup+t0*(2.*dz-xup-xdn)+tdn*xdn)/dz
                      
c       c121   ****   end tdn_121  then  *****

          else
c4   ******  tup < 0  tm > 0  tdn > 0  case   ******
            xup=(t0-tup)*dzh/(tm-tup)
            tavg=0.5*(tup*xup+t0*(2.*dz-xup))/dz
                      

c       c122   ***   end tdn_121  else   ***
          endif   
        
c   c12  ***********  end tm_1 else   ********** 
        endif

c1    ********************  end tup then   ***********    
      else

        if(tm .lt. 0.) then

          if(tdn .lt. 0.) then
c5   *****  tup > 0  tm < 0 tdn < 0 case   *********
            xup=dzh-(t0-tup)*dzh/(tm-tup)
            tavg=0.5*(t0*(dz-xup)+tm*(dzh+xup)+tdn*dzh)/dz
                      
c       c211   ****   end tdn_211  then  *****

          else
c6   *****  tup > 0  tm < 0  tdn > 0 case  *********
            xup=dzh-(t0-tup)*dzh/(tm-tup)
            xdn=(t0-tm)*dzh/(tdn-tm)
            tavg=0.5*(t0*(2.*dz-xup+xdn)+tm*(xup+xdn))/dz
                                   

c       c212   ***   end tdn_211  else   ***
          endif   

c   c21   ************   end tm_2  then   ************
        else

          if(tdn .lt. 0.) then
c7   *****  tup > 0  tm > 0  tdn < 0 case   ********
            xdn=dzh-(t0-tm)*dzh/(tdn-tm)
            tavg=(t0*(dz-xdn)+0.5*(t0+tdn)*xdn)/dz
                  
c       c221   ****   end tdn_221  then  *****

          else
c8   *****  tup > 0  tm > 0  tdn > 0 case   ********
            tavg=(tup+2.*tm+tdn)/4.
                      
c       c222   ***   end tdn_221  else   ***
          endif           

c   c22   ************   end tm_2  else   ************
        endif

c   *********************  end tup else  **************
      endif                      
c 777   
      
100   free=frh2o(tavg, smc, sh2o, smcmax, b, psisat )

      if ( xh2o .lt. sh2o .and. xh2o .lt. free) then 
         if ( free .gt. sh2o ) then
              xh2o = sh2o
          else
              xh2o = free
          endif
      endif
              
      if ( xh2o .gt. sh2o .and. xh2o .gt. free )  then
         if ( free .lt. sh2o ) then
              xh2o = sh2o
          else
              xh2o = free
          endif
      endif 
             
      if(xh2o .lt. 0. ) xh2o=0.
10    if(xh2o .gt. smc) xh2o=smc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calculate sink/source term and replace previous water content 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     
      snksrc=-dh2o*hlice*dz*(xh2o-sh2o)/dt

        if(xh2o .lt. 0.) then
          xh2o=sh2o
        endif

      sh2o=xh2o
      
77    return
      end
      function frh2o(t,smc,sh2o,smcmax,b,psis)
 
      implicit none
     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
cc  purpose:  calculate free water content if temperature 
cc  =======   is below 273.15k (t0). 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real frh2o, hlice, gs, ck, dice, dh2o, t0, error, t
      real smc, sh2o, smcmax, b, psis, bx, tr, swl
      real smcm, smcc, df, denom, swlk, dswl

      integer n

      parameter (hlice=3.335 e5)      
      parameter (gs = 9.81)
      parameter (ck=8.)
      parameter (dice=920.0)
      parameter (dh2o=1000.0)
      parameter (t0=273.15)
      parameter (error=0.005)
       
c  ***             limits on parameter b: b < 5.5              ****
c  ***   simulations showed if b > 5.5 unfrozen water content  ****
c  ***   is non-realistically high at very low temperatures    ****
c******************************************************************
c
       bx = b
       if ( b .gt. 5.5 ) bx = 5.5
c------------------------------------------------------------------
        
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc  assumed that free water content equals its value at temperature 
cc  273.11k in range of temperatures 273.11k - 273.15k to reduce
cc  iterratins problem in this range
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      

      tr=t
      if(t .gt. 273.11 .and. t .le. t0 ) tr=273.11

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  if temperature above 273.15k sh2o = smc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
      if(tr .gt. t0) then
        frh2o=smc
        goto 77
      endif  

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  convert soil moisture variables into %
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      swl=(smc-sh2o)*100.
      smcm=smcmax*100.
      smcc=smc*100.                       

      n=0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  start of iterrations
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

   10  df = ( -psis*gs/hlice ) * ( ( 1.+ck*swl*0.01 )**2. ) *
     +      ( smcm/(smcc-swl ) )**bx
        
       denom = 2. * ck / ( 1.+ck*swl*0.01 ) + bx / ( smcc - swl )
       swlk = swl - (1.-(tr-t0)/(tr*df))/denom
 
       dswl=abs(swlk-swl)
       swl=swlk
       if(swl.ge.smcc) then
c        write(*,*) 'warn2 ',n,swl,smcc
       swl=smcc*0.8
        smcc=smcc*0.97
       endif
       
       n=n+1 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc  exit if more than 900 iterrations. free water content
cc  will not be changed
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        
      if(n .gt. 900 ) then
        if(tr . lt. 271.16) then
          swl = ( smc-0.02 ) * 100.
        else
          swl=0.
        endif    
        goto 20
      endif
        
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc  end of iterrations if last estimate difference is less than 
cc  an error
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         
      if ( dswl .gt. error ) goto 10
      
   20 if(swl .lt. 0.)  then
c        write(*,*) ' swl < 0. ',swl
        swl=0.
      endif  

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc  recalculate ice content in % into volumetric free water
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c this eq was using max1(), which yields an integer result
c changed to max() - pablo grunmann 12-07-98. - !!!!!!!!!!!!
      frh2o = max ( (smc - swl*0.01 ) ,  0.02 )

c  ***        flerchinger equation             *******      
cc      fk=(((-hlice/(gs*psis))*((tr-t0)/tr))**(-1/b))*smcmax
cc      frh2o = min ( fk, smc )
cc      if(frh2o .lt. 0.) frh2o = 0.02
c-----------------------------------------------------

   77 continue
      return
      end      
      subroutine hstep ( stcout, stcin, rhsts, dt, nsoil )

      implicit none

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate/update the soil temperature field.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsold
      parameter ( nsold = 20 )
      integer k
      integer nsoil

      real ai    ( nsold )
      real bi    ( nsold )
      real ci    ( nsold )
      real dt
      real rhsts ( nsoil )

c -----------    frozen ground version     -------------------------
c      
      real stcout   ( nsoil )
      real stcin  ( nsoil )
c ------------------------------------------------------------------      
      
cmic$ taskcommon abci
!$omp threadprivate(/abci/)
      common /abci/ ai, bi, ci

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     create finite difference values for use in rosr12 routine
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do 10 k = 1 , nsoil
        rhsts(k) = rhsts(k) * dt
        ai(k) = ai(k) * dt
        bi(k) = 1. + bi(k) * dt
        ci(k) = ci(k) * dt
 10   continue

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     solve the tri-diagonal matrix equation
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      call rosr12 ( ci,ai,bi,ci,rhsts,rhsts,nsoil )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc/update the soil temps using matrix solution
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do 20 k = 1 , nsoil
      
c -----------    frozen ground modification    ----------------------
c   new temperatures in stcout from old stcin
c       
        stcout(k) = stcin(k) + ci(k)
c -------------------------------------------------------------------
c        stc(k) = stc(k) + ci(k)
        
  20  continue
      return
      end
      subroutine snopac ( etp,eta,prcp,prcp1,snowng,smc,smcmax,smcwlt,
     &     smcref, smcdry, cmc, cmcmax, nsoil, dt, sbeta, q1,
     &     q2,t1,sfctmp,t24,th2,f,f1,s,stc,epsca,sfcprs,
     &     b, pc, rch, rr, cfactr, salp, esd,
     +     snowh, sh2o, slope, kdt, frzfact, psisat,snup,
     &     zsoil, dwsat, dksat, tbot, shdfac, runoff1,
     &     runoff2,runoff3,edir1,ec1,ett1,nroot,snmax,ice,
     &     rtdis,quartz, fxexp,csoil)

      implicit none

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    purpose:  to calculate soil moisture and heat flux values and update
cc    =======   soil moisture content and soil heat content values for
cc              the case when a snow pack is present.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsoil

      logical snowng

      real b
      real beta
      real cfactr
      real cmc
      real cmcmax
      real cp
      real cph2o
      real cpice
      real csoil
      real denom
      real dew
      real df1
      real dksat
      real drip
      real dt
      real dwsat
      real ec
      real edir
      real epsca
      real esd
      real snowh
      real eta
      real eta1
      real etp
      real etp1
      real etp2
      real ett
      real ex
      real f
      real fxexp
      real flx1
      real flx2
      real flx3
      real f1
      real kdt
      real lsubf
      real lsubc
      real lsubs
      real pc
      real prcp
      real prcp1
      real q1
      real q2
      real rch
      real rib
      real rr
      real rtdis (nsoil)
      real runoff
      real s
      real sbeta
      real s1
      real sfctmp
      real shdfac
      real sigma
      real smc     ( nsoil )
      real sh2o    ( nsoil )
      real smcdry
      real smcmax
      real smcref
      real smcwlt
      real snmax
      real stc     ( nsoil )
      real t1
      real t11
      real t12
      real t12a
      real t12b
      real t24
      real tbot
      real th2
      real yy
      real zsoil( nsoil )
      real zz1
c
c new snow heat flux calc      
      real dsoil
      real dtot
      real expsno
      real expsoi
      real ssnow
c
      real tfreez, salp, sfcprs, slope, frzfact, psisat, snup
      real runoff1, runoff2, runoff3,runoxx3
      real edir1, ec1, ett1, quartz
      real sndens, sncond, rsnow, sncover, qsat, etp3, seh, t14
      real sice1, csnow

      integer nroot, ice


cmic$ taskcommon rite
!$omp threadprivate(/rite/)

      common/rite/ beta,drip,ec,edir,ett,flx1,flx2,flx3,runoff,
     &             dew,rib,runoxx3
      parameter(cp=1004.5,cph2o=4.218e+3,cpice=2.106e+3,
     &     lsubf=3.335e+5,lsubc=2.5e+6,lsubs=2.83e+6,sigma=5.67e-8)
     
c ------------    frozen ground version     ------------------------
c   
c  
      parameter ( tfreez = 273.15)
c-------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     executable code begins here...
c     convert etp fm  kg m-2 s-1  to  m s-1  and then to an amount
c     (m) and call it an effective snowpack reduction amt, etp2 (m).
c     this is the amt the snowpack would be reduced due to evaporation
c     from the snow sfc during the timestep.  evaporation will
c     proceed at the potntl rate unless the snow depth is less than
c     the expected snowpack reduction.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      prcp1=prcp1*0.001
      etp2 = etp * 0.001 * dt
      beta = 1.0
      if(ice.ne.1) then
        if ( esd .lt. etp2 ) then
          beta = esd / etp2
        endif
      endif
      dew = 0.0
      if ( etp .lt. 0.0 ) then
        dew = -etp * 0.001
      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc an 'effective snow-grnd sfc temp' based on heat fluxes
c     btwn the snow pack and the soil and on net radiation.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      t12a = ((f - sigma * t24) / rch+th2-sfctmp-beta*epsca) / rr
      
c ---------------    frozen ground version    -----------------------
c  variable snow density & conductivity

      sndens = esd / snowh
      sncond = csnow ( sndens )
c new soil heat flux under snow calculation, 10 may 1999, m. ek
c 1st soil thermal conductivity
c pps added subscript to sh2o
      call tdfcnd ( df1, smc(1), b, f1,quartz,smcmax,sh2o(1) )
c next veg effect in thermal cond. (reduction), 10 may 1999, m. ek
      df1 = df1 * exp(sbeta*shdfac)
c finally snow effect in thermal cond., 10 may 1999, m. ek  
      dsoil = -(0.5 * zsoil(1))
      dtot = snowh + dsoil
      expsno = snowh/dtot
      expsoi = dsoil/dtot
c 1. harmonic mean (series flow)
c        df1 = (sncond*df1)/(expsoi*sncond+expsno*df1)
c 2. arithmetic mean (parallel flow)
c        df1 = expsno*sncond + expsoi*df1
c 3. geometric mean
        df1 = (sncond**expsno)*(df1**expsoi)
c
c      t12b = sncond * stc(1) / ( snowh * rr * rch )
      t12b = df1 * stc(1) / ( dtot * rr * rch )
c      denom = 1.0 + sncond / ( snowh * rr * rch )
      denom = 1.0 + df1 / ( dtot * rr * rch )
c -------------------------------------------------------------------      
           
c      t12b = 0.35 * stc(1) / ( 10.0 * esd * rr * rch )
c      denom = 1.0 + 0.35 / ( 10.0 * esd * rr * rch )

      t12 = (sfctmp + t12a + t12b ) / denom

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     recalculate the heat flux into or out of the soil, 
c     and set the effective potential evapotransp to zero.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cc.....s = 0.35 * ( t12 - stc(1) ) / ( 10.0 * esd )
      
c ---------------    frozen ground version    -----------------------
c  variable snow density & conductivity
c
c new soil heat flux under snow calculation, 10 may 1999, m. ek
c 1st soil thermal conductivity
c pps added subscript to sh2o
      call tdfcnd ( df1, smc(1), b, f1,quartz,smcmax,sh2o(1) )
c next veg effect in thermal cond. (reduction), 10 may 1999, m. ek
      df1 = df1 * exp(sbeta*shdfac)
c finally snow effect in thermal cond., 10 may 1999, m. ek  
      dsoil = -(0.5 * zsoil(1))
      dtot = snowh + dsoil
      expsno = snowh/dtot
      expsoi = dsoil/dtot
c 1. harmonic mean (series flow)
c        df1 = (sncond*df1)/(expsoi*sncond+expsno*df1)
c 2. arithmetic mean (parallel flow)
c        df1 = expsno*sncond + expsoi*df1
c 3. geometric mean
        df1 = (sncond**expsno)*(df1**expsoi)
c
c      s = sncond * ( t12 - stc(1) ) / ( snowh )
      s = df1 * ( t12 - stc(1) ) / ( dtot )
      ssnow = sncond * (t1 - stc(1) ) /snowh

c -------------------------------------------------------------------
      
c turn off bounds on soil heat flux under snow
c     s = min ( max ( s,-100.0 ), 100.0 )
      if ( t12 .le. tfreez ) then

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       if the 'effective snow-grnd sfc temp' is at or blo freezing,
c       no snow melt will occur.  set the skin temp to this
c       effective temp and set the effective precip to zero.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        esd = max ( 0.0 , esd - etp2 )
        t1 = t12
        flx3 = 0.0
        ex = 0.0
        snmax = 0.0

      else

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       if the 'effective snow-grnd sfc temp' is above freezing, snow
c       melt will occur. call the snow melt rate,ex and amt, snmax.
c       revise the effective snow depth.  revise the skin temp because
c       it would have chgd due to the latent heat released by the
c       melting. calc the latent heat released, flx3. set the effective
c       precip, prcp1 to the snow melt rate, ex for use in smflx.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c this is the old version of snow melt
c        ex = 0.001 * rch * rr * ( t12 - 273.15 ) / lsubf
c        snmax = ex * dt
c        ex = min ( esd / dt, ex )
c        esd = max ( esd - snmax, 0.0 )
c        t1 = max ( 273.15, (t12-(lsubf * ex * 1000.0 / (rch*rr))))
c        flx3 = lsubf * ex * 1000.0
c        prcp1 = prcp1 + ex
c below is the correction of evaporation considering snow melting
c                    f. chen (96/3/18)

c        t1 = 273.15

c ------------    frozen ground version    --------------------------
c   adjustment t1 to account for snow patches
c
        if ( snup .gt. 0.0 .and. esd .lt. snup ) then
          rsnow = esd / snup
          sncover = 1.-( exp(-salp*rsnow)-rsnow*exp(-salp))
c          sncover = min ( esd / snup, 1.)
        else
          sncover = 1.
        endif
            
        t1 = tfreez * sncover + t12 * ( 1.0 - sncover )
c -------------------------------------------------------------------
        
        qsat = (0.622*6.11e2)/(sfcprs-0.378*6.11e2)
        etp = rch*(qsat-q2)/cp
        etp2 = etp*0.001*dt
        beta = 1.0
        if ( esd .le. etp2 ) then
          beta = esd / etp2
          esd = 0.0
          snmax = 0.0
          ex = 0.0
        else
          esd = max ( 0.0 , esd - etp2 )
          etp3 = etp*lsubc
          
c ------------    frozen ground version    --------------------------
c

c          s = sncond * ( t1 - stc(1) ) / ( snowh )
          s = df1 * ( t1 - stc(1) ) / ( dtot )
          ssnow = sncond * (t1 - stc(1) ) /snowh

c -------------------------------------------------------------------

c          s = 0.35 * ( t1 - stc(1) ) / ( 10.0 * esd )

c turn off bounds on soil heat flux under snow
c         s = min ( max ( s,-100.0 ), 100.0 )
          seh = rch*(t1-th2)
          t14 = t1*t1
          t14 = t14*t14
          flx3 = f - sigma*t14 - s - seh - etp3
          if(flx3.le.0.0) flx3=0.0
          ex = flx3*0.001/lsubf
          
c ------------    frozen ground version    --------------------------
c   snowmelt reduction depending on snow cover
c   if snow cover less than 5% no snowmelt reduction
c
          if ( sncover .gt. 0.05) ex = ex * sncover
c ------------------------------------------------------------------- 
         
          snmax = ex * dt
        endif
        
        if(snmax.lt.esd) then
          esd = esd - snmax
        else
          ex = esd/dt
          snmax = esd
          esd = 0.0
          flx3 = ex*1000.0*lsubf
c check this out (below) ...see ken's 95 memo, m. ek, 10 may 1999
c         call tdfcnd ( df1, smc(1), b, f1 )
c         yynum = f - sigma * t24
c         yy = sfctmp + (yynum/rch+th2-sfctmp-(flx3+etp3)) / rr
c          zz1 = df1 / ( -0.5 * zsoil(1) * rch * rr ) + 1.0
c          t1 = (yy + (zz1 - 1.0) * stc(1)) / zz1
c         s = df1 * ( stc(1) - t1 ) / ( 0.5 * zsoil(1) )
        endif
        prcp1 = prcp1 + ex

      endif
         
c set the effective potential evapotransp to zero
      etp1 = 0.0
c what is this^^^ ??? 5 may 1999
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     smflx returns soil moisture values and preliminary values of
c     evapotranspiration.  in this, the snow pack case, the prelim-
c     values (eta1) are not used in subsequent calculation of e.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if (ice .ne. 1) then
         
c ------------    frozen ground version    --------------------------
c (+) new states added: sh2o,  and frozen ground correction factor
c
         call smflx ( eta1,smc,nsoil,cmc,etp1,dt,prcp1,zsoil,
     +             sh2o, slope, kdt, frzfact,
     &             smcmax,b,pc,smcwlt,dksat,dwsat,
     &             smcref,shdfac,cmcmax,smcdry,cfactr,runoff1,runoff2,
     &             runoff3, edir1, ec1, ett1,sfctmp,q2,nroot,rtdis,
     &             fxexp)

      endif

      eta = beta * etp

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     calc/update the grnd sfc (skin) mixing ratio.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      q1 = q2 + eta * cp / rch

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     retrieve the soil temp diffusivity/conductivity.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c new soil heat flux under snow calculation, 10 may 1999, m. ek
c 1st soil thermal conductivity
c pps added subscript to sh2o
      call tdfcnd ( df1, smc(1), b, f1,quartz,smcmax,sh2o(1) )
c next veg effect in thermal cond. (reduction), 10 may 1999, m. ek
      df1 = df1 * exp(sbeta*shdfac)
c finally snow effect in thermal cond., 10 may 1999, m. ek  
      dsoil = -(0.5 * zsoil(1))
      dtot = snowh + dsoil
      expsno = snowh/dtot
      expsoi = dsoil/dtot
c 1. harmonic mean (series flow)
c        df1 = (sncond*df1)/(expsoi*sncond+expsno*df1)
c 2. arithmetic mean (parallel flow)
c        df1 = expsno*sncond + expsoi*df1
c 3. geometric mean
        df1 = (sncond**expsno)*(df1**expsoi)

c ------------    frozen ground version    --------------------------
c   soil conductivity adjustment
c      
      sice1 = smc(1) - sh2o(1)
c__________________________________________________________________
c this if-statement is no longer used if peters-lidard thermal
c conductivity is used for all conditions (frozen,unfrozen):
c      if ( sice1 .gt. 0.0 ) call frzcnd ( df1, sice1 ) 
c -------------------------------------------------------------------
           
      if(ice.eq.1) df1=2.2

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     the 'adjusted top soil lyr temp' ( yy ) and the 'adjusted soil
c     heat flux' ( zz1 ) are set to the top soil lyr temp, and 1,
c     respectively.  these are close-enough approximations because
c     the sfc heat flux to be computed in shflx will effectively be
c     the flux at the snow top surface.  t11 is a dummy arguement
c     since we will not use its value as revised by shflx.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      zz1 = 1.0
      yy = stc(1)-0.5*s*zsoil(1)*zz1/df1
      t11 = t1

c ------------    frozen ground version    --------------------------
c   snow depth & density adjustment based on snow compaction
c      
      call snowpack ( esd, dt, snowh, sndens, t1, yy )
c -------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     shflx will calc/update the soil temps. note the soil heat flux
c     ( s1 ) and the skin temp ( t11 ) output from this call are not
c     used in any subsequent calculations.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c ------------    frozen ground version    --------------------------
c (+) new states added: sh2o & parameter smcwlt & psisat
c

      call shflx ( s1,stc,smc,smcmax,nsoil,t11,dt,yy,zz1,zsoil,tbot,
     +             smcwlt, psisat, sh2o,
     &             b,f1,df1,ice, 
     &             quartz,csoil)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     if precip is falling, calc heat flux from snow sfc to newly
c     accumulating precip.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      flx1 = 0.0
      if ( snowng ) then
        flx1 = cpice * prcp * ( t1 - sfctmp )
      else
        if ( prcp .gt. 0.0 ) flx1 = cph2o * prcp * ( t1 - sfctmp )
      endif
      return
      end
      subroutine snowpack ( w,dts,hc,ds,tsnow,tsoil )

      implicit none

c **************************************************************
c **        subroutine to calculate snow compaction          ***
c **  equation of increasing of snow density was obtained    ***
c     as an approximate solution of e. anderson differential ***
c     equation (3.29), noaa technical report nws 19,         ***
c                 by   victor koren   03/25/95               ***
c **************************************************************

c **************************************************************
c  w      is a water equivalent of snow, in m                ***
c  dts    is a time step, in sec                             ***
c  hc     is a snow depth, in m                              ***
c  ds     is a snow density, in g/cm3                        ***
c  tsnow  is a snow surface temperature, k                   ***
c  tsoil  is a soil surface temperature, k                   ***
c      subroutine will return new values of h and ds         ***
c **************************************************************

      real c1, c2, hc, w, dts, ds, tsnow, tsoil, h, wx
      real dt, tsnowx, tsoilx, tavg, b, dsx, dw

      integer ipol, j
      parameter (c1=0.01, c2=21.0)
      real pexp

c **  conversion into simulation units   ************************* 

      h=hc*100.
      wx=w*100.
      dt=dts/3600.
      tsnowx=tsnow-273.15
      tsoilx=tsoil-273.15

c **  calculating of average temperature of snow pack              ***

      tavg=0.5*(tsnowx+tsoilx)                                    

c **  calculating of snow depth and density as a result of compaction
c              ds=ds0*(exp(b*w)-1.)/(b*w)
c              b=dt*c1*exp(0.08*tavg-c2*ds0)
c **  c1 is the fractional increase in density (1/(cm*hr)) 
c **  c2 is a constant (cm3/g) kojima estimated as 21 cms/g

      if(wx .gt. 1.e-2) then
         b=dt*c1*exp(0.08*tavg-c2*ds)
c
c  9/03/98   this line was changed to increase an accuracy of exponent
c        dsx=ds*((dexp(b*wx)-1.)/(b*wx))
c--------------------------------------------------------------------
c  the above line was causing numerical difficulties because of the
c  denominator, that can go to zero (the function
c  has a well defined limit, equal to 1. when b*wx 
c  goes to zero, though)
c
c  suggestion to substitute the line above by 
c                                       pablo grunmann 03-09-98
c--------------------------------------------------------------------
c remember to declare pexp

c precision 
c   desired:  ipol greater than 9 only makes a difference on double
c             precision.
c        ipol=9, for rel.error <~ 1.6 e-6 % (8 significant digits)
c        ipol=8, for rel.error <~ 1.8 e-5 % (7 significant digits)
c        ipol=7, for rel.error <~ 1.8 e-4 % ...

        ipol = 9
        pexp = 0.
        do j = ipol,1,-1
                 pexp = (1. + pexp)*b*wx/real(j+1) 
        end do 
        pexp = pexp + 1.
c        
c  this result imitates the original formula
c  
        dsx=ds*(pexp)
c--------------------------------------------------------------------
c  end of substitution
c--------------------------------------------------------------------
         if(dsx .gt. 0.40) dsx=0.40
         if(dsx .lt. 0.05) dsx=ds
         ds=dsx
         h=wx/ds

c **  correction of snow depth and density depending on liquid water
c **  during snowmelt. assumed that 13% of liquid water can be stored
c **  in snow per day during snowmelt till snow density 0.40

         if((tsnowx .ge. 0.) .and. (h .ne. 0.)) then
            dw=0.13*dt/24.
            ds=ds*(1.-dw)+dw
            if(ds .gt. 0.40) ds=0.40
            h=wx/ds
         endif
                   
      else
         if(wx .le. 0.) then
            wx=0.
            h=0.
            ds=0.
         else
            h=wx/ds
         endif
 
      endif

c **  replace previous liquid water and change depth to m

c    7 
      hc=h*0.01

      return
      end