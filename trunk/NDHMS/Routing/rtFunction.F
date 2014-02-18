      subroutine exeRouting (did)
         use module_RT_data, only: rt_domain
         use module_namelist, only: nlst_rt

#ifdef MPP_LAND  
         use module_mpp_land 
#endif

       
         implicit none
         integer did



           IF (nlst_rt(did)%SUBRTSWCRT.EQ.1 .or. nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) THEN

              RT_DOMAIN(did)%QSTRMVOLRT_DUM = RT_DOMAIN(did)%QSTRMVOLRT
              RT_DOMAIN(did)%LAKE_INFLORT_DUM = RT_DOMAIN(did)%LAKE_INFLORT

#ifdef HYDRO_D
               write(6,*) "*****yw******start drive_RT "
#endif



!          write(6,*) "yyww RT_DOMAIN(did)%SH2OX(15,1,7)= ", RT_DOMAIN(did)%SH2OX(15,1,7)

         call drive_RT( RT_DOMAIN(did)%IX,RT_DOMAIN(did)%JX,nlst_rt(did)%NSOIL,&
             RT_DOMAIN(did)%IXRT,RT_DOMAIN(did)%JXRT,  &
             RT_DOMAIN(did)%SMC,RT_DOMAIN(did)%STC,RT_DOMAIN(did)%SH2OX, &
             RT_DOMAIN(did)%SICE, &
             RT_DOMAIN(did)%INFXSRT,RT_DOMAIN(did)%SFCHEADRT,RT_DOMAIN(did)%SMCMAX1,&
             RT_DOMAIN(did)%SMCREF1,RT_DOMAIN(did)%LKSAT,  &
             RT_DOMAIN(did)%SMCWLT1, RT_DOMAIN(did)%SMCRTCHK,RT_DOMAIN(did)%DSMC,&
             RT_DOMAIN(did)%ZSOIL, RT_DOMAIN(did)%SMCAGGRT,&
             RT_DOMAIN(did)%STCAGGRT,RT_DOMAIN(did)%SH2OAGGRT, &
             RT_DOMAIN(did)%SLDPTH,RT_DOMAIN(did)%VEGTYP,RT_DOMAIN(did)%SOLDEPRT,&
             RT_DOMAIN(did)%INFXSAGGRT,RT_DOMAIN(did)%DHRT,RT_DOMAIN(did)%QSTRMVOLRT, &
             RT_DOMAIN(did)%QBDRYRT,RT_DOMAIN(did)%LAKE_INFLORT,&
             RT_DOMAIN(did)%SFCHEADSUBRT,RT_DOMAIN(did)%INFXSWGT,&
             RT_DOMAIN(did)%LKSATRT, &
             RT_DOMAIN(did)%INFXSUBRT,RT_DOMAIN(did)%OVROUGHRT,&
             RT_DOMAIN(did)%QSUBRT,RT_DOMAIN(did)%ZWATTABLRT, &
             RT_DOMAIN(did)%QSUBBDRYRT,   &
             RT_DOMAIN(did)%RETDEPRT,RT_DOMAIN(did)%SOXRT,RT_DOMAIN(did)%SOYRT,&
             RT_DOMAIN(did)%SUB_RESID,RT_DOMAIN(did)%SMCRT,&
             RT_DOMAIN(did)%SMCMAXRT,RT_DOMAIN(did)%SMCWLTRT, &
             RT_DOMAIN(did)%SH2OWGT,RT_DOMAIN(did)%LAKE_MSKRT,&
             RT_DOMAIN(did)%CH_NETRT, RT_DOMAIN(did)%dist, &
             RT_DOMAIN(did)%LSMVOL,RT_DOMAIN(did)%DSMCTOT, &
             RT_DOMAIN(did)%SMCTOT1,&
             RT_DOMAIN(did)%SMCTOT2,RT_DOMAIN(did)%suminfxs1, &
             RT_DOMAIN(did)%suminfxsrt,RT_DOMAIN(did)%SO8RT, &
             RT_DOMAIN(did)%SO8RT_D,nlst_rt(did)%AGGFACTRT,  &
             nlst_rt(did)%SUBRTSWCRT,nlst_rt(did)%OVRTSWCRT, &
             RT_DOMAIN(did)%LAKE_CT, RT_DOMAIN(did)%STRM_CT,    &
             nlst_rt(did)%RT_OPTION,RT_DOMAIN(did)%OV_ROUGH, &
             RT_DOMAIN(did)%INFXSAGG1RT,RT_DOMAIN(did)%SFCHEADAGG1RT,&
             RT_DOMAIN(did)%SFCHEADAGGRT,&
             nlst_rt(did)%DTRT, &
             nlst_rt(did)%DT,RT_DOMAIN(did)%LAKE_INFLOTRT,&
             RT_DOMAIN(did)%QBDRYTRT,RT_DOMAIN(did)%QSUBBDRYTRT,&
             RT_DOMAIN(did)%QSTRMVOLTRT,RT_DOMAIN(did)%q_sfcflx_x,&
             RT_DOMAIN(did)%q_sfcflx_y,RT_DOMAIN(did)%LKSATFAC,&
             RT_DOMAIN(did)%OVROUGHRTFAC,rt_domain(did)%dist_lsm(:,:,9) )

            RT_DOMAIN(did)%QSTRMVOLRT_TS = RT_DOMAIN(did)%QSTRMVOLRT-RT_DOMAIN(did)%QSTRMVOLRT_DUM
            RT_DOMAIN(did)%LAKE_INFLORT_TS = RT_DOMAIN(did)%LAKE_INFLORT-RT_DOMAIN(did)%LAKE_INFLORT_DUM

#ifdef HYDRO_D
           write(6,*) "*****yw******end drive_RT "
#endif
        end if





      end subroutine  exeRouting

      subroutine time_seconds(i3)
          integer time_array(8)
          real*8 i3
          call date_and_time(values=time_array)
          i3 = time_array(4)*24*3600+time_array(5) * 3600 + time_array(6) * 60 + &
                time_array(7) + 0.001 * time_array(8)
          return
      end subroutine time_seconds


