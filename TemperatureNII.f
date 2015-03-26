       subroutine temperatureNII(NIIratio,Ne,aptmp)
       real temp,NIIratio,NIIratcalc,Ne, aptmp
       integer i
       temp=6000.
c       print*,NIIratio
       do i=1,200
       temp=temp+100.
       NIIratcalc=6.91*exp(2.5e4/temp)/(1.+2.5e-3*(Ne/temp**0.5))
c       print*,temp, NIIratcalc
          if (NIIratcalc.le.NIIratio) then 
          aptmp=temp
          goto 100 
          endif
       enddo
c       print*,temp
 100   return 
       end

