       subroutine temperatureNII(NIIra,Ne,aptmp)
       real temp,NIIra,NIIrca,Ne, aptmp
       integer i
       temp=6000.
c       print*,NIIra
       do i=1,200
       temp=temp+100.
       NIIrca=6.91*exp(2.5e4/temp)/(1.+2.5e-3*(Ne/temp**0.5))
c       print*,temp, NIIra
          if (NIIrca.le.NIIra) then 
          aptmp=temp
          goto 100 
          endif
       enddo
c       print*,temp
 100   return 
       end

