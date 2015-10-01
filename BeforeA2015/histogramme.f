        subroutine histog(tempVSr)
        integer i,j,n
        real b1,b2, tempVSr(450,3000)
        b1=7000.
        do i=1,80
        n=0
        b2=b1+100.
        do j=1,3000
        if ((tempVSr(60,j).gt.b1).and.(tempVSr(60,j).le.b2)) then
        n=n+1
        end if
        end do
          print*, (b1+b2)/2., n
        b1=b2
        
        end do
        return
        end subroutine 
