      
      Subroutine tribubble(A,n)
	
      

      real A(2000)

       
        



      call Bubble(A,n)


   
  
c      10 format (10I5)
      return
      END

!return p,q in ascending order
      Subroutine Order(p,q)
      real p,q,temp
      if (p.lt.q) then
      temp=p
      p=q
      q=temp
      end if
      return
      end

!Buuble sorting of integer array A
      Subroutine Bubble(A, n)
      real A(10)
      do i=1, n
        do j=n, i+1, -1
         call Order(A(j-1), A(j))
        end do
      end do
      return
      end

!end of file bubble.f90



