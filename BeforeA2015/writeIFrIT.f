C
C  Write to text uniform scalars data file
C
      subroutine WriteIFrIT(n1,n2,n3,var1,filena)
      integer n1, n2, n3                                                  ! Size of the computational mesh in 3 directions
      real var1(401,401,401)
c      real var2(n1,n2,n3)                                                ! Three scalar variables
c      real*4 var3(n1,n2,n3)  
      character*40 filena                                                 ! Name of the file
      open(unit=1, file=filena)  
      write(1,*) n1, n2, n3
      do k=1,n3
         do j=n2,1,-1
            do i=1,n1
c               write(1,*) var1(i,j,k), var2(i,j,k), var3(i,j,k)
               write(1,*) var1(i,j,k)
            enddo
         enddo
      enddo
      close(1)
      return
      end

