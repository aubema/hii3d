C
C  smooth the 3D file according to the window used for statistics
C
      subroutine interp(n1,n2,n3,var1,vars,taille,rc,toverr)
      integer taille,i,j,k,ii,jj,kk,n1,n2,n3,rc,win                                          ! Size of the computational mesh in 3 directions
      real var1(401,401,401),vars(401,401,401),dist2,dmin
      real rcirc,pond(401,401,401),toverr
      do k=1,n3
         do j=1,n2
            do i=1,n1
               vars(i,j,k)=0.
               pond(i,j,k)=0.
            enddo
         enddo
      enddo
c      win=nint(sqrt(2.)*real(taille))/2+2
      win=taille/2+2
      do k=201-nint(real(rc)*toverr)-taille,201+nint(real(rc)*toverr)
     ++taille
         print*,k
         do j=201-rc-taille,201+rc+taille
            do i=201-rc-taille,201+rc+taille
                dmin=10000000.
                do ii=i-win,i+win
                  do jj=j-win,j+win
                     do kk=k-win,k+win
                       if (var1(ii,jj,kk).ge.0.) then
                       dist2=sqrt(real((ii-i)**2+(jj-j)**2+(kk-k)
     +                 **2))
c nearest neighbour
                       if (dist2.lt.dmin) then
                          dmin=dist2
                          vars(i,j,k)=var1(ii,jj,kk)
c                          vars(i,j,k)=var1(ii,jj,kk)/dist2+vars(i,j,k)
c                          pond(i,j,k)=1./dist2+pond(i,j,k)
                       endif

                       endif

c                       if (dist2.le.real(win)) then


c                       if (var1(ii,jj,kk).ge.0.) then
c                          if (dist2.eq.0.) dist2=0.0001
c                          vars(i,j,k)=var1(ii,jj,kk)/dist2+vars(i,j,k)
c                          pond(i,j,k)=1./dist2+pond(i,j,k)
c                       else
c                          if (dist2.le.real(taille/2+1)) then
c                             vars(i,j,k)=0.
c                          endif
c                       endif


c                 endif

                     enddo
                  enddo
               enddo
c               if (pond(i,j,k).ne.0.) then
c                  vars(i,j,k)=vars(i,j,k)/pond(i,j,k)
c               else
c                  vars(i,j,k)=0.
c               endif
            enddo
         enddo
      enddo
      return
      end

