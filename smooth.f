C
C  smooth the 3D file according to the window used for statistics
C
      subroutine smooth(n1,n2,n3,var1,vars,taille)
      integer taille,i,j,k,ii,jj,kk,n1,n2,n3                                       ! Size of the computational mesh in 3 directions
      real var1(401,401,401),vars(401,401,401)
      do k=1,n3
         do j=1,n2
            do i=1,n1
               vars(i,j,k)=0.
            enddo
         enddo
      enddo
      do k=1+taille/2+1,n3-taille/2-1
         do j=1+taille/2+1,n2-taille/2-1
            do i=1+taille/2+1,n1-taille/2-1
               do ii=i-taille/2,i+taille/2
                  do jj=j-taille/2,j+taille/2
                     do kk=k-taille/2,k+taille/2
                       vars(i,j,k)=var1(ii,jj,kk)+vars(i,j,k)
                     enddo
                  enddo
               enddo
               vars(i,j,k)=vars(i,j,k)/taille/taille/taille
            enddo
         enddo
      enddo
      return
      end

