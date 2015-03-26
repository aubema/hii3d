c       Ce programme sert a calculer lecarttype et la moyenne pour le ratio, en fonction du rayon.
        subroutine moyecart (nbx,nby,square,ndata,moy,sigma)
        integer i,j,k,ndata(400,400),nbx,nby,xc,yc
        real t,e,square(400,400,121),moy(400,400),sigma(400,400)
        real r
        open(unit=1,file='rond.in',status='unknown')
           read(1,*) xc, yc
        close(unit=1)
        open(unit=1, file='sigma.txt',status='unknown')
        open(unit=2, file='moy.txt',status='unknown')
        do i=1,nbx
           do j=1,nby
	     t = 0.
	     do k=1,121
	     t = t + square(i,j,k)
               if (ndata(i,j).ne.0) then
                 moy(i,j) = t/real(ndata(i,j))
               endif
             enddo
             write(2,*) i,j,moy(i,j)
          enddo
        end do
        do i=1,nbx
           do j=1,nby
	    e = 0.
	    do k=1,121
              if (square(i,j,k).ne.0)then
	      e = e + (square(i,j,k)-moy(i,j))**2.

              endif
            enddo
                if (ndata(i,j).ne.0) then
                 e=e/real(ndata(i,j))  
                 sigma(i,j)=sqrt(e)
                  r=sqrt(real(xc+1-i)**2.+real(nby-yc-j)**2.)            
                 write(1,*) i,j,r,sigma(i,j)
                endif
          enddo
       end do
       close(unit=1)
       close(unit=2)
       return
       end subroutine
