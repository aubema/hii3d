c       Ce programme sert a calculer lecarttype et la moyenne pour le ratio, en fonction du rayon.
        subroutine moyecart (nbx,nby,square,ndata,moy,sigma)
        integer i,j,k,ndata(401,401),nbx,nby,xc,yc
        real t,e,square(401,401,361),moy(401,401),sigma(401,401)
        real r
        open(unit=1,file='rond.in',status='unknown')
           read(1,*) xc, yc
        close(unit=1)
        open(unit=1, file='sigma.txt',status='unknown')
        open(unit=2, file='moy.txt',status='unknown')
        do i=1,nbx
           do j=1,nby
             moy(i,j) = 0.
	     t = 0.
	     do k=1,361
	     t = t + square(i,j,k)
             enddo
               if (ndata(i,j).ne.0) then
                 moy(i,j) = t/real(ndata(i,j))
                 write(2,*) i,j,moy(i,j)
               endif
          enddo
        enddo
        do i=1,nbx
           do j=1,nby
            sigma(i,j)=0.
            if (ndata(i,j).ne.0) then  
	       e = 0.
	       do k=1,361
	          if (square(i,j,k).ne.0.) then
                     e=e+(square(i,j,k)-moy(i,j))**2.
                  endif
               enddo
               e=e/real(ndata(i,j))  
               sigma(i,j)=sqrt(e)
               r=sqrt(real(xc+1-i)**2.+real(nby-yc-j)**2.)            
               write(1,*) i,j,r,sigma(i,j)
             endif
          enddo
       enddo
       close(unit=1)
       close(unit=2)
       return
       end
