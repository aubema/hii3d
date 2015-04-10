c       Ce programme sert a calculer lecarttype et la moyenne pour le ratio, en fonction du rayon.
c       dans un fenetre mobile de largeur "taille"
c       taille maximale= 11 (361 cellules)
        subroutine moyecart (nbx,nby,taille,square,ndata,moy,sigma)
        integer i,j,k,ndata(401,401),nbx,nby,xc,yc,kmax,taille
        real t,e,square(401,401,361),moy(401,401),sigma(401,401)
        real r
        kmax=taille*taille
        open(unit=1,file='rond.in',status='unknown')
           read(1,*) xc, yc
        close(unit=1)
        open(unit=1, file='sigma.txt',status='unknown')
        open(unit=2, file='moy.txt',status='unknown')
        do i=1+taille/2,nbx-taille/2-1
           do j=1+taille/2,nby-taille/2-1
             moy(i,j) = 0.
	     t = 0.
	     do k=1,kmax
	     t = t + square(i,j,k)
             enddo
               if (ndata(i,j).ne.0) then
                 moy(i,j) = t/real(ndata(i,j))
                 write(2,*) i,j,ndata(i,j),moy(i,j)
               endif
          enddo
        enddo
        do i=1+taille/2,nbx-taille/2-1
           do j=1+taille/2,nby-taille/2-1
            sigma(i,j)=0.
            if (ndata(i,j).ne.0) then  
	       e = 0.
	       do k=1,kmax
	          if (square(i,j,k).ne.0.) then
                     e=e+(square(i,j,k)-moy(i,j))**2.
                  endif
               enddo
               e=e/real(ndata(i,j))  
               sigma(i,j)=sqrt(e)
               r=sqrt(real(xc-i)**2.+real(yc-j)**2.)            
               write(1,*) i,j,ndata(i,j),sigma(i,j)
             endif
          enddo
       enddo
       close(unit=1)
       close(unit=2)
       return
       end
