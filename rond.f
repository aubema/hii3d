c	cette routine sert à couper en rond centrer	
	subroutine rond (nbx,nby,matrice)
	integer nx,ny,i,j,nbx,nby
c        (xc,yc)=coordoné du centre
c        (xr,yr)=coordoné du rayon 
        real rt,matrice(900,900),yc,xc,x,y,rcirc,xr,yr
        real xcirc,ycirc
c      	 r=rayon de reference
c 	 rt=rayon temporaire calculé à chaque point
c	inserer le programe java pour les coordonées ici
	open(unit=1,file='rond.in',status='old')
        read(1,*) xc,yc
        read(1,*) xr, yr
       



        print*, 'bonjour'

        rcirc=sqrt((xr-xc)**2.+(yr-yc)**2.)
	do i=1,nbx
	   do j=1,nby
                x=real(i)
                y=real(j)
		rt=sqrt((x-xc)**2.+(y-yc)**2.)
		if (rt.gt.rcirc) then
                   matrice(i,j)=0.
		endif
	   enddo
	enddo
        close(unit=1)
        return
        end
	
