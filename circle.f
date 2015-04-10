c programme pour elargir la distribution en fonction de l epaisseur 
c presumee de la nebuleuse 
        subroutine circle(sigma,nbx,nby)
	real xc,yc,xr,xy,rcirc,rij,sigma(401,401)
        integer r,nbx,nby
        open(unit=1,file='rond.in',status='old')
        read(1,*) xc,yc
        read(1,*) xr,yr
        rcirc=sqrt((xr-xc)**2.+(yr-yc)**2.)
        close(unit=1)
        do i=1,nbx
            do j=1,nby
                rij=sqrt((xc-i)**2.+(nby-j)**2.)
                if (rij.le.rcirc) then
                   sigma(i,j)=sigma(i,j)*sqrt(2.*sqrt(rcirc**2.-
     +             rij**2.))
                endif
            enddo
        enddo
        return
        end
