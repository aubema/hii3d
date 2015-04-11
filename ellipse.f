c programme pour elargir la distribution en fonction de l epaisseur 
c presumee de la nebuleuse de forme ellipsoide
        subroutine ellipse(sigma,nbx,nby,toverr,rcirc)
	real xc,yc,xr,xy,rcirc,rij,sigma(401,401),toverr
        integer r,nbx,nby
        open(unit=1,file='rond.in',status='old')
        read(1,*) xc,yc
        read(1,*) xr,yr
c        rcirc=sqrt((xr-xc)**2.+(yr-yc)**2.)
        close(unit=1)
        do i=1,nbx
            do j=1,nby
                rij=sqrt((xc-i)**2.+(nby-j)**2.)
                if (rij.le.rcirc) then
                   sigma(i,j)=sigma(i,j)*sqrt(2.*toverr*rcirc*
     +             sqrt(1-(rij/rcirc)**2.))
                endif
            enddo
        enddo

        return
        end
