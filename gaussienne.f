c       Ce programme sert a fournir une valeur aleatoire de temperature ou de densite pour un rayon donne, sur une gaussienne.
        subroutine gaussienne(moy,sigma,i,j,k,nby,R3D,xc,yc,intmin,
     +  intmax,toverr)
        real moy(401,401),sigma(401,401),alea(2000000)
        real e,pi,Fmax,xmin,xmax,Inte,y,R3D,F,r,phi
        real intmin,intmax,random
        integer i,j,n,m,k,ii,jj,xc,yc,xr,yr

        real toverr,rcirc
        integer thick2

        e=2.71828182846
        pi=3.14159265359
        n=1
        xmin=intmin
        xmax=intmax


c modelisation en rubans a rayon fixe
c        if (i.eq.201) then
c           if (j.lt.201) phi=-pi/2.
c           if (j.gt.201) phi=pi/2.
c        else
c           phi=atan(abs((real(j-201))/(real(i-201))))
c           if ((i.gt.201).and.(j.lt.201)) phi=-phi
c           if ((i.lt.201).and.(j.lt.201)) phi=phi+pi
c           if ((i.lt.201).and.(j.gt.201)) phi=pi-phi
c        endif
c        r=sqrt(real(i-201)**2.+real(j-201)**2.+real(k-201)**2.)
c        ii=nint(r*cos(phi))+xc
c        jj=nint(r*sin(phi))+yc


c modelisation avec une ellipsoide et les stats locales dans le plan image      
        open(unit=1,file='rond.in',status='old')
          read(1,*) xc,yc
          read(1,*) xr,yr
          rcirc=sqrt(real((xr-xc)**2.+(yr-yc)**2.))
        close(unit=1)
        r=sqrt(real(i-201)**2.+real(j-201)**2.)
        thick2=nint(toverr*rcirc*sqrt(1-(r/rcirc)**2.))
        if (abs(k-201).le.thick2) then
           ii=i-201+xc
           jj=j-201+xc
        




           if (sigma(ii,jj).ne.0.) then
              Fmax=1./(sigma(ii,jj)*sqrt(2.*pi))/100.
              Inte=(xmax-xmin)/100.
              y=xmin+Inte/2.
              do while (y.le.xmax)
                 y=y+Inte
                 F=1./(sigma(ii,jj)*sqrt(2.*pi))*e**(-1.*(y-moy(ii,jj))
     +           **2./(2.*sigma(ii,jj)**2.))
                 do m=1,nint(F/Fmax)
                    alea(n)=y
                    n=n+1
                 enddo
              enddo
              random=rand()
              R3D=alea(nint(random*real(n)))
           else
              R3D=0.
           endif

        else
           R3D=0.
        endif

        return
        end
