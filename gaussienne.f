c       Ce programme sert a fournir une valeur aleatoire de temperature ou de densite pour un rayon donne, sur une gaussienne.
        subroutine gaussienne(moy,sigma,i,j,k,R3D,xc,yc,nby,intmin,
     +  intmax)
        real moy(400,400),sigma(400,400),alea(200000000)
        real e,pi,Tmin,xmin,xmax,Inte,y,R3D,T,r,phi
        real intmin,intmax
        integer i,j,n,m,k,ii,jj
        
        e=2.71828182846
        pi=3.14159265359
        n=0
        phi=atan((real(j-201))/(real(i-201)))
        r=sqrt(real(i-201)**2.+real(j-201)**2.+real(k-201)**2.)
        ii=nint(r*cos(phi))
        jj=nint(r*sin(phi))
        xmin=intmin
        xmax=intmax
        Tmin=1./(sigma(ii,jj)*sqrt(2.*pi))*e**(-(xmin-moy(ii,jj))**2./
     +  (2.*sigma(ii,jj)**2.))
        Inte=(xmax-xmin)/1000.
        y=xmin+Inte/2.
           do while (y.le.xmax)
           y=y+Inte
           T=1./(sigma(ii,jj)*sqrt(2.*pi))*e**(-(y-moy(ii,jj))**2./
     +  (2.*sigma(ii,jj)**2.))
              do m=1,nint(T/Tmin)
                 n=n+1
                 alea(n)=y
              enddo
           enddo
           R3D=alea(nint(rand()*real(n)))
           r=sqrt(real(xc+1-ii)**2.+real(nby-yc-jj)**2.)
c           if (r.le.87.) print*,R3D,r
        return
        end subroutine

        
