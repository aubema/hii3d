c programme gaussienne.f 
c    DATAM27!!!!!!!!!!!!!!!!!
c   
c    Copyright (C) 2012  Martin Aube
c
c    This program is free software: you can redistribute it and/or modify
c    it under the terms of the GNU General Public License as published by
c    the Free Software Foundation, either version 3 of the License, or
c    (at your option) any later version.
c
c    This program is distributed in the hope that it will be useful,
c    but WITHOUT ANY WARRANTY; without even the implied warranty of
c    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c    GNU General Public License for more details.
c
c    You should have received a copy of the GNU General Public License
c    along with this program.  If not, see <http://www.gnu.org/licenses/>.
c
c    Contact: martin.aube@cegepsherbrooke.qc.ca
c
c    Ce programme sert a fournir une valeur aleatoire de ratio pour un point
c    donne de notre matrice en 3 dimensions, en tirant aleatoirement sur une
c    fonction gaussienne construite a l'aide des moyennes et ecart-types.
c
c    Copyright (C) 2014   Martin Aubé, Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
        subroutine gaussienne(moy,sigma,i,j,k,nby,R3D,intmin,
     +  intmax)
        real moy(401,401),sigma(401,401),alea(2000000)
        real e,pi,Fmax,xmin,xmax,Inte,y,R3D,F,r,phi
        real intmin,intmax,random
        integer i,j,n,m,k,ii,jj
        e=2.71828182846
        pi=3.14159265359
        n=1
        xmin=intmin
        xmax=intmax
        if (sigma(ii,jj).ne.0.) then
           Fmax=1./(sigma(ii,jj)*sqrt(2.*pi))/100.
           Inte=(xmax-xmin)/100.
           y=xmin+Inte/2.
           do while (y.le.xmax)
              y=y+Inte
              F=1./(sigma(ii,jj)*sqrt(2.*pi))*e**(-1.*(y-moy(ii,jj))
     +        **2./(2.*sigma(ii,jj)**2.))
              do m=1,nint(F/Fmax)
                 alea(n)=y
                 n=n+1
              enddo
           enddo
c On tire aleatoirement dans la gaussienne, et la valeur R3D sera ensuite
c ajoutee a la matrice 3D dans hii3d.
           random=rand()
           R3D=alea(nint(random*real(n)))
        else
           R3D=0.
        endif
        return
        end
