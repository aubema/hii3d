c programme hii3d.f 
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
c    Decrire ici le programme dans son ensemble
c
c
c
c
c
c
c
c
c    Copyright (C) 2014  Alexandre Carbonneau, Catherine Masson, Maude Roy-Labbe, Martin Aubé, 
c    Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
c             
        real densitp(900,900),tempp(900,900), square(400,400,121)
	real neVSr(450,3000),temp(900,900),temp3d(450,450,450)
        real moyt(450),moy(400,400),sigmat(450),sigma(400,400)
        real Nvsr(450,3000),SIIratio(400,400),NIIratio(900,900)
        real ne3d(450,450,450),R3D,xr,xc,yr,yc,rcirc
        real intmin, intmax, SII3d(401,401,401)
        integer Ntot(450),siz
        integer nbx, nby, ndata(400,400),ndatN(450),i,j,r,k
        character*20 namef(30)
        character*40 outfile
        character*12 nom
c on prend les données de raies d'emission qu'on transforme en temperature et densite electronique avec Osterbrock
        call TNelines(nbx,nby,tempp,densitp,SIIratio,NIIratio)
       
c extraire des tables de donnees par rayon constant
        intmin=0.45
        intmax=1.43
        call squaredata(nbx,nby,SIIratio,square,ndata)
        do k=1,121
c        print*, square(179,132,k)
        enddo

        call histo3(square)
c        call circledata(nbx,nby,tempp,densitp,tempVSr,neVSr,
c     +  ndatT,ndatN)
c        do i=1,450
c            do j=1,3000
c               if (neVSr(i,j).ne.0) then 
c               print*, neVSr(i,j)
c               endif 
c            enddo
c        enddo
        
c        call histo(tempVSr,neVSr,Nvsr)
        call moyecart(nbx,nby,square,ndata,moy,sigma)
c        print*, sigma(187,222)
        call circle(sigma,nbx,nby)
c        print*, sigma(187,222)
c       SII min=0,45  max=1,43 graph
c       NII min=      max=     graph
c faire les matrices 3D
	open(unit=1,file='rond.in',status='old')
          read(1,*) xc,yc
          read(1,*) xr,yr
          rcirc=sqrt((xr-xc)**2.+(yr-yc)**2.)
        close(unit=1)
        do i=1,401
            do j=1,401
                do k=1,401
                 SII3d(i,j,k)=0. 
                enddo
            enddo
        enddo  
        do i=201-nint(rcirc)-10,201+nint(rcirc)+10
           print*,i,'/401'
           do j=201-nint(rcirc)-10,201+nint(rcirc)+10
              do k=201-nint(rcirc)-10,201+nint(rcirc)+10
                 call gaussienne(moy,sigma,i,j,k,R3D,xc,yc,nby,
     +                intmin,intmax)
                 SII3d(i,j,k)=R3D
              enddo
           enddo
         enddo
         siz=401
         call CreateMatrix(SII3d,siz,siz,siz)
        stop
        end
