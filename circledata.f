c programme circledata.f 
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
c    extrait les donnees de T et Ne le long d'un cercle de rayons varies
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

	subroutine circledata(nbx,nby,temp,densit,tempVSr,neVSr,ndatT,
     +  ndatN)
	integer nx,ny,i,j,nbx,nby,ndatT(401),ndatN(401)
c        (xc,yc)=coordoné du centre
c        (xr,yr)=coordoné du rayon 
        real rt,temp(401,401),densit(401,401),yc,xc,x,y,rcirc,xr,yr
        real xcirc,ycirc,tempVSr(401,3000),neVSr(401,3000),rmin
c 3000 correspond a un arrondi de 2*pi*900 i.e. circonference du plus grand rayon possible
c dimension max de 3000 permet de traiter une nebuleuse de 401 pixel de rayon au max 
c      	 r=rayon de reference
c 	 rt=rayon temporaire calculé à chaque point
c	inserer le programe java pour les coordonées ici
        rmin=5.
        do i=1,401
           ndatT(i)=0
           ndatN(i)=0
           do j=1,3000
              tempVSr(i,j)=0.
              neVSr(i,j)=0.
           enddo
        enddo
	open(unit=1,file='rond.in',status='old')
        read(1,*) xc,yc
        read(1,*) xr,yr
        rcirc=sqrt((xr-xc)**2.+(yr-yc)**2.)
	rcirc = 10000
	do i=1,nbx
	   do j=1,nby
                if (int(temp(i,j)).ne.0) then
                  x=real(i)
                  y=real(j)
		  rt=sqrt((x-xc)**2.+(y-yc)**2.)
		  if ((rt.le.rcirc).and.(rt.gt.rmin)) then
                     ndatT(nint(rt))=ndatT(nint(rt))+1
                     tempVSr(nint(rt),ndatT(nint(rt)))=temp(i,j)
		  endif
                  endif
             enddo
         enddo
         do i=1,nbx
	   do j=1,nby 
               if (int(densit(i,j)).ne.0) then
                  x=real(i)
                  y=real(j)
		  rt=sqrt((x-xc)**2.+(y-yc)**2.)
		  if ((rt.le.rcirc).and.(rt.gt.rmin)) then
                     ndatN(nint(rt))=ndatN(nint(rt))+1
                     neVSr(nint(rt),ndatN(nint(rt)))=densit(i,j)
		  endif
                endif

	   enddo
	enddo
        close(unit=1)
        return
        end subroutine
