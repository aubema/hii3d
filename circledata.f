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

	subroutine circledata(nbx,nby,temp,densit,tempVSr,neVSr,ndata)
	integer nx,ny,i,j,nbx,nby,ndata(450)
c        (xc,yc)=coordoné du centre
c        (xr,yr)=coordoné du rayon 
        real rt,temp(900,900),densit(900,900),yc,xc,x,y,rcirc,xr,yr
        real xcirc,ycirc,tempVSr(450,3000),neVSr(450,3000)
c 3000 correspond a un arrondi de pi*900 i.e. circonference du plus grand rayon possible
c dimension max de 3000 permet de traiter une nebuleuse de 450 pixel de rayon au max 
c      	 r=rayon de reference
c 	 rt=rayon temporaire calculé à chaque point
c	inserer le programe java pour les coordonées ici
        print*,'debut'
        do i=1,450
           ndata(i)=1
           do j=1,3000
              tempVSr(i,j)=0.
              neVSr(i,j)=0.
           enddo
        enddo
	open(unit=1,file='rond.in',status='old')
        read(1,*) xc,yc
        read(1,*) xr,yr
        rcirc=sqrt((xr-xc)**2.+(yr-yc)**2.)
        print*,rcirc,'toto'
	do i=1,nbx
	   do j=1,nby
                if (int(temp(i,j)).ne.0) then
                  x=real(i)
                  y=real(j)
		  rt=sqrt((x-xc)**2.+(y-yc)**2.)
                  print*,int(rt)
		  if (rt.le.rcirc) then
                     tempVSr(nint(rt),ndata(nint(rt)))=temp(i,j)
                     neVSr(nint(rt),ndata(nint(rt)))=densit(i,j)
                     ndata(nint(rt))=ndata(nint(rt))+1
		  endif
                endif
	   enddo
	enddo
        close(unit=1)
        return
        end
