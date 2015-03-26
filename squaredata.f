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

	subroutine squaredata(nbx,nby,ratio,square,ndata)
	integer nx,ny,i,j,nbx,nby,ndata(400,400),taille,demibox,ii,jj
        integer nbmax
        real ratio(400,400),square(400,400,121)                         ! k est le nombre de donnees max dans 11x11
        taille=11                                                       ! valeur maximale de 11
        nbmax=taille*taille/2
        do i=1,400
           do j=1,400
              ndata(i,j)=0
              do k=1,121
                 square(i,j,k)=0.
              enddo
           enddo
        enddo
        demibox=taille/2
	do i=1,nbx
	   do j=1,nby
              do ii=i-demibox,i+demibox
                do jj=j-demibox,j+demibox
                   if (ratio(ii,jj).ne.0.) then
                     ndata(i,j)=ndata(i,j)+1
                     square(i,j,ndata(i,j))=ratio(ii,jj)
		  endif
                 enddo
               enddo
                  if (ndata(i,j).lt.nbmax) then 
                  square(i,j,ndata(i,j))=0.
                   ndata(i,j)=0.
                  endif
             enddo
         enddo
        return
        end subroutine
