c programme squaredata.f 
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
c    Ce programme analyse la matrice de ratio SII ou NII en deplacant une fenetre
c    mobile dessus. Ainsi, pour chaque point de la nebuleuse, chaque pixel, on a
c    une matrice qui peut comprendre jusqua 401 donnees, dependemment de la taille.
c    Au final, la matrice square aura stocké les donnees d'une fenetre pour chaque
c    coordonnees.
c
c    Copyright (C) 2014 Martin Aubé, Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
        subroutine squaredata(nbx,nby,taille,ratio,square,ndata)
        integer i,j,nbx,nby,ndata(401,401),taille,demibox,ii,jj
        integer nbmin,k
        real ratio(401,401),square(401,401,225)                                   ! 225 est le nombre de valeurs dans une fenetre de 15x15, 401 est la taille max de l'image
        nbmin=10                                                                  ! nombre de donnees minimales dans la fenetre glissante pour proceder au calculs statistiques - en bas de 10 on estime que les stats ne sont pas significatives
        demibox=taille/2
c On 'vide' les matrices ndata et square, en les remplissant de zeros.
        do i=1,nbx
           do j=1,nby
              ndata(i,j)=0
              do k=1,225                                                          ! k est le nombre de donnees max dans taille x taille.
                 square(i,j,k)=0.
              enddo
           enddo
        enddo
        do i=demibox+1,nbx-demibox-1
          do j=demibox+1,nby-demibox-1
              ndata(i,j)=0
              do ii=i-demibox,i+demibox
                do jj=j-demibox,j+demibox
c Si il y a un ratio a cette coordonnee, la matrice square prend la valeur de ce ratio
c pour cette coordonnee i,j et la kieme donnee.
                   if (ratio(ii,jj).ne.0.) then
                     ndata(i,j)=ndata(i,j)+1
                     square(i,j,ndata(i,j))=ratio(ii,jj)
                   endif
                enddo
              enddo
              if (ndata(i,j).lt.nbmin) then 
                 ndata(i,j)=0
              endif
            enddo
         enddo
        return
        end
