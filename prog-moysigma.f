c programme moysigma.f 
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
c    Ce programme sert a calculer la moyenne et l'ecart-type pour chaque point de la
c    nebuleuse. Pour chaque point, ou coordonnee, la moyenne et l'ecart-type sont calculés
c    a partir des donnees de la matrice square, c'est-a-dire, a partir d'une fenetre mobile
c    de dimensions taille x taille. Ces moyennes et ecart-types sont stockés sous forme de 
c    matrices et de fichiers textes.
c

c    Copyright (C) 2014  Martin Aubé, Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
        subroutine moysigma (nbx,nby,taille,square,ndata,moy,sigma)
        integer i,j,k,ndata(401,401),nbx,nby,kmax,taille
        real t,e,square(401,401,225),moy(401,401),sigma(401,401)
        
        kmax=taille*taille
    
c On ouvre les fichiers textes.
        open(unit=1, file='sigma.txt',status='unknown')
        open(unit=2, file='moy.txt',status='unknown')
c On calcul la moyenne en chaque point.
        do i=1+taille/2,nbx-taille/2-1
           do j=1+taille/2,nby-taille/2-1
             moy(i,j) = 0.
	     t = 0.
	     do k=1,kmax
	     t = t + square(i,j,k)
             enddo
               if (ndata(i,j).ne.0) then
                 moy(i,j) = t/real(ndata(i,j))
                 write(2,*) i,j,ndata(i,j),moy(i,j)
               endif
          enddo
        enddo
c On calcul l'ecart-type en chaque point.
        do i=1+taille/2,nbx-taille/2-1
           do j=1+taille/2,nby-taille/2-1
            sigma(i,j)=0.
            if (ndata(i,j).ne.0) then  
	       e = 0.
	       do k=1,kmax
	          if (square(i,j,k).ne.0.) then
                     e=e+(square(i,j,k)-moy(i,j))**2.
                  endif
               enddo
               e=e/real(ndata(i,j))  
               sigma(i,j)=sqrt(e)       
               write(1,*) i,j,ndata(i,j),sigma(i,j)
             endif
          enddo
       enddo
c On ferme les fichiers textes.
       close(unit=1)
       close(unit=2)
       return
       end
