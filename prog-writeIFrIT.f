c programme writeIFrIT.f 
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
c    Ce programme sert a transcrire une matrice en donnees scalaires uniformes
c    compatibles avec le programme IFrIT. Il cree le fichier avec la matrice envoyee.
c
c
c    Copyright (C) 2014   Martin Aubé, Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
c
c Les commentaires en bleu representent les commandes necessaire pour 
c que la routine fonctionne avec des scalaires a 3 variables. 
c Il suffit d'enlever les commentaires pour que cela fonctionne, mais
c ce n'est pas utile pour l'instant.

      subroutine WriteIFrIT(n1,n2,n3,var1,filena)
      integer n1, n2, n3                                                  ! Size of the computational mesh in 3 directions
      real var1(401,401,401)
c      real var2(n1,n2,n3)                                                ! Three scalar variables
c      real*4 var3(n1,n2,n3)  
      character*40 filena                                                 ! Name of the file
      open(unit=1, file=filena)  
      write(1,*) n1, n2, n3
      do k=1,n3
         do j=n2,1,-1
            do i=1,n1
c              write(1,*) var1(i,j,k), var2(i,j,k), var3(i,j,k)
               write(1,*) var1(i,j,k)
            enddo
         enddo
      enddo
      close(1)
      return
      end

