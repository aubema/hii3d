c programme extrant2d.f 
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
c    Ce programme est une routine servant a l'ecriture des extrants 2D.
c    Il transcipt l'image dans un fichier.
c
c    Copyright (C) 2014  Alexandre Carbonneau, Catherine Masson, Maude Roy-Labbe, Martin Aubé, 
c    Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
       subroutine extrant2d (outfil,valeur,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)
       real valeur(401,401),xcell0,ycell0,gain,offset
       real valprint(401,401)
       integer i,j,nbx,nby,valmax
       character*12 nom
       character*40 outfil
c       print*,'Ecriture du fichier de ',nom,': ',outfil
       open(unit=1,file=outfil,status='unknown')
       write(1,1000) ! Tag P2).
       write(1,1001) xcell0
       write(1,1002) ycell0
       write(1,1003) pixsiz
       write(1,1004) gain
       write(1,1005) offset
       write(1,*) nbx,nby,valmax
       do i=1,nbx ! Debut de la boucle sur toutes les cases en x.
       do j=1,nby ! Debut de la boucle sur toutes les cases en y.
       valprint(i,j)=(valeur(i,j)-offset)/gain ! Transformation des donnees avec le gain et l'offset et recherche
       enddo ! Fin de la boucle sur toutes les cases en y.
       enddo ! Fin de la boucle sur toutes les cases en x.
       write(1,*) ((nint(valprint(i,j)),i=1,nbx),j=1,nby) ! Ecriture de toutes les donnees qui sont ensuite inscrites
c ! dans la matricel. Ce sont des boucles imbriquees dans la
c ! fonction "write" qui couvrent tout le domaine delimite par nbx et nby.
c ! L'increment de la boucle sur les ranges (latitute) est de -1 car
c ! on considere que dans les fichiers,
c ! le nombre en haut a gauche des donnees est a la
c ! coordonnee (1,nby).
       close(1) ! Fermeture du fichier relief.pgm.

 1000 format('P2')
 1001 format('# lon0 ',F9.5)
 1002 format('# lat0 ',F9.5)
 1003 format('# pixsiz ',F9.3)
 1004 format('# gain ',F15.11)
 1005 format('# offset ',F15.11)
       return
       end


