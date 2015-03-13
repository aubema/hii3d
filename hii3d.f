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
        real densitp(900,900),tempfinalp(900,900), tempVSr(450,3000)
	real neVSr(450,3000)
        integer nbx, nby, ndata(450)
        character*20 namef(30)
        character*40 outfile
        character*12 nom
c on prend les données de raies d'emission qu'on transforme en temperature et densite electronique avec Osterbrock
        call TNelines(nbx,nby,tempfinalp,densitp)
        print*,'etape 5'
c extraire des tables de donnees par rayon constant
        call circledata(nbx,nby,tempfinalp,densitp,tempVSr,neVsr,
     +  ndata)
        stop
        end
