program mainprog 
c This is a program that takes a 2D image of interstellar medium and infers its 3D configuration. This was created by the GRAPHYCS group 
c
c Copyright (C) 2010 Martin Aube
c
c This program is free software: you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation, either version 3 of the License, or
c (at your option) any later version.
c
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
c GNU General Public License for more details.
c
c You should have received a copy of the GNU General Public License
c along with this program. If not, see <http://www.gnu.org/licenses/>.
c
c Contact: martin.aube@cegepsherbrooke.qc.ca
c
c
implicit none

real matrice3D,imagex,imagey,imagez,tempfinalm(700,1200),densitm(700,1200)
integer:: ncols,nlines,numx,numy,numz


call lines2TNe(tempfinalm,densitm, ncols, nlines) !programme (ALEXANDRINE!) (donner nom de l'image)

!boucle (Ne/T)

call stat.f ! ELPHES(ELie/raPHael/johannES)

!call 2Dto3D, produit matrice 3D


call Moyenne(numx,numy,numz,matrice3D,imagex,imagey,imagez) !programme donnant image 2D

call Modifications(nh) !programme qui compare les deux images et les modifie (GuiMauve)
!fin boucle(Ne/T)
end program
