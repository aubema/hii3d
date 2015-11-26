c programme en-sigma.f 
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
c    Ce programme elargit la distribution des donnees pour en fonction du nombre de cellules
c    montant un flag = 2 dans la matrice fill(i,j,k)
c
c    Copyright (C) 2014   Martin Aubé, Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
        subroutine ensigma(sigma,nbx,nby,fill)
	real sigma(401,401),nvoxel
        integer fill(401,401,401)
c toverr est l etirement de l'ellipse (a valider grand axe sur petit axe? )
        integer nbx,nby
        open(unit=1,file='rond.in',status='old')
        read(1,*) xc,yc
        close(unit=1)
c La boucle fait en sorte de modifier tous les sigmas existants.
        do i=1,nbx
            do j=1,nby
                nvoxel=0.
                do k=1,401
                   if (fill(i,j,k).eq.2) then
                      nvoxel=nvoxel+1.
                      print*,'fill'
                   endif
                enddo
                sigma(i,j)=sigma(i,j)*sqrt(nvoxel)
            enddo
        enddo
        return
        end