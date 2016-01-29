c programme ellipse.f 
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
c    Ce programme elargit la distribution des donnees pour respecter l'hypothese d'une
c    nebuleuse de forme ellipsoidale. Pour ce faire, le programme elargit l'ecart-type 
c    de sorte a l'adapter a une ellipse.
c
c    Copyright (C) 2014   Martin Aubé, Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
        subroutine ellipse(sigma,nbx,nby,toverr,rcirc)
 real xc,yc,xr,xy,rcirc,rij,sigma(401,401),toverr
c toverr est l etirement de l'ellipse (a valider grand axe sur petit axe? )
        integer nbx,nby
        open(unit=1,file='rond.in',status='old')
        read(1,*) xc,yc
        close(unit=1)
c La boucle fait en sorte de modifier tous les sigmas existants.
        do i=1,nbx
            do j=1,nby
                rij=sqrt((xc-i)**2.+(nby-j)**2.)
                if (rij.le.rcirc) then
                   sigma(i,j)=sigma(i,j)*sqrt(2.*toverr*rcirc*
     +             sqrt(1-(rij/rcirc)**2.))
                endif
            enddo
        enddo
        return
        end
