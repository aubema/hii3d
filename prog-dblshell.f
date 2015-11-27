c programme dblshell.f 
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
c    Ce programme elargit la l ecart type a chaque position selon le nombre de voxel requis
c    pour traverser un combinaison de coquille spherique double
c    l elargissement se fait pas un facteur sqrt(N), N est le nombre de voxel dans la ligne de visee
c
c    Copyright (C) 2014   Martin Aubé, Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
c    on cree une matrice 3d avec des 1 aux endroits remplis de gaz et des 0 partout ailleurs
c    les parametres sont:
c    1- l'angle de la droite entre les deux centre des coquilles par rapport a l'axe
c    de visee (anglez)
c    2- l'angle dans le plan par rapport a l'axe des x (anglex)
c    3- la distance projettee (rapp) sur le plan entre l etoile centrale et le centre d'une coquille
c         on suppose cette distance identique pour chaque coquille.
c       
c    l'equation d'une sphere centree a un point xs,ys,zs est (x-xs)**2.+(y-ys)**2.+(z-zs)**2.=rcirc**2.
c    rcirc est le rayon externe de la coquille
c    la distance entre l'etoile et le centre de la sphere est reliee au rapp par distet=rapp/sin(anglez)
c    coquille 1:
c      z1s=distet*cos(anglez)+201
c      x1s=distet*cos(anglex)+xe
c      y1s=distet*sin(anglex)+ye
c            201 est le centre de notre matrice 3d ou on decide de placer l'etoile centrale
c             xe et ye sont les coord de l'etoile centrale dans l'image
c    coquille 2:
c      z2s=201-distet*cos(anglez)
c      x2s=xe-distet*cos(anglex)
c      y2s=ye-distet*sin(anglex)
c 
c    epaisseur des coquilles = thickc
c    rhole = rayon interieur de la coquille = rcirc-thickc
        subroutine dblshell(nbx,nby,rcirc,thickc,anglez,anglex,
     +  dist,fill,xe,ye)
	real xe,ye,z1s,z2s,x1s,x2s,y1s,y2s,dist,anglez,anglex
        real rhole2,thickc,r22,r12,rcirc2,pi
        integer fill(401,401,401),i,j,k
c    fill vaut 0 en-dehors des coquilles
c    fill vaut 1 a l'interieur
c    fill vaut 2 dans l epaisseur des coquilles
        integer nbx,nby
c La boucle fait en sorte de modifier tous les sigmas existants.
      pi=3.14159265359
      if (xe.gt.ye) then
         ze=xe
      else
         ze=ye
      endif
      z1s=dist*cos(anglez)+ze
      x1s=dist*sin(anglez)*cos(anglex)+xe
      y1s=-dist*sin(anglez)*sin(anglex)+ye
      z2s=ze-dist*cos(anglez)
      x2s=xe-dist*sin(anglez)*cos(anglex)
      y2s=ye+dist*sin(anglez)*sin(anglex)
      rhole2=(rcirc-thickc)**2.
      rcirc2=rcirc**2.
      do k=1,401
        z=real(k)
        do i=1,401
          x=real(i)
          do j=1,401
            y=real(j)
            fill(i,j,k)=0
            r12=(x-x1s)**2.+(y-y1s)**2.+(z-z1s)**2.
            r22=(x-x2s)**2.+(y-y2s)**2.+(z-z2s)**2.
            if ((r12.le.rcirc2).or.(r22.le.rcirc2)) then
               fill(i,j,k)=2
            endif
            if ((r12.le.rhole2).or.(r22.le.rhole2)) then
               fill(i,j,k)=1
            endif
          enddo
        enddo
      enddo      
      return
      end
