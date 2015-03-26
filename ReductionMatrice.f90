subroutine CreateMatrix(B,numx,numy,numz)

  ! Cette routine prend une matrice, créé l'en-tête d'un fichier VTK et la réduit d'un facteur 27 pour en réduire la taille. 
  implicit none

  integer :: i,j,k,m,n,o,temp,numx,numy,numz,x,y,z
  real :: B(:,:,:)
  character nomf

16 format(a)
17 format(i6)
18 format(i9)

open(unit=2,file='formationmatrice3.vtk',status="replace")

  
! Le bloc suivant créé l'entête du format .vtk

write(unit=2, fmt=16)'# vtk DataFile Version 2.0'
write(unit=2, fmt=16)'Modélisation de nébuleuse'
write(unit=2, fmt=16)'ASCII'
write(unit=2, fmt=16)''    
write(unit=2, fmt=16)'DATASET STRUCTURED_POINTS'
write(unit=2, fmt=16)'DIMENSIONS'
write(unit=2, fmt=18) int((numx)/3.),int((numy)/3.),int((numz)/3.)
write(unit=2, fmt=16)'ORIGIN    0.000   0.000   0.000'
write(unit=2, fmt=16)'SPACING    1.000   1.000   1.000'
write(unit=2, fmt=16)''         
write(unit=2, fmt=16,advance="no")'POINT_DATA   '
write(unit=2, fmt=18)int((numx)/3.)*int((numy)/3.)*int((numz)/3.)
write(unit=2, fmt=16)'SCALARS scalars float'
write(unit=2, fmt=16)'LOOKUP_TABLE default'

  ! Chaque itération créé une donnée de la matrice
  ! Dans le ficher VTK, la hiérarchie des axes est x --> y --> z  pour ce qui est de l'ordre

  do k = 2, (int((numx+1)/3)-1)*3+1, 3
    do j = 2, (int((numy+1)/3)-1)*3+1, 3
      do i = 2, (int((numz+1)/3)-1)*3+1, 3
        temp=0
        do m = i-1, i+1
          do n = j-1, j+1
            do o = k-1, k+1
              temp=temp+B(m,n,o)
            end do
          end do
        end do

        write(unit=2, fmt=17) temp/27
      end do
    end do
  end do
return
end

