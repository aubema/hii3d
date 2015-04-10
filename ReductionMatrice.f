        subroutine CreateMatrix(B,numx,numy,numz)

c Cette routine prend une matrice, créé l'en-tête d'un fichier VTK et la réduit d'un facteur 27 pour en réduire la taille.

        integer  i,j,k,m,n,o,numx,numy,numz,x,y,z
        real  B(401,401,401),temp
        character nomf

   16   format(a)
   17   format(f6.3)
   18   format(i9)

        open(unit=2,file='formationmatrice3.vtk',status="unknown")

  
c Le bloc suivant créé l'entête du format .vtk

        write(2,16) '# vtk DataFile Version 2.0'
        write(2,16) 'Modélisation de nébuleuse'
        write(2,16) 'ASCII'
        write(2,16) ''    
        write(2,16) 'DATASET STRUCTURED_POINTS'
        write(2,16) 'DIMENSIONS'
        write(2,18) numx,numy,numz
        write(2,16) 'ORIGIN    0.000   0.000   0.000'
        write(2,16) 'SPACING    1.000   1.000   1.000'
        write(2,16) ''         
        write(2,16) 'POINT_DATA   '
        write(2,18) numx,numy,numz
        write(2,16) 'SCALARS scalars float'
        write(2,16) 'LOOKUP_TABLE default'

c Chaque itération créé une donnée de la matrice
c Dans le ficher VTK, la hiérarchie des axes est x --> y --> z  pour ce qui est de l'ordre

        do i = 1,numx
           do j = 1, numy
               do k = 1, numz
                  write(2,17) B(i,j,k)
               end do
           end do
        end do
        close(unit=2)
        return
        end

