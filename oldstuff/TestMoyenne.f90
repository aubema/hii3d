
! subroutine Moyenne(numx,numy,numz,matrice3D)


  implicit none

  integer :: i,j,k,numx,numy,numz,numxx,numyy,numzz,valmax
  real :: datao(700,1000,700) 
  real :: itempo,imagex(700,700),imagey(700,700),imagez(700,700),xcell0,ycell0,gain,offset,minx,maxx
  real :: mintotal,mintempo,maxtotal,maxtempo,pixsiz,miny,maxy,minz,maxz
  character*12 :: nom	
  character*40 :: outfile
  numx=1200
  numy=1200  ! À titre d'exemple seulement; elles devraient être déclarées dans le programme principal
  numz=1200
  maxtotal=0.
  maxtempo=0.
  mintotal=39842098. !trouver une valeur	
  mintempo=39842098. !trouver une valeur  
  minx=0.
  maxx=0.
  miny=0.
  maxy=0.
  minz=0.
  maxz=0.			
  outfile='Integrationx.pgm'
  nom='Integrationx'
  xcell0=0.
  ycell0=0.
  pixsiz=1.
  
  valmax=65535
  

  

16 format(a)

18 format(i6)

open(unit = 1,file='formationmatrice.vtk',status="old") !remplacer formationmatrice.vtk par le bon fichier

  ! Le bloc suivant créé l'entête du format .vtk


read(1,*)

read(1,*)

read(1,*)
   
read(1,*)

read(1,*)

read(1,*)

read(1,*) numx
read(1,*) numy
read(1,*) numz

read(1,*)

read(1,*)
 
read(1,*)

read(1,*)

read(1,*)

read(1,*)

do i=1, (numx)
   do j=1, numy
	imagez(i,j)=0.
   end do
end do

do j=1, (numy)
   do k=1, numz
	imagex(j,k)=0.
   end do
end do

do i=1, (numx)
   do k=1, numz
	imagey(i,k)=0.
   end do
end do
!fichier .vtk la hierarchie des boucles est z y x. Le k=1 est au "fond" de la matrice par rapport a la ligne de visee.

!lecture des donnees dans le fichier .vtk
  
  do k = 1,(numz)
     do j = 1, (numy)
        do i = 1, (numx)
        read(1,*) datao(i,j,k) 
	! print*, i,j,k, datao(i,j,k)
	end do
     end do
  end do

!moyenne en x

do j=1, (numy)
    do k=1, (numz)
	do i=1, (numx)
	  imagex(j,k)=imagex(j,k)+datao(i,j,k)
	end do
   	imagex(j,k) = imagex(j,k)/real(numx) 
   end do
end do


!max en x

 do j = 1, numy
	do k = 1, numz
		maxtempo = (imagex(j,k))
		if (maxtempo > maxtotal) then
		maxtotal=maxtempo
		end if
 	end do
end do
	
maxx = maxtotal
maxtotal=0.
!min en x



 do j = 1, numy
	do k = 1, numz
		mintempo = (imagex(j,k))
		if (mintempo < mintotal) then
		mintotal=mintempo
		end if
 	end do
end do

minx = mintotal
mintotal=123213155131551.


!gain et offset

gain= ((maxx-minx)/65535.)
offset = minx


!imprime image moyennee en x


call extrant2d(outfile,imagex,nom,xcell0,ycell0,pixsiz,gain,offset,numy,numz,valmax)


!moyenne en y

do i=1, numx
    do k=1, numz
	do j=1, numy
	  imagey(i,k)=imagey(i,k)+datao(i,j,k)
	end do
        imagey(i,k) = imagey(i,k)/real(numy)
    end do
end do

!max en y

 do i = 1, numx
	do k = 1, numz
		maxtempo = (imagey(i,k))
		if (maxtempo > maxtotal) then
		maxtotal=maxtempo
		end if
 	end do
end do
	
maxy = maxtotal
maxtotal=0.
!min en y



 do i = 1, numx
	do k = 1, numz
		mintempo = (imagey(i,k))
		if (mintempo < mintotal) then
		mintotal=mintempo
		end if
 	end do
end do

miny = mintotal
mintotal=123213155131551.


!gain et offset

gain= ((maxy-miny)/65535.)
offset = miny


!imprime image moyennee en y
outfile='Integrationy.pgm'
nom='Integrationy'

call extrant2d(outfile,imagey,nom,xcell0,ycell0,pixsiz,gain,offset,numx,numz,valmax)


!moyenne en z

do i=1, numx
    do j=1, numy
	do k=1, numz
 	  imagez(i,j)=imagez(i,j)+datao(i,j,k)
	end do
	imagez(i,j) = imagez(i,j)/real(numz)
    end do
end do

!max en z

 do i = 1, numx
	do j = 1, numy
		maxtempo = (imagez(i,j))
		if (maxtempo > maxtotal) then
		maxtotal=maxtempo
		end if
 	end do
end do
	
maxz = maxtotal
maxtotal=0.

!min en z


 do i = 1, numx
	do j = 1, numy
		mintempo = (imagez(i,j))
		if (mintempo < mintotal) then
		mintotal=mintempo
		end if
 	end do
end do

minz = mintotal
mintotal=123213155131551.


!gain et offset

gain= ((maxz-minz)/65535.)
offset = minz


!imprime image moyennee en z
outfile='Integrationz.pgm'
nom='Integrationz'

call extrant2d(outfile,imagez,nom,xcell0,ycell0,pixsiz,gain,offset,numx,numy,valmax)


!print*, minx, maxx, miny, maxy, minz, maxz, gain, offset	    
return
end

