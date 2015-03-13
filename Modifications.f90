!compare les valeurs obetnues avec les valeurs reelles et apporte les modifications necessaires

subroutine Modifications(nh)

implicit none


integer :: i, nh,j,positioni,positionj,iplusgrand,jplusgrand
real :: maxtempo,moyenne,total,plusgrand
real,allocatable :: Ax(:), sigmaAx(:), Bx(:), sigmaBx(:)
real,allocatable :: Ay(:), sigmaAy(:), By(:), sigmaBy(:)
real,allocatable :: Az(:), sigmaAz(:), Bz(:), sigmaBz(:)
real,allocatable :: Ecart(:,:)

nh=2 !trouver une valeur
total=0.

allocate(Ax(nh))
allocate(sigmaAx(nh))
allocate(Bx(nh))
allocate(sigmaBx(nh))
allocate(Ay(nh))
allocate(sigmaAy(nh))
allocate(By(nh))
allocate(sigmaBy(nh))
allocate(Az(nh))
allocate(sigmaAz(nh))
allocate(Bz(nh))
allocate(sigmaBz(nh))
allocate(Ecart(4,nh))

open (unit=1,file='fichierx',status="old") !changer le nom du fichier 

do i=1, nh
	read(1,*) Ax(i)
	read(1,*) sigmaAx(i)
	read(1,*) Bx(i)
	read(1,*) sigmaBx(i)
end do
 close (unit=1)

open (unit=2,file='fichiery',status="old") !changer le nom du fichier 

do i=1, nh
	read(2,*) Ay(i)
	read(2,*) sigmaAy(i)
	read(2,*) By(i)
	read(2,*) sigmaBy(i)
end do
 close (unit=2)

open (unit=3,file='fichierz',status="old") !changer le nom du fichier 

do i=1, nh
	read(3,*) Az(i)
	read(3,*) sigmaAz(i)
	read(3,*) Bz(i)
	read(3,*) sigmaBz(i)
end do
 close (unit=3)


! Debut des comparaisons


do i=1,nh
  Ecart(1,i) = (((Ay(i)-Ax(i))**2. + (Az(i)-Ax(i))**2.))/Ax(i)
  Ecart(2,i) = (((sigmaAy(i)-sigmaAx(i))**2. + (sigmaAz(i)-sigmaAx(i))**2.))/sigmaAx(i)
  Ecart(3,i) = (((By(i)-Bx(i))**2. + (Bz(i)-Bx(i))**2.))/Bx(i)
  Ecart(4,i) = (((sigmaBy(i)-sigmaBx(i))**2. + (sigmaBz(i)-sigmaBx(i))**2.))/sigmaBx(i)
end do

do i = 1, 4
	do j = 1, nh
		total=total+(Ecart(i,j))
 	end do
end do
moyenne = total/(4.*real(nh)) !trouver un moyen pour que la racine represente reellement un pourcentage (pas le cas ici car ecart a la 2)



! if moyenne .ge. 0.05

if (moyenne.ge.0.05) then


!ponderer les ecarts , lecart en z doit etre le moins grand possible 

!trouver le plus grand ecart
plusgrand=0.
do i = 1, 4
	do j = 1, nh
		if (Ecart(i,j) .ge. plusgrand) then
		plusgrand=Ecart(i,j)
		iplusgrand=i
		jplusgrand=j
		end if
 	end do
end do

!modifier la racine correspondante

! call programmes 2d-->3d --> 3*2d en modifiant le racine(n) de sigma(A) et sigma(lambda)
end if


! else
! on est content, yay!


print*, moyenne
print*, plusgrand
print*, iplusgrand
print*, jplusgrand

return
end

