c	Nombre de pixel : 65535
c	circonférence : 907 pixel
c	rayon=144?
c       teta=angle
c       xc, yc = coordonnées du centre de la nébuleuse
c       xrt, yrt = coordonnées des points le long de la ligne tirée
c       xr, yr = coordonnées des points après la pondération
c       xcirc, ycirc= position du centre du cercle (étoile ionisante)

c	ce programe sert a extraire des lignes de meme longueur passant par le centre de limage

       subroutine extractrayon (lin,ligne, don)
	
	

       real teta,xc,yc,xrt,yrt,densitp(900,900),xcirc,ycirc,xr,yr
       real ligne(900,900),i,j,ngl,PI,ray
       integer nbx,nby,R,k,d,x,y,w,don
       character*12 nom
       character*60 filename
       filename='densitp.pgm'
       nom='electrondens'
       open(unit=1,file='rond.in',status='unknown')
       open(unit=2,file='valray.txt',status='unknown')
       read(1,*) xcirc,ycirc
       read(1,*) xr,yr
       open(unit=3,file='geometry.tmp',status='unknown')  
          read(3,*) nbx, nby
       PI=3.14159253589793
	ycirc=nby-ycirc
        yr=nby-yr
	
	print*,'yeah'

       call intrants2d(filename,densitp,nom,toto,toto,toto,nbx,nby)
       ray=sqrt((xr-xcirc)**2.+(yr-ycirc)**2.)
       ray=ray-1
c     -1 ajoute pour eviter dobtenir des zeros dans valray

       teta=1./ray
       ngl=0.
       w=1

       do while (ngl.lt.PI)
		d=0
	  	do R=-int(ray),int(ray)
                       
			d=d+1
			i=sin(ngl)*real(R)+ycirc
	  		j=cos(ngl)*real(R)+xcirc
			x=nint(j)
			y=nint(i)
			ligne(d,w)=densitp(x,y)
		enddo


		write(2,*) (ligne(k,w),k=1,d)
		w=w+1
		ngl=ngl+teta
       enddo
       don=d
       lin=w-1

       return
       stop
       end
	
