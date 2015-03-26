c	routine pour determiner la densite electronique
c	a partir de la figure 5.3 (Osterbrock 1989) la 
c	precision de la digitalisation de la courbe est 
c	estimee a .01 sur le rapport d'intensite.  Cette
c	incertitude est consideree pour le calcul de l'erreur.
c
c	dens= densite electronique
c	rapp= rapport de raie
c	aptmp= temperature electronique approximative
c	ratio(*,*)= figure 5.3 digitalisee
c
c
       subroutine intersii (dens, rapp, aptmp)
       dimension ratio(19,2)
       integer ii, n
       real dens, rapp, aptmp, ratio,m,b     
       ratio(1,1)=1.43	
       ratio(2,1)=1.42
       ratio(3,1)=1.41
       ratio(4,1)=1.39
       ratio(5,1)=1.36
       ratio(6,1)=1.32
       ratio(7,1)=1.25
       ratio(8,1)=1.18
       ratio(9,1)=1.10
       ratio(10,1)=1.00
       ratio(11,1)=0.86
       ratio(12,1)=0.70
       ratio(13,1)=0.61
       ratio(14,1)=0.56
       ratio(15,1)=0.52
       ratio(16,1)=0.49
       ratio(17,1)=0.46
       ratio(18,1)=0.451
       ratio(19,1)=0.45
       ratio(1,2)=10.
       ratio(2,2)=20.
       ratio(3,2)=30.
       ratio(4,2)=40.
       ratio(5,2)=50.
       ratio(6,2)=100.
       ratio(7,2)=200.
       ratio(8,2)=300.
       ratio(9,2)=400.
       ratio(10,2)=500.
       ratio(11,2)=1000. 
       ratio(12,2)=2000.
       ratio(13,2)=3000.
       ratio(14,2)=4000.
       ratio(15,2)=5000.
       ratio(16,2)=10000.
       ratio(17,2)=20000.
       ratio(18,2)=30000.
       ratio(19,2)=100000.
       do i=1,19
         ratio(i,2)=ratio(i,2)*sqrt(10000./aptmp)                                    ! correction de l'echelle horizontale pour la temparature du grap. 5.3 de Osterbrock
       enddo
       n=1
       do while (rapp.lt.ratio(n,1))
           n=n+1
           m=(ratio(n-1,1)-ratio(n,1))/(ratio(n-1,2)-ratio(n,2))
           b=ratio(n-1,1)-m*ratio(n-1,2)
           dens=(rapp-b)/m
       enddo
       return
       end      
