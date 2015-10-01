	real  PI, e, ectA, moyA, yA, x1A, x2A
	real xA, interA, demintA, aA, bA, ytotA
	real mA, cA, sA, donnesgaus(50,3,500)
	real xD, yD, D, yL, xL, L 
	real ytotD, ytotL
	integer nxA, nyA, nA, nh, nyD, nxD
	integer n1, n2, n3, nxL, nyL 

c Ce programme entre les donnes statistiques est forme une ligne de valeurs possibles
c chaque valeur peut etre presente plusieurs fois proportionnelement au statistique trouvé précedement
c les statistiques sur les donnes d'amplitude ont une forme gaussienne cest pourquoi la premiere partie est differente des deux autres
c les statistique sur les donnes de longueur donde et de dephasage ne vari pas dune maniere gaussienne 
c les donnes fournient pour lamplitude sont la moyenne et lecart type pour chaque harmonique
c les donnes fournient pour la longueur donde et le dephasage sont chaque valeur x, et le nb de fois quelle apparaisse en y
C les fichier testamp, testlong et testdef sont seulement des fichiers test en model reduit, dans le vrai il va y avoir plus dinterval et plus dharmonique
	PI=3.14159	
	e=2.71828

c	read nh (le nb dharmonique) du programme precedent
	nh=4

        open(unit=1,file='testamp',status='unknown')

	do k=1,nh
        n1=0.
        n2=0.
        n3=0.
c	Amplitude
c	Read moyenne et l'écart type pour chaque amplitude de chaque harmonique (fournis par un autre proramme)
          read(1,*) ectA,moyA
c	 print*,ectA,moyA
c            moyA=2.	
c 	    ectA=3.
	    x1A=moyA+(-3.*ectA)	
	    x2A=moyA+(3.*ectA)
	    interA=abs(x2A-x1A)/51.
	    demintA=interA/2. 
            aA=x1A+demintA
            bA=x2A-demintA
c	print*,inter,demint
	    xA=aA
	    ytotA=0.
            sA=1.
	       do nxA=1,50
		 mA=1./(ectA*((2.*PI)**0.5))
                 cA=-1.*((xA-moyA)**2.)/((2.*ectA)**2.)
		 yA=mA*exp(cA)
	         yA=yA*43.31*ectA
c 4.3 est un chiffre qui sert que le nombre de donnes total d'une soit de 50
	          nyA=nint(yA)
                 ytotA=ytotA+nyA

c ytot est le nombre total de donnees pour une ligne d'amplitude

                 xA=xA+interA
	             do m=1,nyA
                     n1=n1+1
	             donnesgaus(k,1,n1)=xA
	             enddo
                enddo



c	La longueur d'onde
	open(unit=3,file='testlong',status='unknown')
	  ytotL=0.
	   do nxL=1,4
c modfifier 4 pour 50 dans le vrai
            read(3,*) xL,yL
	    nyL=nint(yL)
	    ytotL=ytotL+nyL
	  enddo
        close (3)
        ytotL=ytotL
        L=499./ytotL
        ytotL=ytotL*L
        ytotL=nint(ytotL)
c on fait une premiere boucle sur les donnees pour trouver le nombre de donnes total
c ensuite nous trouvons la constante pour laquelle il faut multiplier nos nombre de donnes pour qu'il y en aille 499, un nb fix
	open(unit=3,file='testlong',status='unknown')
   	yL=0.
	   do nxL=1,4
c modfifier 4 pour 50 dans le vrai
          read(3,*) xL,yL
            yL=yL*L
	    nyL=nint(yL)
            do m=1,nyL
               n2=n2+1
               donnesgaus(k,2,n2)=xL
	     enddo
	  enddo
        close (3) 
c La deuxieme boucle est fait pour sortir les valeurs


c Le dephasage
	open(unit=4,file='testdef',status='unknown')
	  ytotD=0.
	   do nxD=1,4
c modfifier 4 pour 63 dans le vrai
            read(4,*) xD,yD
	    nyD=nint(yD)
	    ytotD=ytotD+nyD
	  enddo
        close (4)
        ytotD=ytotD
        D=499./ytotD
        ytotD=ytotD*D
        ytotD=nint(ytotD)
	 print*,ytotD,D
  
	open(unit=4,file='testdef',status='unknown')
   	yD=0.
	   do nxD=1,4
c modfifier 4 pour 63 dans le vrai
          read(4,*) xD,yD
            yD=yD*D
	    nyD=nint(yD)
            do m=1,nyD
               n3=n3+1
               donnesgaus(k,3,n3)=xD
c             print*,xD,n2
	     enddo
c	   print*,ytotD
	  enddo
        close (4) 

	enddo
c       print*,ytotD
	open(unit=2,file='donnesgaus',status='unknown')
	do k=1,nh
	write(2,*) (donnesgaus(k,1,m),m=1,499)
	write(2,*) (donnesgaus(k,2,m),m=1,499)
	write(2,*) (donnesgaus(k,3,m),m=1,499)
	enddo
	end
