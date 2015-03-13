      real donnesgaus(50,3,500), c, x,z1,parline(50,3),z2,a,b
      real e,d,pi,dtheta,theta,phi
        integer y, i,k,l,m,j,nh,ndonnes,ndonneso,ntheta,n3d
c random.f est le program qui fait le tire aleatoire parmis les satistiques recu, il prend commen entre le ficher cree par donnesgaus4.f
       pi=3.14159
       dtheta=1./180.*pi
       ntheta=nint(pi/2./dtheta)
c       print*,'ntheta',ntheta,dtheta
        print*,c
        open(unit=1,file='donnesgaus',status='unknown')
c          read(1,*) nh,ndonnes
	   nh=4.
           ndonnes=499.
c	  pour le tester nous avons atribuer ces valeurs
          do k=1,nh
            do l=1,3
                 read(1,*) (donnesgaus(k,l,m),m=1,ndonnes)
c le parametre k est le numéro de l'harmonique
c le parametre l=1 est l'amplitude, l=2 est la longueur d'onde et l=3 est le dephasage
c le parametre m est la position dans le tableau dans une ligne de donnes
            enddo
          enddo
        close(unit=1)
        c=real(ndonnes)
c Sortir toutes les lignes pour couvrir la sphere a angle solide constant
        theta=-dtheta
        m=0
        open(unit=2,file='randomlines',status='unknown')
        do i=1,ntheta
           theta=theta+dtheta
           phi=-dtheta
           do while (phi.lt.pi)
             phi=phi+dtheta/cos(theta)
c Assigne une direction angulaire (phi et theta) aux lignes de donnes (couvre toute la sphere)
             n3d=n3d+1
             do k=1,nh
                   x=rand()
                   y=int(x*c+1.)
                parline(k,1)=donnesgaus(k,1,y)
                   x=rand()
                   y=int(x*c+1.)
                parline(k,2)=donnesgaus(k,2,y)
                   x=rand()
                   y=int(x*c+1.)
                parline(k,3)=donnesgaus(k,3,y)
c les lignes de codes precedentes servent a donner une position aleatoire aux donnes l=1 l=2, l=3
             enddo
c                print*,parline(2,3)

        write(2,*) theta,phi, 'theta, phi'
        do l=1,3 
           write(2,*) (parline(k,l),k=1,nh)
        enddo
 
           enddo
         enddo
         close(unit=2)
        end
