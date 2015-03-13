      integer ndata,n,j,g,z,i,nbx,nbline,nbint,q,nhf,nhmin
c nhf= nombre d'harmonique finale
      real datai(3000),datao(3000),an(2000),bn(2000),L,lambda(2000)
      real f(3000),pi, bnres(2000), anres(2000), a0, maxx, somme, rms
      real Liste(2000), Liste2(2000), ListeF(2000), parfour(100,3)
      real vecteur(3000), valeur(900,900),somA,somL,ecA,ecL
      real ligne(900,900), parfourF(100,3),const_a0(2000)
      real toto, tabfour(100,3,2000),somD,stats(100,8)
      real distAmpli(100,100,100),distDepha(100,100,100)
      real distLong(100,100,100)
      real intervalle, deltaintA,moyenne(200),valtot,moytot
      real deltaintD,deltaintL
      real moygrap, ecarAmpli,pourEca
      real SomA2, SomA1, PosA1
      integer nha,nhb,Ta(2000),Tb(2000), fcn, typfonction(2000),nh,k,u
      integer harm,nbrSomA
      character*12 nom
      character*60 imagefile
      character*2 flagA
      character*6 fmt
      character*7 fileA,fileD,fileL
c===============================
c Variable a determiner
       pourEca=0.5
       nhmin=20
       pi=3.14159
c=============================      
      
c  nombre d'harmonique finale desire
c  les nhmin plus grande vont etre conserver
c initialisation des variables  
      toto=1.
      nom='temperature'
      imagefile='temperaturem.pgm'
      nh=50
      nbint=1300
      somD=0.
c      call intrants2d(imagefile,valeur,			enleve le 13 fevrier
c     + nom,toto,toto,toto,nbx,nby)
c      print*, valeur
      call extractrayon (nbline,ligne,nbx)
      do i=1, nbline
         do q=1, nbx
         vecteur(q)=ligne(q,i)
         enddo
c	calcul les nh plus grande amplitude et longueur d'onde associe de fourier pour la ligne 
c	et tri par ordre d'amplitude decroissante
c        print*,'nbr de donne',nbx
         call fourier(vecteur,nbx,nh,parfour,a0)
c	call la subroutine qui transforme les fonctions en dephasage, resultat: parfour(k,3) deviens le dephasage



 	call dephasage(parfour,nh,nhf,parfourF)



         do j=1,3
           do k=1,nh
             tabfour(k,j,i)=parfourF(k,j)
c	tabfour: k=harmonique j=1(amplitude),2(longueru d'onde),3(dephasage), i=chaque ligne analyse
           enddo
         enddo
         const_a0(i)=a0
c const_a0 est la valeur de a0 pour la ligne i
      enddo	
      						
      do i=1,nbline
	do j=1,3
	   do k=1,nh
	     if (k.gt.nhmin) then
	       tabfour(k,j,i)=0.
	     endif
	   enddo
	enddo
      enddo

      do j=1,nh
        somD=0.
        somA=0.
        somL=0.
        ecL=0.
        ecA=0.
        do i=1,nbline
          somA=somA+tabfour(j,1,i)
c	somA=somme des amplitude
          somD=somD+tabfour(j,3,i)
c	somD=somme des dephasage
          somL=somL+tabfour(j,2,i)
c	somL=somme des longueurs d'onde
	  
          
        enddo
c	stats:    j=harmonique
c                 (j,1)=moyenne d'amplitude
c		  (j,2)=ecart type des amplitude
c		  (j,3)=moyenne des longueurs d'onde - cette valeur ne sera pas utilisee car les statistiques sont non gaussiennes
c		  (j,4)=ecart type des longueurs d'onde  - cette valeur ne sera pas utilisee car les statistiques sont non gaussiennes
c		  (j,5)=moyenne de dephasage
c                 (j,6)=ecart type des dephasages
c                 (j,7)= moyenne de a0
c                 (j,8)=ecart type de a0
        stats(j,5)= somD/real(nbline)
        stats(j,1)= somA/real(nbline)
        stats(j,3)= somL/real(nbline)
c =====================
c calcul des ecarts type
c
	do i=1,nbline
	  ecA=ecA + (tabfour(j,1,i)-stats(j,1))**2.
c	ecA=somme des amplitudes-la moyenne au carre
	  ecL=ecL+(tabfour(j,2,i)-stats(j,3))**2.
c	ecL=somme des longueurs donde-la moyenne au carre
	enddo
        stats(j,2)= sqrt((1./real(nbline))*ecA)
        stats(j,4)= sqrt((1./real(nbline))*ecL)
c        print*,j,stats(j,1),stats(j,2),stats(j,3),
c     +         stats(j,4),stats(j,5)
      enddo
c=========================================

      open (unit=1,file='amplitude.txt',status='unknown')
      open (unit=2,file='lambda.txt',status='unknown')

c  Pour faire le graphique 3D le dossier est 'tableaustat(harmonique).txt'
c  tabfour(harmonique,*,*) 
      open (unit=3,file='tableaustat1.txt',status='unknown')
      open (unit=4,file='tableaustat2.txt',status='unknown')
      open (unit=5,file='tableaustat3.txt',status='unknown')
c    Initiatialisation du tableau

      do i=1,nbline
      do k=1,nhmin
         write(1,*) (tabfour(k,1,i))
      enddo
        write(1,*) ' '
      enddo

      do i=1,nbline
      do k=1,nhmin
         write(2,*) (tabfour(k,2,i))
      enddo
        write(2,*) ' '
      enddo

c tableaustat.txt comprend les amplitudes, les longuer donde et les dephasage
      do k=3,5
      do i=1,nbline
         write(k,*) (i),(tabfour((k-2),1,i)),
     +  (tabfour((k-2),2,i)),
     +  (tabfour((k-2),3,i))
      enddo


c dist----(1,*)= interval  dist----(2,*)= nombre de valeur dans linterval
      do u=1,100
        distAmpli(k,2,u)=0.
        distAmpli(k,1,u)=0.
        distDepha(k,2,u)=0.
        distDepha(k,1,u)=0.
        distLong(k,2,u)=0.
        distLong(k,1,u)=0.
      enddo
      enddo


c =========================
c   Pour distribution 
c   Cette section est la derniere etape avant de faire fitter des gaussienne



c   Pour amplitude

      do k=1,nhmin
      intervalle=6.*stats(k,2)
         deltaintA=intervalle/50.  

      do i=1,nbline
          do u=1,50
            if ((tabfour(k,1,i).gt.(real(u-1)*deltaintA
     +  +(stats(k,1)-3.*stats(k,2)))).and.(tabfour(k,1,i).le.real(u)
     + *deltaintA+(stats(k,1)-3.*stats(k,2))))
     +  then
            distAmpli(k,2,u)=distAmpli(k,2,u)+1
            endif
          enddo
        enddo
c la prochaine boucle sert a remplacer le numerau de 
c  linterval par la valeur du milieu de l'interval
         do u=1,50
            distAmpli(k,1,u)=stats(k,1)-3.*stats(k,2)+real(u-1)*
     +      deltaintA+deltaintA/2.
         enddo

c   Pour le decalage
       deltaintD=2.*pi/63.
       do i=1,nbline
          do u=1,63
            if ((tabfour(k,3,i)).gt.(real(u-1)*deltaintD).and.
     +         (tabfour(k,3,i).le.real(u)*deltaintD))
     +  then
            distDepha(k,2,u)=distDepha(k,2,u)+1
             endif
           enddo
        enddo
         do u=1,63
            distDepha(k,1,u)=(real(u-1)*deltaintD+deltaintD/2.)
         enddo

c   Pour la longueur d'onde
       do i=1,nbline
          do u=1,50
            if ((abs((tabfour(k,2,i))-(real(nbx-1)/real(u))).le.0.0001))
     +       then 
             distLong(k,2,u)=distLong(k,2,u)+1
             endif
           enddo
         enddo
        do u=1,50
           distLong(k,1,u)=real(nbx-1)/real(u)
        enddo
        fmt = '(I2.2)'
        write (flagA,fmt) k
c        print*,trim(flagA)
        fileA='DistA'//trim(flagA)
        fileL='DistL'//trim(flagA)
        fileD='DistD'//trim(flagA)
         open(unit=9,file=fileD,status='unknown')
         do u=1,63
            write(9,*) distDepha(k,1,u),distDepha(k,2,u)
         enddo
        close(unit=9)
        open(unit=9,file=fileA,status='unknown')
         do u=1,50
            write(9,*) distAmpli(k,1,u),distAmpli(k,2,u)
         enddo
        close(unit=9)
        open(unit=9,file=fileL,status='unknown')
         do u=1,50
            write(9,*) distLong(k,1,u),distLong(k,2,u)
         enddo
        close(unit=9)
        enddo   





c       do u=1,50
c         print*,distAmpli(1,2,u)
c       enddo

c       do u=1,63
c          print*,distDepha(1,2,u)
c       enddo
c=====================================

c      calcul de la moyenne du graphique
c       valtot=0.
c      total des valeurs
c       moytot=0.
c       do u=1,200
c          moyenne(u)=0.
c       enddo
c      distAmpli
c       do u=1,63
c         moyenne(u)=u*distAmpli(1,2,u)
c         valtot=valtot+distAmpli(1,2,u)
c         moytot=moytot+moyenne(u)
         
c       enddo
c       moygrap=(moytot/valtot)
c       print*,moygrap


      
c========================================
c   ecart type pour graphique

c      Amplitude
c       ecA=0.
c       do i=1,50
c       ecA=ecA + (distAmpli(1,2,i)-moygrap)**2.
c       enddo
c       ecarAmpli= sqrt((1./(50))*ecA)


c==========================================
c   Sommets pour graphique

c      Amplitude
c        Nous trouvons la plus haute valeur pour notre premier sommet
c       nbrSomA=1
c       SomA1=0.
c       PosA1=0.
c       do i=1,50
c          if ((distAmpli(1,2,i)).gt.(SomA1))
c     +     then 
c               SomA1=distAmpli(1,2,i)
c               PosA1=i
c          endif
c       enddo
c     Avec les nouvelles images, nous concluons qu'il n'a pas de deuxieme sommet
c       SomA2=0.
c       PosA2=0.
c       do i=1,50
c          if ((distAmpli(2,i)).gt.(pourcEca*SomA1).and.
c     +    (i).lt.(PosA1-(1*ecarAmpli)).or.
c     +    (i).gt.(PosA1+(1*ecarAmpli)))
c     +    then 
c               SomA2=distAmpli(1,2,i)
c               PosA2=i
c          endif
c       enddo
       
c       if ((SomA2).gt.(0))
c     +    then 
c              nbrSomA=2
c       endif

c      print*, SomA1,PosA1

      close (unit=1)
      close (unit=2)
      close (unit=3)
      close (unit=4)
      close (unit=5)

c	print*,nhmin

      stop
      end
