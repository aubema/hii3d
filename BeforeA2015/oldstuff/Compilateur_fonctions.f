      PROGRAM Compilateur_fonctions
c programme pour fusioner des fcts sin et cos de fourier en une fct sin
      real ListeG(100,3),C, phi, A, B, lo,PI,listeGsin(100,3)
      real listefinale(100,3)
      integer k,kk,j,deuxf,n
      PI=3.14159
      open(unit=1,file='ListeG',status='unknown')
c      open(unit=2,file='ListeGsin',status='unknown')
         do k=1,100
            read(1,*) ListeG(k,1),ListeG(k,2),ListeG(k,3)
         enddo
      close(unit=1)
      do k=1,100
         deuxf=0
         do kk=1,100
            if (ListeG(k,2).eq.ListeG(kk,2))  then
               if (kk.ne.k) then
                  deuxf=1
c cas de deux fonctions avec la même amplitude
                  if (ListeG(k,3).eq.1) then
                       A=ListeG(k,1)
                  else 
                       B=ListeG(k,1)
                  endif
                  if (ListeG(kk,3).eq.1) then
                       A=ListeG(kk,1)
                  else 
                       B=ListeG(kk,1)
                  endif
                  C=sqrt(A**2.+B**2.)
                  if (A.gt.0.) then
                     phi=atan(B/A)
                  else 
                     phi=atan(B/A)+PI
                  endif
                  print*,'fct double',k,kk
               endif
            endif
         enddo
         if (deuxf.eq.0) then
            print*,'fct simple',k
            if (ListeG(k,3).eq.1) then
c cas d'une fonction sin
               C=ListeG(k,1)
               phi=0.
            else           
c cas d'une fonction cos
               C=ListeG(k,1)
               phi= PI/2.
            endif
            if (C.lt.0.) then
               C=-C
               phi=phi+PI
               if (phi.ge.2.*PI) then
                  phi=phi-2.*PI
               endif
            endif
         endif
         lo=ListeG(k,2)
         listeGsin(k,1)=C
         listeGsin(k,2)=lo
         listeGsin(k,3)=phi
         print*,listeGsin(k,1),listeGsin(k,2),listeGsin(k,3)
      enddo
      n=0
      do k=1,100
         do kk=1,100
         if (kk.ne.k) then
            if (listeGsin(k,2).ne.0.) then
               if (listeGsin(k,2).eq.listeGsin(kk,2)) then
               listeGsin(kk,2)=0.
               endif   
            endif
         endif
         enddo
         if (listeGsin(k,2).ne.0.) then
               n=n+1
               listefinale(n,1)=listeGsin(k,1)
               listefinale(n,2)=listeGsin(k,2)
               listefinale(n,3)=listeGsin(k,3)
         endif
      enddo

      do k=1,n
         print*,k,listefinale(k,1),listefinale(k,2),listefinale(k,3)
      enddo
       open(unit=3,file='listefinale',status='unknown')
        do k=1,n
           write(3,*) listefinale(k,1),listefinale(k,2),listefinale(k,3)  
        enddo  
      stop
      end
