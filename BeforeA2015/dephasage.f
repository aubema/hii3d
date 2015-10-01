      subroutine dephasage(parfour,nh,n,parfourT)
c programme pour fusioner des fcts sin et cos de fourier en une fct sin
      real parfour(100,3),C, phi, A, B, lo,PI,parfoursin(100,3)
      real parfourF(100,3),liste2(2000),parfourT(100,3)
      integer k,kk,j,deuxf,n,u,v
      PI=3.14159
c      open(unit=1,file='parfour',status='unknown')
c      open(unit=2,file='parfoursin',status='unknown')
c         do k=1,nh
c            read(1,*) parfour(k,1),parfour(k,2),parfour(k,3)
c         enddo
c      close(unit=1)

c parfour contient les valeurs suivantes - ce sont les parametres de fourier
c 1 ampliture
c 2 longueur d'onde
c 3 type de fonction 0= cos  1= sin
      
      do k=1,nh
         deuxf=0
c (deuxf) permet d'etre certain qu'on ne combine pas une fonction avec elle-meme
         do kk=1,nh
            if (parfour(k,2).eq.parfour(kk,2))  then
               if (kk.ne.k) then
                  deuxf=1
c cas de deux fonctions avec la meme amplitude
                  if (parfour(k,3).eq.1) then
                       A=parfour(k,1)
                  else 
                       B=parfour(k,1)
                  endif
                  if (parfour(kk,3).eq.1) then
                       A=parfour(kk,1)
                  else 
                       B=parfour(kk,1)
                  endif
c identite trigo pour combiner un cos et un sin d'amplitude differentes en un sin avec dephasage
                  C=sqrt(A**2.+B**2.)
                  if (A.gt.0.) then
                     phi=atan(B/A)
                  else 
                     phi=atan(B/A)+PI
                  endif
               endif
            endif
         enddo
         if (deuxf.eq.0) then
            if (parfour(k,3).eq.1) then
c cas d'une fonction sin
               C=parfour(k,1)
               phi=0.
            else           
c cas d'une fonction cos
               C=parfour(k,1)
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
         lo=parfour(k,2)
         parfoursin(k,1)=C
         parfoursin(k,2)=lo
         parfoursin(k,3)=phi
      enddo
      n=0
      do k=1,nh
         do kk=1,nh
         if (kk.ne.k) then
            if (parfoursin(k,2).ne.0.) then
               if (parfoursin(k,2).eq.parfoursin(kk,2)) then
               parfoursin(kk,2)=0.
               endif   
            endif
         endif
         enddo
         if (parfoursin(k,2).ne.0.) then
               n=n+1
               parfourF(n,1)=parfoursin(k,1)
               parfourF(n,2)=parfoursin(k,2)
               parfourF(n,3)=parfoursin(k,3)
               if (parfourF(n,3).lt.0.) then 
                  parfourF(n,3)=parfourF(n,3)+2.*PI
               endif
         endif
      enddo


	

       open(unit=3,file='parfourF',status='unknown')
        do k=1,n
           write(3,*) parfourF(k,1),parfourF(k,2),parfourF(k,3)
        enddo

        do u=1,2000
           liste2(u)=0.
  
        enddo
        do u=1,100
           liste2(u)=parfourF(u,1)
           parfourT(u,1)=0.
           parfourT(u,2)=0.
           parfourT(u,3)=0.
        enddo



      call tribubble(liste2,100)

c         
        do u=1,100
           parfourT(u,1)=liste2(u)
           do v=1,100
            if (parfourF(v,1).eq.parfourT(u,1)) then
            parfourT(u,2)=parfourF(v,2)
            parfourT(u,3)=parfourF(v,3)
            endif
           enddo
        enddo
      
      return  
      stop
      end
