c programme pour trouver les coefficients de Fourier d une ligne ou colonne
c graphycs
      subroutine fourier(datai,ndat,nh,ListeG,a0)
c declaration des variables
      integer ndata,i,n,j,g,z,ndat
      real datai(3000),datao(3000),an(2000),bn(2000),L,lambda(2000)
      real f(3000),pi, bnres(2000), anres(2000), a0, maxx, somme, rms
      real Liste(2000), Liste2(2000), ListeF(2000), ListeG(100,3)
      integer nha,nhb,fcn, typfonction(2000),nh
c      character
c initialisation des variables  

      do i=(ndat+1),2*ndat
        datai(i)=datai(i-ndat)

      enddo
      ndata=ndat*2
c      do i=2,ndata
c        if (abs(datai(i)-datai(i-1)).ge.data(i)) then 
c          datai(i)=datai(i-1)
c        endif 
c      enddo
      do i=1,100
       do j=1,3
         ListeG(i,j)=0.
        enddo
      enddo
      do i=1,2000
         an(i)=0.
         bn(i)=0.
         Liste(i)=0.
         typfonction(i)=0
         
      enddo
      do i=1,3000
         datao(i)=0.
      enddo
      a0=0.
      L=real(ndata)/2.-1.
      pi=3.14159
      maxx=0.
      j=0.
      g=0.
      Som=0.
      Moy=0.




c main
      
      nha=1
      nhb=1
      do i=1, ndata
       a0=a0+1./(2.*L)*datai(i)
      enddo
      do n=1,ndata/4
         lambda(n)=2.*L/real(n)
         do i=1,ndata
            an(n)=an(n)+2./(2.*L)*datai(i)*cos(real(n)*real(i-1)
     +      *pi/L)
            bn(n)=bn(n)+2./(2.*L)*datai(i)*sin(real(n)*real(i-1)
     +      *pi/L)
         enddo
      enddo
      do n=1,ndata/4
      enddo
      z=0
      do n=1,ndata/4
        
            fcn=1                                 !sin
            z=z+1
            Liste(z)=(bn(n))
            typfonction(z)=fcn
        
         
            fcn=0                                 !cos
            z=z+1
            Liste(z)=(an(n))
            typfonction(z)=fcn

        
      enddo

       do i=1,ndata/4
         Liste2(i)=abs(Liste(i))
       enddo
    

      call tribubble(Liste2,ndata/2)

      
      do i=1,nh
      ListeF(i)= Liste2(i)
      enddo
     


    
      g=0
      do ii=1,nh
        
         do jj=1,ndata/4
            if (abs(abs(ListeF(ii))-abs(an(jj))).le.1.e-10) then
            ListeG(ii,1)=an(jj)
            ListeG(ii,2)=lambda(jj)
            ListeG(ii,3)=0.

            endif
            if (abs(abs(ListeF(ii))-abs(bn(jj))).le.1.e-10) then
            ListeG(ii,1)=bn(jj)
            ListeG(ii,2)=lambda(jj)
            ListeG(ii,3)=1.

            endif
            

       
          enddo
      enddo
      do i=1,nh 
         
         
      enddo
      open (unit=1,file='res.txt',status='unknown')
      open (unit=2,file='inp.txt',status='unknown')

      do i=1,ndata
      f(i)=a0
        do n=1,nh
            if (ListeG(n,3).eq. 1.) then
            f(i)=f(i)+ListeG(n,1)*sin(real(i-1)*2.*pi/ListeG(n,2)
     +      )
            endif
     
            if (ListeG(n,3).eq. 0.) then
            f(i)=f(i)+ListeG(n,1)*cos(real(i-1)*2.*pi/ListeG(n,2)
     +      )
      
            endif
        enddo
       
         write(1,*) i,f(i)
         write(2,*) i,datai(i)
      enddo  
      close(unit=2)
      close(unit=1)  
c    Calcul du RMS
c    ceci represente l'erreur entre la somme des fonctions de fourier et les donnees
      somme=0
      rms=0
      do i=1,ndata
         somme= somme+(f(i)-datai(i))*(f(i)-datai(i))
      enddo
      rms=sqrt(somme)    

     
        
      return
      end

      

      
