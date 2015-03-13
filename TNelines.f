c programme TNelines
c    DATAM27!!!!!!!!!!!!!!!!!
c   
c    Copyright (C) 2012  Martin Aube
c
c    This program is free software: you can redistribute it and/or modify
c    it under the terms of the GNU General Public License as published by
c    the Free Software Foundation, either version 3 of the License, or
c    (at your option) any later version.
c
c    This program is distributed in the hope that it will be useful,
c    but WITHOUT ANY WARRANTY; without even the implied warranty of
c    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c    GNU General Public License for more details.
c
c    You should have received a copy of the GNU General Public License
c    along with this program.  If not, see <http://www.gnu.org/licenses/>.
c
c    Contact: martin.aube@cegepsherbrooke.qc.ca
c
c    Ce programme a pour but de transformer les images dintensite de raies de la nebuleuse en images de
c    temperature et de densite delectron. Ces dernieres images seront utilisees pour la deuxieme partie c    
c    du projet qui les transforme en trois dimensions. Les parties du programme mises en commentaires sont
c    des parties que nous avons decide denlever mais quil est toujours possible de rajouter en enlevant les
c    commentaires 
c    Copyright (C) 2013  Alexandre Carbonneau, Catherine Masson (Alexandrine), Maude Roy-Labbe (support moral important) et ELPHES+Guigui (support moral moins important)  
c
c        
        subroutine TNelines(ncols, nlines,tempfinalm,densitm)
        real out(30,900,900),vect(1000000),vmin,vmax,gain,offset
        real valeur(900,900),SIIratio(900,900),raies(900,900)
        real sig2noise
        integer i,j,nlines,ncols,pos,longueur,nfiles,n,ii
        character*20 namef(30)
        character*40 outfile
        character*12 nom
        real xcell0,ycell0,pixsiz,NIIratio(900,900),aptmp,dens
        real densit(900,900),tempfinal(900,900),somme,pix,add
        real tempfinalp(900,900)
        real distt,distv,densitp(900,900)
        integer nbx,nby,valmax,SII6716,SII6731,SIIBx,SIIBy
        integer NII6584,NII5755,NIIBx,NIIBy,cor, box
        real nor,cm, SIIratiom(900,900),NIIratiom(900,900)
        integer nx,ny,xcirc,ycirc
c define the signal to noise ratio
        sig2noise=2.
c define the interpolation radius
        box=15
        open(unit=1,file='geometry.tmp',status='unknown')
           read(1,*) ncols, nlines
           read(1,*) nfiles
           do n=1,nfiles
              read(1,*) namef(n)
           enddo
	close(unit=1)
        do n=1,nfiles
        print*,namef(n)
          open(unit=2,file=namef(n),status='unknown')
c             longueur=(ncols-1)+(ncols+1)*nlines
            longueur=ncols*nlines+ncols+nlines
c           formule changée 24 oct. enlève le décalage
            read (2,*)
             read(2,*) (vect(ii),ii=1,longueur)
             

          close(unit=2)
          vmin=1000000000.
          vmax=0.
          do i=1,ncols
             do j=1,nlines
                pos=i+(ncols+1)*j
                out(n,i,j)=vect(pos)
                valeur(i,j)=vect(pos)
                if (vect(pos).lt.vmin) then
                  vmin=vect(pos)
                endif
                if (vect(pos).gt.vmax) then
                  vmax=vect(pos)
                endif
             enddo
           enddo
           

c  .pgm images of the original .fit images
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfile=namef(n)(1:9)//".pgm"
          xcell0=0.
          ycell0=0.
          nom=namef(n)
          pixsiz=1.
         nbx=ncols
          nby=nlines
          valmax=65535
          call extrant2d (outfile,valeur,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)
        enddo        
        do n=1,nfiles
              if (namef(n)(1:9).eq.'SII6716-s') then
                   SII6716=n
              endif
              if (namef(n)(1:9).eq.'SII6731-s') then
                   SII6731=n
              endif
              if (namef(n)(1:9).eq.'NII6584-s') then
                   NII6584=n
              endif
              if (namef(n)(1:9).eq.'NII5755-s') then
                   NII5755=n
              endif
              if (namef(n)(1:9).eq.'SII6716-b') then
                   SIIBx=n
              endif
              if (namef(n)(1:9).eq.'SII6731-b') then
                   SIIBy=n
              endif
              if (namef(n)(1:9).eq.'NII6584-b') then
                   NIIBx=n
              endif
              if (namef(n)(1:9).eq.'NII5755-b') then
                   NIIBy=n
              endif
              
c si on perd les images, on reutiliser celles du dossier newimage
c en multipliant _int et _sigma selon la formule suivante
c imarith SII6716_int.fit SII6716_sigma.fit mul SII6716-s.fit

c Ha__6563 est un test, il faut le remplacer par 5755.
           enddo
c Processus pour enleve le bruit des images de raies originales
        do i=1, ncols
          do j=1, nlines
            if (out(SIIBx,i,j).lt.sig2noise) then
              out(SII6716,i,j)=0.
            endif
          enddo
        enddo
        do i=1, ncols
          do j=1, nlines
            if (out(SIIBy,i,j).lt.sig2noise) then
              out(SII6731,i,j)=0.
            endif
          enddo
        enddo
        do i=1, ncols
          do j=1, nlines
            if (out(NIIBx,i,j).lt.sig2noise) then
              out(NII6584,i,j)=0.
            endif
          enddo
        enddo
        do i=1, ncols
          do j=1, nlines
            if (out(NIIBy,i,j).lt.sig2noise) then
              out(NII5755,i,j)=0.
            endif
          enddo
        enddo

c==============================================================================
c Faire les ratios de SII et NII         
c SIIratio
          do i=1,ncols
             do j=1,nlines
                if (out(SII6731,i,j).eq.0.) then
                  SIIratio(i,j)=0.
                else 
                SIIratio(i,j)=out(SII6716,i,j)/out(SII6731,i,j)
                  if (SIIratio(i,j).lt.0.2) then
                    SIIratio(i,j)=0.
                  else if (SIIratio(i,j).gt.2.) then
                    SIIratio(i,j)=0.
                  endif
                endif
             enddo
          enddo
c NIIratio (corriger le Ha)
          do i=1,ncols
           do j=1,nlines
             if (out(NII5755,i,j).eq.0.) then
               NIIratio(i,j)=0.
c corriger le 1000/3 a 4/3
             else  
            NIIratio(i,j)=(4.*out(NII6584,i,j))/(3.*out(NII5755,i,j))      
                  if (NIIratio(i,j).lt.0.) then
                    NIIratio(i,j)=0.
                  else if (NIIratio(i,j).gt.300.) then
                    NIIratio(i,j)=0.
                  endif
              endif
           enddo
          enddo

c===================================================================================
c Processus de calcul pour calculer la densite et la temperature 	
c La densite electronique
         do i=1,ncols
             do j=1,nlines
             aptmp=8900.
             dens=0.
             if ((NIIratio(i,j).ne.0.) .and. (SIIratio(i,j).ne.0.)) then
c Si la valeur converge pas augmente le k
               somme=0.
               do k=1,10
                 call intersii (dens, SIIratio(i,j),aptmp)
                 densit(i,j)=dens
                 call temperatureNII(NIIratio(i,j),dens,aptmp)
                 tempfinal(i,j)=aptmp
                 if (aptmp+dens.eq.somme) then 
                 goto 200
                 endif
                 somme=aptmp+dens
               enddo
c Si le ratio est nul, les temperature et la densite ne sont pas consideres
             else
               densit(i,j)=0.
               tempfinal(i,j)=0.
 200         endif
             enddo
         enddo
         do i=1,ncols
             do j=1,nlines
               if (densit(i,j).gt.19000.) then
                  densit(i,j)=0.
               endif
             enddo
          enddo

c================================================================================
c trie pixel temperature
       do i=2, ncols-2
         do j=2, nlines-2
           pix=0
           if (tempfinal(i,j).ne.0.) then
            do n=i-1, i+1
               do m=j-1, j+1                           
                 if (tempfinal(n,m).ne.0.) then
                   pix=pix+1.
                 endif
               enddo
             enddo
           endif
           if (pix.lt.3.) then
             tempfinal(i,j)=0.
           endif
        enddo
       enddo
c trie pixel densite
       do i=2, ncols-2
         do j=2, nlines-2
           pix=0
           if (densit(i,j).ne.0.) then
            do n=i-1, i+1
               do m=j-1, j+1                           
                 if (densit(n,m).ne.0.) then
                   pix=pix+1.
                 endif
               enddo
             enddo
           endif
           if (pix.lt.3.) then
             densit(i,j)=0.
           endif
        enddo
       enddo
c===============================================================================
c les images de densite et temperature en utilisant la ponderation
c lissage de limage temperature par ponderation
       do i=box, ncols-box
         do j=box, nlines-box
           add=0.
           distt=0.          
           if (tempfinal(i,j).eq.0.) then
            do n=i-(box-1), i+(box-1)
               do m=j-(box-1), j+(box-1)
                 distv=0.                            
                   distv=sqrt((abs(i-n))**2.+(abs(j-m))**2.)
                   if (distv.eq.0.) then
                     distv=10.
                   endif
                   if (distv.lt.real(box)) then
                     if (tempfinal(n,m).ne.0.) then
                       distt=distt+1./distv
                       add=add+tempfinal(n,m)/(distv)
                     endif
                   endif
               enddo
             enddo
             tempfinalp(i,j)=(add/distt)
             else
             tempfinalp(i,j)=tempfinal(i,j)
           endif
        enddo
       enddo
        print*,'etape 1'
	call rond (nbx,nby,tempfinalp)
        print*,'etape 2'
c lissage de limage de densite par ponderation
       do i=box, ncols-box
         do j=box, nlines-box
c        do i=1, ncols
c          do j=1, nlines
           add=0.
           distt=0.
           if (densit(i,j).eq.0.) then
            do n=i-(box-1), i+(box-1)
               do m=j-(box-1), j+(box-1)
                 distv=0.   
c             do n=1, ncols
c               do m=1, nlines                          
                   distv=sqrt((abs(i-n))**2.+(abs(j-m))**2.)
                   if (distv.eq.0.) then
                     distv=10.
                   endif
                   if (distv.lt.real(box)) then
                     if (densit(n,m).ne.0.) then
                       distt=distt+1./distv
                       add=add+densit(n,m)/(distv)
                     endif
                 endif
               enddo
             enddo
             densitp(i,j)=(add/distt)
             else
             densitp(i,j)=densit(i,j)
           endif
        enddo
       enddo
       print*,'etape 3'
       call rond (nbx,nby,densitp)
       print*,'etape 4'
c===================================================================================================
c Cette section est dediee a imprimer toutes les images
c Print image ratio SII
          vmin=1000000000.
          vmax=0.
          do i=1,ncols
             do j=1,nlines
                if (SIIratio(i,j).lt.vmin) then
                  vmin=SIIratio(i,j)
                endif
                if (SIIratio(i,j).gt.vmax) then
                  vmax=SIIratio(i,j)
                endif
             enddo
          enddo        
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfile="SIIratio.pgm"
          xcell0=0.
          ycell0=0.
          nom="SIIratio"
          pixsiz=1.
          nbx=ncols
          nby=nlines
          valmax=65535
          print*, vmin,vmax,gain
          call extrant2d (outfile,SIIratio,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)
c print NIIratio
          vmin=1000000000.
          vmax=0.
          do i=1,ncols
             do j=1,nlines
                if (NIIratio(i,j).lt.vmin) then
                  vmin=NIIratio(i,j)
                endif
                if (NIIratio(i,j).gt.vmax) then
                  vmax=NIIratio(i,j)
                endif
             enddo
          enddo         
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfile="NIIratio.pgm"
          xcell0=0.
          ycell0=0.
         nom="NIIratio"
          pixsiz=1.
          nbx=ncols
          nby=nlines
          valmax=65535
          print*, vmin,vmax,gain
          call extrant2d (outfile,NIIratio,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)
          vmin=1000000000.
          vmax=0.
c Print image temperature pond
          vmin=1000000000.
          vmax=0.
          do i=1,ncols
             do j=1,nlines
                if (tempfinalp(i,j).lt.vmin) then
                  vmin=tempfinalp(i,j)
                endif
                if (tempfinalp(i,j).gt.vmax) then
                  vmax=tempfinalp(i,j)
               endif
             enddo
          enddo        
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfile="temperaturep.pgm"
          xcell0=0.
          ycell0=0.
          nom="temperaturep"
          pixsiz=1.
          nbx=ncols
          nby=nlines
          valmax=65535
          print*, vmin,vmax,gain
          call extrant2d (outfile,tempfinalp,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)  
c Print image densite pondere          
          vmin=1000000000.
          vmax=0.           
           do i=1,ncols
             do j=1,nlines
                if (densitp(i,j).lt.vmin) then
                  vmin=densitp(i,j)
                endif
                if (densitp(i,j).gt.vmax) then
                  vmax=densitp(i,j)
                endif
             enddo
          enddo
          gain=(vmax-vmin)/65535.
          offset=vmin
         outfile="densitp.pgm"
          xcell0=0.
          ycell0=0.
          nom="densitp"
          pixsiz=1.
          nbx=ncols
          nby=nlines
          valmax=65535
          print*, vmin,vmax,gain
          call extrant2d (outfile,densitp,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)
 
c        reduction de limage avec le centre de masse, pour que limage soit plus petite pour pouvoir lanalyser avec nos ordinateurs. 
c        Est a travailler, nest pas termine
c         centre=0.
c         allo=0.
c         do i=1,nlines
c           do j=1,ncols
c             nor=0.
c             cm=0.
c             if (ncols.ge.nlines) then
c               cm=tempfinalm(i,j)*j+cm
c               nor=tempfinalm(i,j)+nor
c             else
c               cm=tempfinalm(i,j)*i+cm
c               nor=tempfinalm(i,j)+nor
c             endif
c             centre=cm/nor
c             allo=allo+centre
c           enddo
c         enddo
c         if (ncols.ge.nlines) then
c           allo=allo/nlines
c         else
c           allo=allo/ncols
c         endif
c         print*,cm
        return
	end 
