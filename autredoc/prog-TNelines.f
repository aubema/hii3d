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
        subroutine TNelines(ncols, nlines,tempp,densit,SIIrat,
     +  NIIrat)
        real out(30,401,401),vect(1000000),vmin,vmax,gain,offset
        real valeur(401,401),SIIrat(401,401),raies(401,401)
        real sig2no
        integer i,j,nlines,ncols,pos,length,nfiles,n,ii
        character*20 namef(30)
        character*40 outfil
        character*12 nom
        real xcell0,ycell0,pixsiz,NIIrat(401,401),aptmp,dens
        real densit(401,401),somme,pix,add
        real tempp(401,401)
        real distt,distv
        integer nbx,nby,valmax,SII6716,SII6731,SIIBx,SIIBy
        integer NII6584,NII5755,NIIBx,NIIBy,cor, box
        real nor,cm, SIIrm(401,401),NIIrm(401,401)
        integer nx,ny,xcirc,ycirc
c define the signal to noise ratio
        sig2no=3.
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
            length=ncols*nlines+ncols+nlines
            read (2,*)
            read(2,*) (vect(ii),ii=1,length)
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
          outfil=namef(n)(1:9)//".pgm"
          xcell0=0.
          ycell0=0.
          nom=namef(n)
          pixsiz=1.
          nbx=ncols
          nby=nlines
          valmax=65535
          call extrant2d (outfil,valeur,nom,xcell0,ycell0,pixsiz,
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
            if (out(SIIBx,i,j).lt.sig2no) then
              out(SII6716,i,j)=0.
            endif
          enddo
        enddo
        do i=1, ncols
          do j=1, nlines
            if (out(SIIBy,i,j).lt.sig2no) then
              out(SII6731,i,j)=0.
            endif
          enddo
        enddo
        do i=1, ncols
          do j=1, nlines
            if (out(NIIBx,i,j).lt.sig2no) then
              out(NII6584,i,j)=0.
            endif
          enddo
        enddo
        do i=1, ncols
          do j=1, nlines
            if (out(NIIBy,i,j).lt.sig2no) then
              out(NII5755,i,j)=0.
            endif
          enddo
        enddo
c
c==============================================================================
c Faire les ratios de SII et NII         
c SIIrat
          do i=1,ncols
             do j=1,nlines
                if (out(SII6731,i,j).eq.0.) then
                  SIIrat(i,j)=0.
                else 
                SIIrat(i,j)=out(SII6716,i,j)/out(SII6731,i,j)
                  if (SIIrat(i,j).lt.0.2) then
                    SIIrat(i,j)=0.
                  else if (SIIrat(i,j).gt.2.) then
                    SIIrat(i,j)=0.
                  endif
                endif
             enddo
          enddo
c NIIrat (corriger le Ha)
          do i=1,ncols
           do j=1,nlines
             if (out(NII5755,i,j).eq.0.) then
               NIIrat(i,j)=0.
c corriger le 1000/3 a 4/3
             else  
            NIIrat(i,j)=(4.*out(NII6584,i,j))/(3.*out(NII5755,i,j))      
                  if (NIIrat(i,j).lt.0.) then
                    NIIrat(i,j)=0.
                  else if (NIIrat(i,j).gt.300.) then
                    NIIrat(i,j)=0.
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
             if ((NIIrat(i,j).ne.0.).and.(SIIrat(i,j).ne.0.)) then
c Si la valeur converge pas augmente le k
               somme=0.
               do k=1,10
                 call intersii (dens, SIIrat(i,j),aptmp)                            ! routine qui retourne la densite si on lui donne temperature et ratio sii
                 densit(i,j)=dens
                 call temperatureNII(NIIrat(i,j),dens,aptmp)                        ! cette routine retourne la temperature si on lui donne la densite et le ratio nii
                 tempp(i,j)=aptmp
               enddo
c Si le ratio est nul, les temperature et la densite ne sont pas consideres
             else
               densit(i,j)=0.
               tempp(i,j)=0.
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
c===================================================================================================
c Cette section est dediee a imprimer toutes les images
c Print image ratio SII
          vmin=1000000000.
          vmax=0.
          do i=1,ncols
             do j=1,nlines
                if (SIIrat(i,j).lt.vmin) then
                  vmin=SIIrat(i,j)
                endif
                if (SIIrat(i,j).gt.vmax) then
                  vmax=SIIrat(i,j)
                endif
             enddo
          enddo        
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfil="SIIrat.pgm"
          xcell0=0.
          ycell0=0.
          nom="SIIrat"
          pixsiz=1.
          nbx=ncols
          nby=nlines
          valmax=65535
          call extrant2d (outfil,SIIrat,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)
c print NIIrat
          vmin=1000000000.
          vmax=0.
          do i=1,ncols
             do j=1,nlines
                if (NIIrat(i,j).lt.vmin) then
                  vmin=NIIrat(i,j)
                endif
                if (NIIrat(i,j).gt.vmax) then
                  vmax=NIIrat(i,j)
                endif
             enddo
          enddo         
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfil="NIIrat.pgm"
          xcell0=0.
          ycell0=0.
         nom="NIIrat"
          pixsiz=1.
          nbx=ncols
          nby=nlines
          valmax=65535
          call extrant2d (outfil,NIIrat,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)
          vmin=1000000000.
          vmax=0.
c Print image temperature pond
          vmin=1000000000.
          vmax=0.
          do i=1,ncols
             do j=1,nlines
                if (tempp(i,j).lt.vmin) then
                  vmin=tempp(i,j)
                endif
                if (tempp(i,j).gt.vmax) then
                  vmax=tempp(i,j)
               endif
             enddo
          enddo        
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfil="temperature.pgm"
          xcell0=0.
          ycell0=0.
          nom="temperature"
          pixsiz=1.
          nbx=ncols
          nby=nlines
          valmax=65535
          call extrant2d (outfil,tempp,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)  
c Print image densite pondere          
          vmin=1000000000.
          vmax=0.           
           do i=1,ncols
             do j=1,nlines
                if (densit(i,j).lt.vmin) then
                  vmin=densit(i,j)
                endif
                if (densit(i,j).gt.vmax) then
                  vmax=densit(i,j)
                endif
             enddo
          enddo
          gain=(vmax-vmin)/65535.
          offset=vmin
         outfil="density.pgm"
          xcell0=0.
          ycell0=0.
          nom="density"
          pixsiz=1.
          nbx=ncols
          nby=nlines
          valmax=65535
          call extrant2d (outfil,densit,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)
        return
	end 
