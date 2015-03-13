c programme fit2fortran 
c
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
c        lines2TNe (tempfinalm,densitm, ncols, nlines)
        real out(10,700,1200),vect(1000000),vmin,vmax,gain,offset
        real valeur(700,1200),SIIratio(700,1200),raies(700,1200)
        integer i,j,nlines,ncols,pos,longueur,nfiles,n,ii
        character*20 namef(8)
        character*40 outfile
        character*12 nom
        real xcell0,ycell0,pixsiz,NIIratio(700,1200),aptmp,dens
        real densit(700,1200),tempfinal(700,1200),somme,pix,add
        real tempfinalp(700,1200)
        real distt,distv,densitp(700,1200)
        integer nbx,nby,valmax,SII6716,SII6731,NII6548
        integer NII6584,Ha6563, noise,cor, box
        real nor,cm, SIIratiom(700,1200),NIIratiom(700,1200)
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
             longueur=(ncols-1)+(ncols+1)*nlines
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
          outfile=namef(n)(1:8)//".pgm"
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
              if (namef(n)(1:8).eq.'SII_6716') then
                   SII6716=n
              endif
              if (namef(n)(1:8).eq.'SII_6731') then
                   SII6731=n
              endif
              if (namef(n)(1:8).eq.'NII_6548') then
                   NII6548=n
              endif
              if (namef(n)(1:8).eq.'NII_6584') then
                   NII6584=n
              endif
              if (namef(n)(1:8).eq.'Ha__6563') then
                   Ha6563=n
              endif
              if (namef(n)(1:5).eq.'noise') then
                   noise=n
              endif
c Ha__6563 est un test, il faut le remplacer par 5755.
           enddo
c Processus pour enleve le bruit des images de raies originales
          do n=1, nfiles
            do i=1,ncols
              do j=1,nlines
                if (n.ne.noise) then
                cor=out(n,i,j)/out(noise,i,j)
                  if (cor.lt.6.) then
                    out(n,i,j)=0.  
                  endif
                endif
               enddo
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
             if (out(Ha6563,i,j).eq.0.) then
               NIIratio(i,j)=0.
c corriger le 1000/3 a 4/3
             else  
            NIIratio(i,j)=(1000.*out(NII6584,i,j))/(3.*out(Ha6563,i,j))      
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
             aptmp=8500.
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
               if (densit(i,j).gt.15000.) then
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
c interpolation de limage temperature par ponderation
       box=50
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
                     distv=100.
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
c lissage de limage de densite par ponderation
       do i=box, ncols-box
         do j=box, nlines-box
           add=0.
           distt=0.
           if (densit(i,j).eq.0.) then
            do n=i-(box-1), i+(box-1)
               do m=j-(box-1), j+(box-1)
                 distv=0.                            
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
        stop
c        return
c        stop 
	end 
