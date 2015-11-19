c programme SIINIIratio.f
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
c    Ce programme a pour but de transformer les images d'intensite de raies de la 
c    nebuleuse en images de ratio de raies, ainsi qu'en matrice comportant
c    un ratio de raie pour chaque coordonnee de l'image.
c
c    Copyright (C) 2013  Alexandre Carbonneau, Catherine Masson (Alexandrine), Maude Roy-Labbe (support moral important) 
c    et ELPHES+Guigui (support moral moins important),Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c       
        subroutine SIINIIratio(ncols, nlines,SIIrat,NIIrat)
        real out(30,401,401),vect(1000000),vmin,vmax,gain,offset
        real valeur(401,401),SIIrat(401,401),NIIrat(401,401)
        real sig2no,xcell0,ycell0,pixsiz
        integer i,j,nlines,ncols,pos,length,nfiles,n,ii
        integer nbx,nby,valmax,SII6716,SII6731,SIIBx,SIIBy
        integer NII6584,NII5755,NIIBx,NIIBy
        character*20 namef(30)
        character*40 outfil
        character*12 nom
c On prepare les donnees d'emission de raies.

c On definit le signal sur bruit.
        sig2no=3.
c On definit le rayon d'interpolation.
        open(unit=1,file='geometry.tmp',status='unknown')
           read(1,*) ncols, nlines
           read(1,*) nfiles
           do n=1,nfiles
              read(1,*) namef(n)
c lecture de la liste des images de signal et bruit (fichiers .fits)
           enddo
	close(unit=1)
        do n=1,nfiles
c        print*,namef(n)
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

c Les images .pgm des images originales .fit
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
c On appelle la routine extrant2d qui transcipt l'image dans un fichier.
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
        enddo

c P.S. si on perd les images, on reutiliser celles du dossier 
c newimage en multipliant _int et _sigma selon la formule suivante
c imarith SII6716_int.fit SII6716_sigma.fit mul SII6716-s.fit


c Processus pour enlever le bruit des images de raies originales.
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
c On fait les ratios SII et NII.
         
c Ratio SII
          do i=1,ncols
             do j=1,nlines
                if (out(SII6731,i,j).eq.0.) then
                  SIIrat(i,j)=0.
                else 
                SIIrat(i,j)=out(SII6716,i,j)/out(SII6731,i,j)
                  if (SIIrat(i,j).lt.0.2) then
c la courbe du ratio de Osterbrock a un minimum a ~0.4 on utilise 0.2 en raison du bruit (a verifier)
                    SIIrat(i,j)=0.
                  else if (SIIrat(i,j).gt.2.) then
c la courbe du ratio de Osterbrock a un maximum ~1.45 on utilise 2.0 en raison du bruit (a verifier)
                    SIIrat(i,j)=0.
                  endif
                endif
             enddo
          enddo

c Ratio NII
          do i=1,ncols
           do j=1,nlines
             if (out(NII5755,i,j).eq.0.) then
               NIIrat(i,j)=0.
             else  
               NIIrat(i,j)=(4.*out(NII6584,i,j))/(3.*out(NII5755,i,j))       ! on corrige a 4/3 la probabilite de transision des raies 6548 et 6583 
                  if (NIIrat(i,j).lt.0.) then
                    NIIrat(i,j)=0.
                  else if (NIIrat(i,j).gt.300.) then
                    NIIrat(i,j)=0.
                  endif
              endif
           enddo
          enddo

c===================================================================================================
c On imprime toutes les images.

c On imprime l'image de ratio SII.

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
c On appelle la routine extrant2d qui transcipt l'image dans un fichier.
          call extrant2d (outfil,SIIrat,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)

c On imprime l'image de ratio NII.

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
c On appelle la routine extrant2d qui transcipt l'image dans un fichier.
          call extrant2d (outfil,NIIrat,nom,xcell0,ycell0,pixsiz,
     + gain,offset,nbx,nby,valmax)

        return
	end 
