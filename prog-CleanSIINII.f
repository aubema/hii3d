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
        subroutine SIINIIClean(ncols,nlines,S6716,S6731,N6584,N5755,
     +  sig2no)
        real out(30,401,401),vect(1000000),vmin,vmax,gain,offset
        real valeur(401,401),S6716(401,401),S6731(401,401)
        real N6584(401,401),N5755(401,401)
        real sig2no,xcell0,ycell0,pixsiz
        integer i,j,nlines,ncols,pos,length,nfiles,n,ii
        integer nbx,nby,valmax,SII6716,SII6731,SIIBx,SIIBy
        integer NII6584,NII5755,NIIBx,NIIBy
        character*20 namef(30)
        character*40 outfil
        character*20 nom
c On prepare les donnees d'emission de raies.

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

        do i=1,ncols
          do j=1,nlines
             S6716(i,j)=out(SII6716,i,j)
             S6731(i,j)=out(SII6731,i,j)
             N6584(i,j)=out(NII6584,i,j)
             N5755(i,j)=out(NII5755,i,j)
          enddo
        enddo

        return
        end 
