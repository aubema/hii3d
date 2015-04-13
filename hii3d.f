c programme hii3d.f 
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
c    Decrire ici le programme dans son ensemble
c
c
c
c
c
c
c
c
c    Copyright (C) 2014  Alexandre Carbonneau, Catherine Masson, Maude Roy-Labbe, Martin Aubé, 
c    Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
c             
        real densip(401,401),tempp(401,401),square(401,401,361)
	real neVSr(401,3000),temp(401,401),temp3d(401,401,401)
        real moyt(401),moy(401,401),sigmat(401),sigma(401,401)
        real Nvsr(401,3000),SIIrat(401,401),miniNe
        real ne3d(401,401,401),R3D,xr,xc,yr,yc,rcirc
        real intmin, intmax, SII3d(401,401,401),mmn2,fillfa(401,401)
        real vars(401,401,401),SIImod(401,401),vmin,vmax,xcell0,ycell0
        real NII3d(401,401,401),NIImod(401,401),NIIrat(401,401)
        real gain,offset,toverr,random,Ne(401,401,401),Te(401,401,401)
        real Nemod(401,401),rint,rathol,ine,ene,maxiNe,rijk
        integer Ntot(401),siz,taille,binf,bsup,binfz,bsupz,ni,nj,nk
        integer nbx, nby, ndata(401,401),ndatN(401),i,j,r,k,n
        integer valmax,pixsiz,nmod,center
        character*20 namef(30)
        character*40 outfil,tdname
        character*12 nom
        taille=3                                                         ! valeur maximale de 19
        toverr=1.                                                        ! ratio of the max thickness of the object over its lateral radius
        print*,'Enter sampling window size, external radius, center hole
     +   fraction, internal density and external density'
        read*,taille,rcirc,rathol,ine,ene                                          ! hii3d prends en entree la taille de la fenetre et la faction du trou central vs le rayon de l'objet.
c on prend les données de raies d'emission qu'on transforme en ratio de raies et 
c en temperature et densite electronique avec Osterbrock
        call TNelines(nbx,nby,tempp,densip,SIIrat,NIIrat)
        print*,'Image size:',nbx,'x',nby
c
c ===================================================
c SII ratio
c
c creation de matrices taille x taille centrees sur chaque pixel
c les statistiques locales seront faites a l interieur de ces matrices
        call squaredata(nbx,nby,taille,SIIrat,square,ndata)
c calcul de l histogramme pour diagnostique seulement
c        print*,'Producing histograms...'
c        call histo(square,taille)
c calcul de la moyenne et de l ecart type pour chaque matrice 11x11
        print*,'Calculating standard deviations and averages'
        call moyecart(nbx,nby,taille,square,ndata,moy,sigma)
c elargissement de l ecart type
        print*,'Increasing the standard deviations...'
c        call circle(sigma,nbx,nby)
        call ellipse(sigma,nbx,nby,toverr,rcirc)
c faire les matrices 3D
	open(unit=1,file='rond.in',status='old')
          read(1,*) xc,yc
          read(1,*) xr,yr
c          rcirc=sqrt((xr-xc)**2.+(yr-yc)**2.)
        close(unit=1)
        do i=1,401
           do j=1,401
              fillfa(i,j)=1.
              do k=1,401
                 SII3d(i,j,k)=-1. 
              enddo
           enddo
        enddo  
        print*,'Object radius=',rcirc,'pixels'
c trouver les bornes de la distribution
        binf=(nint(rcirc)+10)/taille*taille
        binfz=(nint(rcirc*toverr)+10)/taille*taille
        print*,'Finding data range and filling factor...'
           intmax=0.
           intmin=10000.
        do i=nint(xc)-binf,nint(xc)+binf
           do j=nint(xc)-binf,nint(xc)+binf
           fillfa(i,j)=(3.*sigma(i,j))/moy(i,j)
           if (fillfa(i,j).gt.1.) then
              sigma(i,j)=sigma(i,j)/fillfa(i,j)
              fillfa(i,j)=fillfa(i,j)**2.                                         ! a cause du moyennage en 1/sqrt(N)


        print*,fillfa(i,j)


c       SII min=0,45  max=1,43 graph
c       NII min=      max=     graph
           endif
           if (moy(i,j)-3.*sigma(i,j).lt.intmin) then
              intmin=moy(i,j)-3.*sigma(i,j)
           endif
           if (moy(i,j)+3.*sigma(i,j).gt.intmax) then
              intmax=moy(i,j)+3.*sigma(i,j)

           endif
           if (intmin.lt.0.) intmin=0.
           enddo
        enddo
        if (intmin.lt.0.45) intmin=0.45
        if (intmax.gt.1.43) intmax=1.43
c tir aleatoire sur les distributions
        print*,'Tir aleatoire'
        ni=0
        do i=201-binf,201+binf,taille
           nj=0
           ni=ni+1
c on depasse le rayon de la nebuleuse de 10 pixels pour etre certain d avoir toutes 
c les donnees si l objet n est pas parfaitement circulaire
           do j=201-binf,201+binf,taille
              nk=0
              nj=nj+1
              do k=201-binfz,201+binfz,taille




                 nk=nk+1

         rint=sqrt((201.-real(i))**2.+(201.-real(j))**2.+(201.
     +   -real(k))**2.)
         if (rint.ge.rathol*rcirc) then

                 random=rand()*fillfa(ni,nj)

                 if (random.le.1.) then  
              
                    call gaussienne(moy,sigma,i,j,k,nby,R3D,xc,yc,
     +              intmin,intmax,toverr,rcirc)
                    SII3d(ni,nj,nk)=R3D

                 else
                    SII3d(ni,nj,nk)=0.
                    print*,ni,nj,nk
                 endif



         else 
             SII3d(ni,nj,nk)=0.
         endif


              enddo
           enddo
         enddo

c
c producing the modeled SII ratio image along the line of sight
c
         print*,'Calculating modeled SII ratio...'
         do i=1,ni
            do j=1,nj
               SIImod(i,j)=0.
               nmod=0
               do k=1,nk
                  if (SII3d(i,j,k).gt.0.) then
                     nmod=nmod+1
                     SIImod(i,j)=SIImod(i,j)+SII3d(i,j,k)
                  endif
               enddo
               SIImod(i,j)=SIImod(i,j)/real(nmod)
            enddo
         enddo
c Print image ratio SII modelisee          
          vmin=1000000000.
          vmax=0.           
           do i=1,ni
             do j=1,nj
c                if (SIImod(i,j).ne.0.) then
                   if (SIImod(i,j).lt.vmin) then
                      vmin=SIImod(i,j)
                   endif
                   if (SIImod(i,j).gt.vmax) then
                      vmax=SIImod(i,j)
                   endif
c                endif
             enddo
          enddo
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfil="SIImod.pgm"
          xcell0=0.
          ycell0=0.
          nom="SIIratio"
          pixsiz=1.
          valmax=65535
          call extrant2d (outfil,SIImod,nom,xcell0,ycell0,pixsiz,
     + gain,offset,ni,nj,valmax)
c write output 3D file
         tdname='SIIratio3D.txt'
         print*,'Writing 3D matrix...'
         call WriteIFrIT(ni,nj,nk,SII3d,tdname)
c
c ===================================================
c NII ratio
c
c creation de matrices taille x taille centrees sur chaque pixel
c les statistiques locales seront faites a l interieur de ces matrices
        call squaredata(nbx,nby,taille,NIIrat,square,ndata)
        
c calcul de l histogramme pour diagnostique seulement
c        print*,'Producing histograms...'
c        call histo(square,taille)
c calcul de la moyenne et de l ecart type pour chauque matrice 11x11
        print*,'Calculating standard deviations and averages'
        call moyecart(nbx,nby,taille,square,ndata,moy,sigma)
c elargissement de l ecart type
        print*,'Increasing the standard deviations...'
c        call circle(sigma,nbx,nby,rcirc)
        call ellipse(sigma,nbx,nby,toverr,rcirc)


        print*,'bidon'



c faire les matrices 3D
	open(unit=1,file='rond.in',status='old')
          read(1,*) xc,yc
          read(1,*) xr,yr
c          rcirc=sqrt((xr-xc)**2.+(yr-yc)**2.)
        close(unit=1)
        do i=1,401
           do j=1,401
              fillfa(i,j)=1.
              do k=1,401
                 NII3d(i,j,k)=-1. 
              enddo
           enddo
        enddo  
        print*,'Object radius=',rcirc,'pixels'
c trouver les bornes de la distribution
        binf=(nint(rcirc)+10)/taille*taille
        binfz=(nint(rcirc*toverr)+10)/taille*taille
        print*,'Finding data range and filling factor...'
           intmax=-1.
           intmin=100000.
        do i=nint(xc)-binf,nint(xc)+binf
           do j=nint(xc)-binf,nint(xc)+binf
           fillfa(i,j)=(3.*sigma(i,j))/moy(i,j)
           if (fillfa(i,j).gt.1.) then
              sigma(i,j)=sigma(i,j)/fillfa(i,j)
              fillfa(i,j)=fillfa(i,j)**2.                                         ! a cause du moyennage en 1/sqrt(N)


c        print*,fillfa(i,j)
 

          endif
           if (moy(i,j)-3.*sigma(i,j).lt.intmin) then
              intmin=moy(i,j)-3.*sigma(i,j)
           endif
           if (moy(i,j)+3.*sigma(i,j).gt.intmax) then
              intmax=moy(i,j)+3.*sigma(i,j)
           endif
           if (intmin.lt.0.) intmin=0.
           enddo
        enddo
c tir aleatoire sur les distributions
        print*,'Tir aleatoire'
        ni=0
        do i=201-binf,201+binf,taille
           nj=0
           ni=ni+1
c on depasse le rayon de la nebuleuse de 10 pixels pour etre certain d avoir toutes 
c les donnees si l objet n est pas parfaitement circulaire
           do j=201-binf,201+binf,taille
              nk=0
              nj=nj+1
              do k=201-binfz,201+binfz,taille
                 nk=nk+1



         rint=sqrt((201.-real(i))**2.+(201.-real(j))**2.+(201.
     +   -real(k))**2.)
         if (rint.ge.rathol*rcirc) then



                 random=rand()*fillfa(ni,nj)
c                 print*,random,fil

                 if (random.le.1.) then  
              
                    call gaussienne(moy,sigma,i,j,k,nby,R3D,xc,yc,
     +              intmin,intmax,toverr,rcirc)
                    NII3d(ni,nj,nk)=R3D
                 else
                    NII3d(ni,nj,nk)=0.
c        print*,ni,nj,nk
                 endif



         else 
             NII3d(ni,nj,nk)=0.
         endif



              enddo
           enddo
         enddo

c
c producing the modeled NII ratio image along the line of sight
c
         print*,'Calculating modeled NII ratio...'
         do i=1,ni
            do j=1,nj
               NIImod(i,j)=0.
               nmod=0
               do k=1,nk
                  if (NII3d(i,j,k).gt.0.) then
                     nmod=nmod+1
                     NIImod(i,j)=NIImod(i,j)+NII3d(i,j,k)
                  endif
               enddo
               NIImod(i,j)=NIImod(i,j)/real(nmod)

            enddo
         enddo
c Print image ratio NII modelisee          
          vmin=1000000000.
          vmax=0.           
           do i=1,ni
             do j=1,nj
c                if (NIImod(i,j).ne.0.) then
                   if (NIImod(i,j).lt.vmin) then
                      vmin=NIImod(i,j)
                   endif
                   if (NIImod(i,j).gt.vmax) then
                      vmax=NIImod(i,j)
                   endif
c                endif
             enddo
          enddo
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfil="NIImod.pgm"
          xcell0=0.
          ycell0=0.
          nom="NIIratio"
          pixsiz=1.
          valmax=65535
          call extrant2d (outfil,NIImod,nom,xcell0,ycell0,pixsiz,
     + gain,offset,ni,nj,valmax)
c write output 3D file
         tdname='NIIratio3D.txt'
         print*,'Writing 3D matrix...'
         call WriteIFrIT(ni,nj,nk,NII3d,tdname)
c
c===================================================================================
c Processus de calcul pour calculer la densite et la temperature 	
c La densite electronique
         miniNe=100000.
         maxiNr=0.
         do k=1,nk
            do i=1,ni
               do j=1,nj
                  aptmp=8900.
                  dens=0.
                  if ((NII3d(i,j,k).ne.0.).and.(SII3d(i,j,k).ne.0.)) 
     +            then
c Si la valeur converge pas augmente le k
                     somme=0.
                     do n=1,10
                        call intersii (dens, SII3d(i,j,k),aptmp)                            ! routine qui retourne la densite si on lui donne temperature et ratio sii




                        Ne(i,j,k)=dens
                        call temperatureNII(NII3d(i,j,k),dens,aptmp)                        ! cette routine retourne la temperature si on lui donne la densite et le ratio nii
                        Te(i,j,k)=aptmp
                     enddo
c a partir dici cest phil qui essaie de quoi
                        if ((dens.lt.miniNe) .and. (dens.ne.0.)) then 
                        miniNe=dens
                        endif
c et ca finit la partie de phil

                        if ((dens.gt.maxiNe) .and. (dens.ne.0.)) then 
                        maxiNe=dens
                        endif
c Si le ratio est nul, les temperature et la densite ne sont pas consideres
                  else
                     Ne(i,j,k)=0.
                     Te(i,j,k)=0.
 200              endif
                  center=nj/2+1
                  rijk=real(taille)*sqrt(real((j-center)**2+
     +            (i-center)**2+(k-center)**2))
                  if ((Ne(i,j,k).eq.0.).and.(rijk.gt.rcirc*rathol)) 
     +            Ne(i,j,k)=ene
                  
               enddo
            enddo
         enddo
         do k=1,nk
            do i=1,ni
               do j=1,nj
                  if (Ne(i,j,k).gt.19000.) then
                     Ne(i,j,k)=0.
                  endif
               enddo
            enddo
         enddo
         print*,Ne(center,center,center)
         tdname='Ne3D.txt'
         print*,'Writing 3D Ne matrix...'
         call WriteIFrIT(ni,nj,nk,Ne,tdname)
         tdname='Te3D.txt'
         print*,'Writing 3D Te matrix...'
         call WriteIFrIT(ni,nj,nk,Te,tdname)
c
c producing the modeled Ne image along the line of sight
c
         print*,'Calculating modeled NII ratio...'
         do i=1,ni
            do j=1,nj
               Nemod(i,j)=0.
               nmod=0
               do k=1,nk
                  if (Ne(i,j,k).gt.0.) then
                     nmod=nmod+1
                     Nemod(i,j)=Nemod(i,j)+Ne(i,j,k)
                  endif
               enddo
               Nemod(i,j)=Nemod(i,j)/real(nmod)

            enddo
         enddo
c Print image ratio NII modelisee          
          vmin=1000000000.
          vmax=0.           
           do i=1,ni
             do j=1,nj
c                if (Nemod(i,j).ne.0.) then
                   if (Nemod(i,j).lt.vmin) then
                      vmin=Nemod(i,j)
                   endif
                   if (Nemod(i,j).gt.vmax) then
                      vmax=Nemod(i,j)
                   endif
c                endif
             enddo
          enddo
          gain=(vmax-vmin)/65535.
          offset=vmin
          outfil="Nemod.pgm"
          xcell0=0.
          ycell0=0.
          nom="Ne"
          pixsiz=1.
          valmax=65535
          call extrant2d (outfil,Nemod,nom,xcell0,ycell0,pixsiz,
     + gain,offset,ni,nj,valmax)
        print*, 'minimum Ne' , miniNe
        stop
        end
