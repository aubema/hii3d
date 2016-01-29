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
c    Ce programme est le programme principal qui gere toutes les autres routines.
c    Il transforme les donnes de raies d'emission en ratio de raies. Avec ces ratios 
c    de raies, il effectue une analyse basee sur des moyennes, des ecart-types et
c    des fonctions gaussiennes, pour finalement obtenir des matrices de ratios
c    en trois dimensions. Cette grande analyse est repetee deux fois, une fois pour
c    chaque type de raie, soit SII et NII. Le programme utilise les deux matrices
c    de ratios SII et NII ainsi que la theorie d'Osterbrock pour obtenir des matrices
c    de densite electronique et de temperature electronique, de maniere iterative.
c    La matrice de densite electronique est transcrite en texte pour pouvoir etre
c    analysee par d'autres programmes.
c
c
c    Copyright (C) 2015  Martin Aubé, Alexandre Carbonneau, Catherine Masson,  
c    Maude Roy-Labbe, Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
c  Declaration des variables           
      real sqrS(401,401,225),sqrN(401,401,225)
      real moyS(401,401),moyN(401,401),sigmS(401,401)
      real sigmN(401,401),SIIrat(401,401),NIIrat(401,401)
      real R3D,rcirc,intmnN(401,401),intmnS(401,401),intmxN(401,401)
      real intmxS(401,401)
      real xe,ye                                                              ! xe,ye = position de l etoile centrale                                               
      real NII3d(401,401,401),SII3d(401,401,401)
      real SIImod(401,401),NIImod(401,401),vmin,vmax,xcell0,ycell0
      real gain,offset,Ne(401,401,401),Te(401,401,401)
      real Nemod(401,401),ine,ene
      real SIIresol(401,401),NIIresol(401,401)
      real dens,aptmp,somme,pi,convr,distet
      real thetax,shap(401,401,401),thetaz
      real thickc,nmod,ze,sig2no
      real Tmin,Tmax,Nmin,Nmax,ra,pixsiz
      integer box,ni,nj,nk,ii,jj,kk
      integer nbx,nby,ndatS(401,401),ndatN(401,401),i,j,k,n
      integer valmax,imagx,imagy
      integer imin,imax,jmin,jmax,kmin,kmax
      integer inirand,fill(401,401,401)
      character*40 outfil,tdname,tdfile
      character*20 nom
      pi=3.14159265359
      Tmin=6000.
      Tmax=15000.
      Nmin=5.
      Nmax=1000.
      convr=pi/180.                                                       ! angle d'inclinaison dans plan image (x-y) de l'axe des liant les deux coquilles
c ouvrir le fichier random.tmp pour rendre le nombre plus aleatoire
      open(unit=1,file='random.tmp',status='unknown')
        read(1,*) inirand
      close(unit=1)
      do i=1,inirand
        ra=rand()
      enddo 
c ATTENTION. Faut-il encore mettre le +1 a chaque coordonnee ou l'utilisateur devra savoir?
c On demande les coordonnees de l'etoile centrale (154,161) et les dimensions de l'image
      open(unit=2,file='rond.in',status='unknown')
        read(2,*) xe,ye
      close(unit=2)
      open(unit=2,file='geometry.tmp',status='unknown')
c Enter the dimensions of the image
        read(2,*) imagx,imagy
      close(unit=2)

c lecture des parametres variables du modele 3D
c
      open(unit=11,file='hii3d.input',status='unknown')
         read(11,*) 
         read(11,*) thetax
         read(11,*) thetaz
         read(11,*) distet
         read(11,*) rcirc
         read(11,*) thickc
         read(11,*) ine
         read(11,*) ene
      close(unit=11)
c thetax = inclination angle of the line joining the 2 shells centers 
c relative to the horizontal right axis counterclock wise
c      thetax=20.
c thetaz = inclination angle of the line joining the 2 shells centers 
c relative to the line of sight
c      thetaz=50.
c distet = physical distance between the center of each shell
c      distet=30.    
c rcirc = shells radius (the 2 shells are identical in size and shape)                                      
c      rcirc=80.
c thickc = thickness of the shells
c      thickc=20.
c minimal signal to noise ratio for the spectral lines images (sig2no)
      sig2no=6.
c converting to radian
      thetax=thetax*convr
      thetaz=thetaz*convr   
c box est la fenetre glissante utilisee pour calculer les statistiques
c spatiales de l'objet la box de 7 est suggeree pour avoir une 
c statistique potable sans trop degrader la resolution
c La variable box a une valeur maximale de 15.
      box=7
c ine est la densite electronique a l'interieur de la cavite
c      ine=30.
c ene est la densite electronique a l'exterieur de la nebuleuse (r>rcirc)
c      ene=20.
c
c fabrication d'un matrice de flag pour identifier ou est le gaz en 3D
c 0=outside, 1=inside, 2=gaz
      print*,'Creating a 3D map of the nebulae topology...'
      call dblshell(rcirc,thickc,thetaz,thetax,distet,fill,xe,ye)
c
c On appelle la routine SIINIIratio qui prend les donnees de raies 
c d'emission pour les transformer en ratio de raies.
c On se retrouve alors avec une matrice de ratio de raies pour 
c SII et NII.
      print*,'Calculation of the 2D SII and NII ratios...'
      call SIINIIratio(nbx,nby,SIIrat,NIIrat,sig2no)
c
c ===============================================================================================
c Debut de la transformation en 3D du ratio SII, a l'aide de SIIrat.
c
c On appelle la routine squaredata qui cree les matrices box x box centrees sur chaque pixel.
      print*,'Extraction of spatial information around each pixel...'
      call squaredata(nbx,nby,box,SIIrat,sqrS,ndatS)
      call squaredata(nbx,nby,box,NIIrat,sqrN,ndatN)
c
c Les statistiques locales seront faites a l'interieur de la matrice square et ndata est le nombre
c de donnees valides dans le carre.
c
c On appelle la routine moysigma qui calcule la moyenne et l'ecart type pour chaque fenetre 
c box x box centree sur chaque pixel.
c    
      print*,'Doing statistical analysis on each 2D grid point...'
      call moysigma(nbx,nby,box,sqrS,ndatS,moyS,sigmS)
      call moysigma(nbx,nby,box,sqrN,ndatN,moyN,sigmN)
c
c elargissement des ecarts type en fonction de l epaisseur de l objet vis a vis de chaque pixel
c Uniquement les valeurs de 2 dans la matrice fill seront denombres
      print*,'Adaptation of the stastistics to the 3D space...'
      call ensigma(sigmS,nbx,nby,fill)
      call ensigma(sigmN,nbx,nby,fill)
c
c Lorsqu on ne change pas la resolution, la distribution des donnees est trop smooth derriere
c chaque pixel ce qui ne permet pas de reproduire les grumeaux (clumps) presents sur l image observee.
c De plus, on ne peut depasser la capacite de mocassin de 71*71*71.
c Solution = changement de resolution du ratio.
c
      print*,'Definition of the coarser grid...'
      imin=int(xe)-int(xe)/box*box+box
      imax=nbx/box*box
      jmin=int(ye)-int(ye)/box*box+box
      jmax=nby/box*box
      ni=(imax-imin)/box+1
      nj=(jmax-jmin)/box+1
      if (imax.gt.jmax) then
        kmax=imax
        kmin=imin
        ze=xe
      else
        kmax=jmax
        kmin=jmin
        ze=ye
      endif
      nk=(kmax-kmin)/box+1
      ni=0
      do i=imin,imax,box
        nj=0
        ni=ni+1
        do j=jmin,jmax,box
          nj=nj+1
          SIIresol(ni,nj)=moyS(i,j)
          NIIresol(ni,nj)=moyN(i,j)
        enddo
      enddo
      ni=0
      do i=imin,imax,box
        nj=0
        ni=ni+1
        do j=jmin,jmax,box
          nk=0
          nj=nj+1
          do k=kmin,kmax,box
            nk=nk+1
            shap(ni,nj,nk)=real(fill(i,j,k))
          enddo
        enddo
      enddo
      tdfile='shape.txt'
      call WriteIFrIT(ni,nj,nk,shap,tdfile)





c
c On fait la matrice 3D, jusqu'au commentaire Fin de la creation de la matrice 3D.
c
c rechercher des bornes superieures et inferieures pour l histogramme +/- 3 sigma
      print*,'Calculation of the 3D NII and SII ratios...'
      do i=imin,imax,box
        do j=jmin,jmax,box
          intmxN(i,j)=-10000.
          intmnN(i,j)=10000.
          intmxS(i,j)=-10000.
          intmnS(i,j)=10000.
        enddo
      enddo
      do i=imin,imax,box
        do j=jmin,jmax,box
          if (moyS(i,j)-3.*sigmS(i,j).lt.intmnS(i,j)) then
            intmnS(i,j)=moyS(i,j)-3.*sigmS(i,j)
          endif
          if (moyN(i,j)-3.*sigmN(i,j).lt.intmnN(i,j)) then
            intmnN(i,j)=moyN(i,j)-3.*sigmN(i,j)
          endif
          if (moyS(i,j)+3.*sigmS(i,j).gt.intmxS(i,j)) then
            intmxS(i,j)=moyS(i,j)+3.*sigmS(i,j)
          endif
          if (moyN(i,j)+3.*sigmN(i,j).gt.intmxN(i,j)) then
            intmxN(i,j)=moyN(i,j)+3.*sigmN(i,j)
          endif
c Selon le graphique d'Osterbrock, le min=0.45 et le max=1.43 pour la raie SII.
c          if (intmnS(i,j).lt.0.45) print*,'moins que 0.45',intmnS(i,j)
          if (intmnS(i,j).lt.0.45) intmnS(i,j)=0.45
c          if (intmxS(i,j).gt.1.43) print*,'plus que 1.43',intmxS(i,j)
          if (intmxS(i,j).gt.1.43) intmxS(i,j)=1.43
          if (intmnN(i,j).lt.35.) then
c             print*,'moins que 35',intmnN(i,j)
             intmnN(i,j)=35.
          endif
          if (intmxN(i,j).gt.450.) then
c             print*,'plus que 450',intmxN(i,j)
             intmxN(i,j)=450.
          endif
        enddo
      enddo

c EST-CE QU IL FAUT METTRE CES LIMITES POUR NII?
c
c On tire aleatoirement sur les distributions.
c
      ii=0
      do i=imin,imax,box
        jj=0
        ii=ii+1
        do j=jmin,jmax,box
c          SIIverif(ii,jj)=0.
          kk=0
          jj=jj+1
          do k=kmin,kmax,box
            kk=kk+1
 210        if (fill(i,j,k).eq.2) then
c On appelle la routine gaussienne qui tire aleatoirement une valeur de ratio de raie
c dans un ensemble de données cree a partir de moy et sigma.     
              if (sigmS(i,j).ne.0.) then        
                call gaussienne(moyS(i,j),sigmS(i,j),R3D,
     +          intmnS(i,j),intmxS(i,j))
                SII3d(ii,jj,kk)=R3D
              else
                SII3d(ii,jj,kk)=0.
              endif
c              SIIverif(ii,jj)=SIIverif(ii,jj)+SII3d(ii,jj,kk)
c On appelle la routine gaussienne qui tire aleatoirement une valeur de ratio de raie
c dans un ensemble de données cree a partir de moy et sigma.
              if (sigmN(i,j).ne.0.) then
                call gaussienne(moyN(i,j),sigmN(i,j),R3D,
     +          intmnN(i,j),intmxN(i,j))
                NII3d(ii,jj,kk)=R3D
              else
                NII3d(ii,jj,kk)=0.
              endif
            else 
              SII3d(ii,jj,kk)=0.
              NII3d(ii,jj,kk)=0.
            endif
            aptmp=10000.
            dens=0.
c Si la valeur ne converge pas, on augmente le k.
            if ((NII3d(ii,jj,kk).ne.0.).and.(SII3d(ii,jj,kk).ne.0.)) 
     +      then
              somme=0.
              do n=1,10
c On appelle la routine interSII qui retourne la densite si on lui donne la temperature et le ratio SII.
                call interSII (dens, SII3d(ii,jj,kk),aptmp)                           
                Ne(ii,jj,kk)=dens
c On appelle la routine temperatureNII qui retourne la temperature si on lui donne la densite et le ratio NII.
                call temperatureNII(NII3d(ii,jj,kk),dens,aptmp)                       
                Te(ii,jj,kk)=aptmp
              enddo
         if (((Te(ii,jj,kk).lt.Tmin).or.(Te(ii,jj,kk).gt.Tmax)).or.
     +   ((Ne(ii,jj,kk).lt.Nmin).or.(Ne(ii,jj,kk).gt.Nmax))) then
c            print*,ii,jj,Te(ii,jj,kk),Ne(ii,jj,kk)
            goto 210
         endif
c Si le ratio est nul, les temperature et la densite ne sont pas consideres.
            else
              Ne(ii,jj,kk)=0.
              Te(ii,jj,kk)=0.
            endif
c On remplit l'exterieur et l'interieur de la nebuleuse avec la densite entree au debut du programme.
            if (fill(i,j,k).eq.0) then
              Ne(ii,jj,kk)=ene
            endif
            if (fill(i,j,k).eq.1) then
              Ne(ii,jj,kk)=ine
            endif       
            
          enddo
        enddo
      enddo
      print*,'Printing out the 2D images and 3D files...'
c Fin de la creation de la matrice 3D.
c Nous possedons alors des matrice SII3d et NII3d en 3D, remplie de ratios de raies.
c
c
c ==============================================
c Les etapes suivantes servent a produire differentes images pour SII.
c On produit une image le long de la ligne de visee pour le ratio SII modelise.
c
      print*,'Calculating modeled SII ratio...'
      do i=1,ni
        do j=1,nj
          SIImod(i,j)=0.
          nmod=1.
          do k=1,nk
            if (SII3d(i,j,k).gt.0.) then
              SIImod(i,j)=SIImod(i,j)+SII3d(i,j,k)
              nmod=nmod+1.
            endif
          enddo
          SIImod(i,j)=SIImod(i,j)/nmod
        enddo
      enddo
c
c On imprime une image du ratio SII modelise.
c       
      vmin=1000000000.
      vmax=0.           
      do i=1,ni
        do j=1,nj
          if (SIImod(i,j).lt.vmin) then
            vmin=SIImod(i,j)
          endif
          if (SIImod(i,j).gt.vmax) then
            vmax=SIImod(i,j)
          endif
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
c On appelle la routine extrant2d qui transcipt l'image dans un fichier.
      call extrant2d (outfil,SIImod,nom,xcell0,ycell0,pixsiz,
     +gain,offset,ni,nj,valmax)
c On imprime une image du ratio SII observe.
      vmin=1000000000.
      vmax=0.           
      do i=1,ni
        do j=1,nj
          if (SIIresol(i,j).lt.vmin) then
            vmin=SIIresol(i,j)
          endif
          if (SIIresol(i,j).gt.vmax) then
            vmax=SIIresol(i,j)
          endif
        enddo
      enddo
      gain=(vmax-vmin)/65535.
      offset=vmin
      outfil="SIIresol.pgm"
      xcell0=0.
      ycell0=0.
      nom="SIIratio"
      pixsiz=1.
      valmax=65535
c On appelle la routine extrant2d qui transcipt l'image dans un fichier.
      call extrant2d (outfil,SIIresol,nom,xcell0,ycell0,pixsiz,
     +gain,offset,ni,nj,valmax)          
c On ecrit la matrice SII3d dans un fichier.
      tdname='SIIratio3D.txt'
c         print*,'Writing 3D matrix...'
c On appelle la routine WriteIFrIT qui transcript la matrice SII3d en donnees scalaires uniformes
c utilisables par le programme IFrIT.
      call WriteIFrIT(ni,nj,nk,SII3d,tdname)
c Fin de la demarche de creation de la matrice 3D de ratios pour SII.
c ======================================
c Les etapes suivantes servent a produire differentes images pour NII.
c On produit une image le long de la ligne de visee pour le ratio NII modelise.

c         print*,'Calculating modeled NII ratio...'
      do i=1,ni
        do j=1,nj
          NIImod(i,j)=0.
          nmod=1.
          do k=1,nk
            if (NII3d(i,j,k).gt.0.) then
              NIImod(i,j)=NIImod(i,j)+NII3d(i,j,k)
              nmod=nmod+1.
            endif
          enddo
          NIImod(i,j)=NIImod(i,j)/nmod
        enddo
      enddo
c On imprime une image du ratio NII modelise.    
      vmin=1000000000.
      vmax=0.           
      do i=1,ni
        do j=1,nj
          if (NIImod(i,j).lt.vmin) then
            vmin=NIImod(i,j)
          endif
          if (NIImod(i,j).gt.vmax) then
            vmax=NIImod(i,j)
          endif
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
c On appelle la routine extrant2d qui transcipt l'image dans un fichier.
      call extrant2d (outfil,NIImod,nom,xcell0,ycell0,pixsiz,
     +gain,offset,ni,nj,valmax)
c On imprime une image du ratio NII observe.
      vmin=1000000000.
      vmax=0.           
      do i=1,ni
        do j=1,nj
          if (NIIresol(i,j).lt.vmin) then
            vmin=NIIresol(i,j)
          endif
          if (NIIresol(i,j).gt.vmax) then
            vmax=NIIresol(i,j)
          endif
        enddo
      enddo
      gain=(vmax-vmin)/65535.
      offset=vmin
      outfil="NIIresol.pgm"
      xcell0=0.
      ycell0=0.
      nom="NIIratio"
      pixsiz=1.
      valmax=65535
c On appelle la routine extrant2d qui transcipt l'image dans un fichier.
      call extrant2d (outfil,NIIresol,nom,xcell0,ycell0,pixsiz,
     +gain,offset,ni,nj,valmax)          
c On ecrit la matrice SII3d dans un fichier.
      tdname='NIIratio3D.txt'
c         print*,'Writing 3D matrix...'
c On appelle la routine WriteIFrIT qui transcript la matrice NII3d en donnees scalaires uniformes
c utilisables par le programme IFrIT.
      call WriteIFrIT(ni,nj,nk,NII3d,tdname)
c
c Fin de la demarche de creation de la matrice 3D de ratios pour NII.
c===================================================================================
c
c On ecrit les matrices Ne et Te dans un fichier pour chaque.
c On appelle la routine WriteIFrIT qui transcript les matrices Ne et Te en donnees scalaires uniformes
c utilisables par le programme IFrIT.
      tdname='Ne3D.txt'
      print*,'Writing 3D Ne matrix...'
      call WriteIFrIT(ni,nj,nk,Ne,tdname)
      tdname='Te3D.txt'
      print*,'Writing 3D Te matrix...'
      call WriteIFrIT(ni,nj,nk,Te,tdname)
c c Les etapes suivantes servent a produire differentes images pour Ne.
c On produit une image le long de la ligne de visee pour la densite Ne modelisee.
c         print*,'Calculating modeled Ne...'
      do i=1,ni
        do j=1,nj
          Nemod(i,j)=0.
          nmod=1.
          do k=1,nk
            if (Ne(i,j,k).gt.0.) then
              Nemod(i,j)=Nemod(i,j)+Ne(i,j,k)
              nmod=nmod+1.
            endif
          enddo
          Nemod(i,j)=Nemod(i,j)/nmod
        enddo
      enddo
c On imprime une image de la densite Ne modelisee. 
      vmin=1000000000.
      vmax=0.           
      do i=1,ni
        do j=1,nj
          if (Nemod(i,j).lt.vmin) then
            vmin=Nemod(i,j)
          endif
          if (Nemod(i,j).gt.vmax) then
            vmax=Nemod(i,j)
          endif
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
c On appelle la routine extrant2d qui transcipt l'image dans un fichier.
      call extrant2d (outfil,Nemod,nom,xcell0,ycell0,pixsiz,
     +gain,offset,ni,nj,valmax)



c Ici, on appelle MOCASSIN.




c On compare l'image originale avec l'image de MOCASSIN.
c On garde la meilleure compatibilite.

                                                                          ! ATTENTION, cette partie reste a ecrire, il va falloir faire une boucle qui analyse point par point la compatibilite et qui
                                                                          ! la cote en pourcentage, pour pouvoir conserver le meilleur resultat. Il faut un if qui va sassurer de conserver la meilleure
                                                                          ! matrice et tous ses parametres.

c Fin du programme hii3d de creation d'une matrice 3D de la nebuleuse.
        stop
        end
