c least square method
c
         character*72 name,obsNII,obsSII,modNII,modSII
         character*12 nom
         integer i,j,nbx,nby
         real toto
         real rms,rmsNii,rmsSii,dobsnii(401,401),dobssii(401,401)        ! dobs est le ratio observe et dmod est le ratio de mocassin
         real dmodnii(401,401),dmodsii(401,401)
         real moyNii,moySii,nNii,nSii,avgSii,avgNii
         nom='bidon'
         print*,'Name of the case'
         read*,name
         obsNII='NIIresol.pgm'
         obsSII='SIIresol.pgm'
         modNII='NII-mocassin.pgm'
         modSII='SII-mocassin.pgm'
         call intrants2d(obsNII,dobsnii,nom,toto,toto,toto,nbx,nby)
         call intrants2d(obsSII,dobssii,nom,toto,toto,toto,nbx,nby)       
         call intrants2d(modNII,dmodnii,nom,toto,toto,toto,nbx,nby)
         call intrants2d(modSII,dmodsii,nom,toto,toto,toto,nbx,nby)
         rmsNii=0.
         rmsSii=0.
         moyNii=0.
         moySii=0.
         nNii=0.
         nSii=0.
         do i=1,nbx
           do j=1,nby
              if (dobsnii(i,j).ne.0.) nNii=nNii+1.
              if (dobssii(i,j).ne.0.) nSii=nSii+1.
              moyNii=moyNii+dobsnii(i,j)
              moySii=moySii+dobssii(i,j)
              rmsNii=rmsNii+(dobsnii(i,j)-dmodnii(i,j))**2.
              rmsSii=rmsSii+(dobssii(i,j)-dmodsii(i,j))**2.
           enddo
         enddo
         moyNii=moyNii/nNii
         moySii=moySii/nSii
         rmsNii=rmsNii/nNii
         avgNii=rmsNii
         rmsSii=rmsSii/nSii
         avgSii=rmsSii
         rmsNii=sqrt(rmsNii)
         rmsSii=sqrt(rmsSii)
         rmsNii=rmsNii/moyNii
         rmsSii=rmsSii/moySii
         rms=rmsNii+rmsSii

         open(unit=1,file='cases-comparizon.tmp',status='unknown',
     +   position="append")
c printing the name_of_case average_of_squared_residuals mean_ratio_SII number_averaged_pixels
             write(1,*) name, avgSii, moySii, nSii
         close(unit=1)
         stop
         end
            
         
