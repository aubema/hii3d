c least square method
c
         character*72 name,obsNII,obsSII,modNII,modSII
         character*12 nom
         integer i,j,nbx,nby
         real toto
         real rms,rmsNii,rmsSii,dobsnii(401,401),dobssii(401,401)
         real dmodnii(401,401),dmodsii(401,401)
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
         do i=1,nbx
           do j=1,nby
              rmsNii=rmsNii+(dobsnii(i,j)-dmodnii(i,j))**2.
              rmsSii=rmsSii+(dobssii(i,j)-dmodsii(i,j))**2.
           enddo
         enddo
         rms=rmsNii+rmsSii

         open(unit=1,file='cases-comparizon.txt',status='unknown',
     +   position="append")
             write(1,*) name, rms
         close(unit=1)
         stop
         end
            
         
