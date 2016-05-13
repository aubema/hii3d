c find lower value in a list
c
         character*72 solution,name
         character*12 bidon
         integer nbcas
         real old
         real rms,rmsNii,rmsSii,dobsnii(401,401),dobssii(401,401)
         real dmodnii(401,401),dmodsii(401,401)

         open(unit=1,file='nbcas.txt',status='old')
             read(1,*) nbcas, bidon
         close(unit=1)
         old=1.e30
         open(unit=2,file='cases-comparizon.txt',status='old')
           do i=1,nbcas
              read(2,*) name,rms
              if (rms.lt.old) then
                 old=rms
                 solution=name
              endif
           enddo
           print*,'The best 3D model is:',solution 
         close(unit=2)
         stop
         end
            
         
