         subroutine extractline (donnee, line, vecteurx, nbcol)
         Real donnee(2000,2000), vecteurx(3000),vecttemp(3000)
         Integer nbcol,i,line,n,fenet



         fenet=8
         do i=1,3000
           vecteurx(i)=0.
         enddo
          do i=1,nbcol
            vecteurx(i)=donnee(i,line)         
          enddo
          do i=fenet+1,nbcol-fenet
            vecttemp(i)=0.
            do n=i-fenet,i+fenet
               vecttemp(i)=vecttemp(i)+vecteurx(n)
            enddo
            vecttemp(i)=vecttemp(i)/real(2*fenet+1)
          enddo
          do i=1,nbcol
            if (abs((vecttemp(i)-vecteurx(i))).gt.4000.) then
c attention le seuil doit changer selon la variable T ou Ne
               vecteurx(i)=vecttemp(i)
            endif
          enddo
          return
          end
