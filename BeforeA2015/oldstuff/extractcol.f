         subroutine extractcol (donnee, col, vecttemp, nbline)
         Real donnee(2000,2000),vecteury(3000),vecttemp(3000)
         Integer nbline,j,col

c       Ce programme isole une colonne de l'image
c         
c    
         do j=1,3000
           vecteury(j)=0.
         enddo      
          do j=1,nbline
            vecteury(j)=donnee(col,j)         
          enddo
          vecttemp(1)=vecteury(1)
          vecttemp(nbline)=vecteury(nbline)
          do j=2,nbline-1
             if ((vecteury(j-1).lt.2000.).and.(vecteury(j+1).lt.2000.)
     +          .and.(vecteury(j).ge.2000.)) then
                vecttemp(j)=(vecteury(j-1)+vecteury(j+1))/2.
             elseif ((vecteury(j-1).gt.2000.).and.(vecteury(j+1)
     +              .gt.2000.).and.(vecteury(j).lt.2000.)) then
                vecttemp(j)=(vecteury(j-1)+vecteury(j+1))/2.
             else
                vecttemp(j)=vecteury(j)
             endif
          enddo
          return
          end
