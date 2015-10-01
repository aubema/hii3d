        subroutine histo(square,taille)
        integer i,j,n,r,nmax,k,Ntot(401),taille
        real b1,b2,bmax,bmiroir
        real square(401,401,361)
        character*8 fileT,fileN
        character*3 flagT,flagN
        character*6 fmt
        fmt = '(I3.3)'
        
        write (flagT,fmt) 150
        fileT='HistT'//trim(flagT)
        open(unit=1,file=fileT,status='unknown')
        b1=1.25
        n=0
        do i=1,20
        n=0
        b2=b1+0.0175
        do j=1,2*taille
        if ((square(150,150,j).gt.b1).and.(square(150,150,j).le.b2)) 
     +  then
        n=n+1
        end if
        enddo
          write(1,*) (b1+b2)/2.,n
        b1=b2
        end do
        close(unit=1)

 
        return
        end subroutine
