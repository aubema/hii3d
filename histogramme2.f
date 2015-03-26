        subroutine histo(tempVSr,neVSr)
        integer i,j,n,r,nmax,k,Ntot(450)
        real b1,b2, tempVSr(450,3000),neVSr(450,3000),bmax,bmiroir
        real Nvsr(450,3000)
        character*8 fileT,fileN
        character*3 flagT,flagN
        character*6 fmt

        fmt = '(I3.3)'
        do r=1,450
        write (flagT,fmt) r
        fileT='HistT'//trim(flagT)
        open(unit=1,file=fileT,status='unknown')
        b1=7000.
        do i=1,80
        n=0
        b2=b1+100.
        do j=1,3000
        if ((tempVSr(r,j).gt.b1).and.(tempVSr(r,j).le.b2)) then
        n=n+1
        end if
        end do
          write(1,*) (b1+b2)/2.,n
        b1=b2
        end do
        close(unit=1)
        end do

        fmt = '(I3.3)'
        do r=1,450
        nmax=0
        bmax=0.
        write (flagN,fmt) r
        fileN='HistN'//trim(flagN)
        open(unit=1,file=fileN,status='unknown')
        b1=15.
        do i=1,80
        n=0
        b2=b1+5.
        do j=1,3000
        if ((neVSr(r,j).gt.b1).and.(neVSr(r,j).le.b2)) then
        n=n+1
        end if
        end do
          if (n.gt.nmax) then
          nmax=n
          bmax=(b1+b2)/2.
          endif
        b1=b2
        end do
        write(1,*) bmax,nmax


          





        b1=bmax
        Ntot(r)=nmax
        do i=1,80
        n=0
        b2=b1+5.
        do j=1,3000
        if ((neVSr(r,j).gt.b1).and.(neVSr(r,j).le.b2)) then
        n=n+1
        Ntot(r)=Ntot(r)+1
        end if
        end do
        do k=1,nmax
            Nvsr(r,j)=bmax
        enddo
          write(1,*) (b1+b2)/2.,n
          do k=1,n
            Nvsr(r,j)=(b1+b2)/2.
          enddo
          bmiroir=bmax-((b1+b2)/2.-bmax)
          write(1,*) bmiroir ,n
          do k=1,n
            Nvsr(r,j)=bmiroir
            Ntot(r)=Ntot(r)+1
          enddo
        b1=b2
        end do
        print*, Ntot(17) 
        close(unit=1)
        end do
c        do r=1,450
c             do k=1,3000
c              if (Nvsr(r,k).ne.0) then
c              print*, Nvsr(r,k)
c              endif
c             enddo
c        enddo
        return
        end subroutine 
