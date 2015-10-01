        subroutine moyecart (tempVSr,neVSr,ndata,moyt,sigmat,moyn,sigman)
        integer r,j,ndata(450)
        real t,e,tempVSr(450,3000),neVSr(450,3000),moyt(450),moyn(450),sigmat(450)
        real sigman(450)

c    temperature
        do r=1,450
	   t = tempVSr(r,1)
	  do j=2,ndata(r)
	     t = t + tempVSr(r,j)
	
	  end do

        moyt(r) = t/ndata(r)
        Print*, moyt(r)
 
        end do

        do r=1,450
	   e = (tempVSr(r,1)**2)
	  do j=2,ndata(r)
	  e = e + ((1/j)*((tempVSr(r,j))**2))
	
          end do
	
       sigmat(r)=sqrt(e-(moyt(r)**2))
       Print*, sigmat(r)
 
       end do
       
c     densite
       do r=1,450
	   t = neVSr(r,1)
	  do j=2,ndata(r)
	     t = t + neVSr(r,j)
	
	  end do

        moyn(r) = t/ndata(r)
        Print*, moyn(r)
 
        end do

        do r=1,450
	   e = (neVSr(r,1)**2)
	  do j=2,ndata(r)
	  e = e + ((1/j)*((neVSr(r,j))**2))
	
          end do
	
       sigman(r)=sqrt(e-(moyn(r)**2))
       Print*, sigman(r)
 
       end do



       return
       end subroutine
