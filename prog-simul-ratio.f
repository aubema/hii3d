c program to read the lineflux file produced by mocassin
c and produce the simulated ratios of NII and SII
c
c read mocassion file
ci=imin,imax,box
c spectral lines per column = 6583 5755 6716 6731
      integer soonze,i,j,k,valmax
      real flux2d(71,71,4),flux(71,71,71,4),SII(401,401),NII(401,401)
      real vmin,vmax,xcell0,ycell0,offset,pixsiz,gain,bidon
      character*20 nom
      character*40 outfil
      soonze=71
      open(unit=1,file='plot.out',status='old')
        do i=1,soonze
           do j=1,soonze
              do k=1,soonze
                 read(1,*) bidon,flux(i,j,k,1),flux(i,j,k,2),
     +           flux(i,j,k,3),flux(i,j,k,4)   
              enddo
           enddo
         enddo
      close(unit=1)
c integrate the fluxes along the line of sight
      do i=1,71
         do j=1,71
           flux2d(i,j,1)=0.
           flux2d(i,j,2)=0.
           flux2d(i,j,3)=0.
           flux2d(i,j,4)=0.
           do k=1,71
             flux2d(i,j,1)=flux2d(i,j,1)+flux(i,j,k,1)
             flux2d(i,j,2)=flux2d(i,j,2)+flux(i,j,k,2)
             flux2d(i,j,3)=flux2d(i,j,3)+flux(i,j,k,3)
             flux2d(i,j,4)=flux2d(i,j,4)+flux(i,j,k,4)
           enddo
         enddo
      enddo
c compute the NII and SII ratios
        do i=1,soonze
         do j=1,soonze
            if (flux2d(i,j,4).gt.0.) then
              SII(i,j)=flux2d(i,j,3)/flux2d(i,j,4)
            else
              SII(i,j)=0.
            endif
            if (flux2d(i,j,2).gt.0.) then
              NII(i,j)=4.*flux2d(i,j,1)/(3.*flux2d(i,j,2))
            else
              NII(i,j)=0.
            endif       
         enddo
       enddo
c writing image ratios
      vmin=1000000000.
      vmax=0.           
      do i=1,soonze
        do j=1,soonze
          if (SII(i,j).lt.vmin) then
            vmin=SII(i,j)
          endif
          if (SII(i,j).gt.vmax) then
            vmax=SII(i,j)
          endif
        enddo
      enddo
      gain=(vmax-vmin)/65535.
      offset=vmin
      outfil="SII-mocassin.pgm"
      xcell0=0.
      ycell0=0.
      nom="SIIratio"
      pixsiz=1.
      valmax=65535 
      call extrant2d (outfil,SII,nom,xcell0,ycell0,pixsiz,
     +gain,offset,soonze,soonze,valmax)   
      vmin=1000000000.
      vmax=0.           
      do i=1,soonze
        do j=1,soonze
          if (NII(i,j).lt.vmin) then
            vmin=NII(i,j)
          endif
          if (NII(i,j).gt.vmax) then
            vmax=NII(i,j)
          endif
        enddo
      enddo
      gain=(vmax-vmin)/65535.
      offset=vmin
      outfil="NII-mocassin.pgm"
      xcell0=0.
      ycell0=0.
      nom="NIIratio"
      pixsiz=1.
      valmax=65535 
      call extrant2d (outfil,NII,nom,xcell0,ycell0,pixsiz,
     +gain,offset,soonze,soonze,valmax) 
      stop
      end
