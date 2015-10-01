c programme questions.f 
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
c    Ce programme sert a obtenir le rayon et le centre de chaque ellipse, 
c    selon le type de nebuleuse. Il demande aussi les dimensions de l'image.
c
c
c    Copyright (C) 2014   Martin Aubé, Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c 
        subroutine questions(xc1,yc1,xc2,yc2,rcirc1,rcirc2,imagx,
     +   imagy,typneb,xr1,yr1,xr2,yr2,xr3,yr3,xr4,yr4,xe,ye)
        real xr,xc,yr,yc,rcirc,xc1,yc1,xc2,yc2
        real rcirc1,rcirc2,xr1,yr1,xr2,yr2,xr3,yr3,xr4,yr4,xe,ye
        integer imagx,imagy,typneb

        print*, 'Which type of planetary nebula do you want to modelise?'
        print*, '1)Simple nebula'
        print*, '2)Nested bipolar nebula'
        print*, '3)Distinct bipolar nebula'
        read*, typneb

        if (typneb.eq.1.) then 
        print*, 'Enter central star coordinate'
        read xc yc
        print*, 'Enter the most external coordinate'
        read xr yr
        xc1=xc
        yc1=yc
        xc2=xc
        yc2=yc
        rcirc1=sqrt(real((xr-xc)**2.+(yr-yc)**2.))
        rcirc2=rcirc1
        endif

        if (typneb.eq.2.) then
        print*, 'Enter one ellipse extremity coordinate (you should write two pair of number)'
        read xr1 yr1 xr2 yr2
        print*, 'Enter the other ellipse extremity coordinate (you should write two pair of number)'
        read xr3 yr3 xr4 yr4
        xc1=(xr1+xr2)/2.
        yc1=(yr1+yr2)/2.
        xc2=(xr3+xr4)/2.
        yc2=(yr3+yr4)/2.
        rcirc1=sqrt(real((xr1-xr2)**2.+(yr1-yr2)**2.))
        rcirc2=sqrt(real((xr3-xr4)**2.+(yr3-yr4)**2.))
        endif

        if (typneb.eq.3.) then
        print*, 'Enter one ellipse extremity coordinate (you should write one pair of number)'
        read xr1 yr1
        print*, 'Enter the other ellipse extremity coordinate (you should write one pair of number)'
        read xr2 yr2
        print*, 'Enter central star coordinate'
        read xe ye
        xc1=(xr1+xe)/2.
        yc1=(yr1+ye)/2.
        xc2=(xr2+xe)/2.
        yc2=(yr2+ye)/2.
        rcirc1=sqrt(real((xr1-xe)**2.+(yr1-ye)**2.))
        rcirc2=sqrt(real((xr3-xe)**2.+(yr3-ye)**2.))
        endif

        print*, 'Enter the dimensions of the images'
        read imagx imagy
        
        return
        end subroutine
