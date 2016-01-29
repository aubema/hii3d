c programme interSII.f 
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
c    Ce programme est une routine servant a determiner la densite electronique
c    a partir de la figure 5.3 (Osterbrock 1989), de la temperature et du ratio SII. 
c    La precision de la digitalisation de la courbe est estimee a .01 sur le rapport d'intensite.
c
c
c    Copyright (C) 2014  Alexandre Carbonneau, Catherine Masson, Maude Roy-Labbe, Martin Aubé, 
c    Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
       subroutine interSII (dens, rapp, aptmp)
       dimension ratio(19,2)
       integer n
       real dens, rapp, aptmp, ratio,m,b     
       ratio(1,1)=1.43
       ratio(2,1)=1.42
       ratio(3,1)=1.41
       ratio(4,1)=1.39
       ratio(5,1)=1.36
       ratio(6,1)=1.32
       ratio(7,1)=1.25
       ratio(8,1)=1.18
       ratio(9,1)=1.10
       ratio(10,1)=1.00
       ratio(11,1)=0.86
       ratio(12,1)=0.70
       ratio(13,1)=0.61
       ratio(14,1)=0.56
       ratio(15,1)=0.52
       ratio(16,1)=0.49
       ratio(17,1)=0.46
       ratio(18,1)=0.451
       ratio(19,1)=0.45
       ratio(1,2)=10.
       ratio(2,2)=20.
       ratio(3,2)=30.
       ratio(4,2)=40.
       ratio(5,2)=50.
       ratio(6,2)=100.
       ratio(7,2)=200.
       ratio(8,2)=300.
       ratio(9,2)=400.
       ratio(10,2)=500.
       ratio(11,2)=1000. 
       ratio(12,2)=2000.
       ratio(13,2)=3000.
       ratio(14,2)=4000.
       ratio(15,2)=5000.
       ratio(16,2)=10000.
       ratio(17,2)=20000.
       ratio(18,2)=30000.
       ratio(19,2)=100000.
       do i=1,19
         ratio(i,2)=ratio(i,2)*sqrt(10000./aptmp)                                    ! correction de l'echelle horizontale pour la temparature du grap. 5.3 de Osterbrock
       enddo
       n=1
       do while (rapp.lt.ratio(n,1))
           n=n+1
           m=(ratio(n-1,1)-ratio(n,1))/(ratio(n-1,2)-ratio(n,2))
           b=ratio(n-1,1)-m*ratio(n-1,2)
           dens=(rapp-b)/m
       enddo
       return
       end      
