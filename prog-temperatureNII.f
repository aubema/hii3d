c programme temperatureNII.f 
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
c    Ce programme est une routine servant a trouver la temperature
c    a l'aide de la densite et  du ratio de NII. En utilisant l'equation
c    d'Osterbrock, nous trouvons la temperature de maniere iterative.
c
c
c    Copyright (C) 2014  Alexandre Carbonneau, Catherine Masson, Maude Roy-Labbe, Martin Aubé, 
c    Thierry Daviault, Philippe Karan, Alice Roy-Labbe, Sunny Roy
c
       subroutine temperatureNII(NIIra,Nel,aptmp)
       real temp,NIIra,NIIrca,Nel, aptmp
       integer i
       temp=6000.
       do i=1,200
       temp=temp+100.
       NIIrca=6.91*exp(2.5e4/temp)/(1.+2.5e-3*(Nel/temp**0.5))
          if (NIIrca.le.NIIra) then 
          aptmp=temp
          goto 100 
          endif
       enddo
 100   return 
       end

