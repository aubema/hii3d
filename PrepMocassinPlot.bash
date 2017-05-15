#!/bin/bash
# This program will prepare the plot.in file for mocassin flux extraction
# in the framework of the hii3d package
# must be run in the mocassin_cases directory
#
# making list of cases
pwd
echo "Are you sure you are in the mocassin_cases directory? (y/n)"
read answer1
echo "Are you sure that mocassin calculations are all completed? (y/n)"
read answer2
if [ "$answer1" != "y" ] &&  [ "$answer2" != "y" ]
then echo "go the the mocassin_cases directory and/or wait for mocassin calculations to be completed"
     exit 0
else
   rm -f *.tmp
   rm -f ../mocassinPlot.bash
   rm -f ../leastSquare.bash
   mopath=`pwd`
   list=`ls -1`
   for i in $list
   do echo "Case "$i
      # finding index of the SII 6716 line
      grep -1 "6718\.30" $i/output/lineFlux.out | tail -1 > toto.tmp
      read bidon I6716 bidon < toto.tmp
      if [ "$I6716" != "$I6716_old" ]
      then echo "I6716="$I6716
      fi
      I6716_old=$I6716
      # finding index of the SII 6731 line
      grep -1 "6732\.69" $i/output/lineFlux.out | tail -1 > toto.tmp
      read bidon I6731 bidon < toto.tmp
      if [ "$I6731" != "$I6731_old" ]
      then echo "I6731="$I6731
      fi
      I6731_old=$I6731

      # finding index of the NII 5755 line
      grep -1 "5756\.19" $i/output/lineFlux.out | tail -1 > toto.tmp
      read bidon I5755 bidon < toto.tmp
      if [ "$I5755" != "$I5755_old" ]
      then echo "I5755="$I5755
      fi
      I5755_old=$I5755
      # finding index of the NII 6584 line
      grep -1 "6585\.27" $i/output/lineFlux.out | tail -1 > toto.tmp
      read bidon I6584 bidon < toto.tmp
      if [ "$I6584" != "$I6584_old" ]
      then echo "I6584="$I6584
      fi
      I6584_old=$I6584
      echo "mono" > $i/input/plot.in
      echo "line "$I6584 " 6584. 6584."  >> $i/input/plot.in
      echo "line "$I5755 " 5755. 5755."  >> $i/input/plot.in
      echo "line "$I6716 " 6716. 6716."  >> $i/input/plot.in
      echo "line "$I6731 " 6731. 6731."  >> $i/input/plot.in

      echo "cd " $mopath"/"$i >>  ../mocassinPlot.bash
      echo "qsub ./submitPlot.pbs" >> ../mocassinPlot.bash
      echo "sleep 0.05"  >> ../mocassinPlot.bash
      # file leastSquare.bash
      echo "cd " $mopath"/"$i  >> ../leastSquare.bash
      echo "rm -f cases-comparizon.tmp" >> ../leastSquare.bash
      echo "ln -s "$HOME"/hg/bin/prog-simul-ratio ." >> ../leastSquare.bash
      echo "ln -s "$HOME"/hg/bin/prog-rms ." >> ../leastSquare.bash
      echo "cp -f output/plot.out ." >> ../leastSquare.bash
      echo "./prog-simul-ratio" >> ../leastSquare.bash
      echo "echo \""$i" \"> rms.tmp" >> ../leastSquare.bash
      echo "./prog-rms < rms.tmp" >> ../leastSquare.bash
      echo "cat cases-comparizon.tmp >> ../../cases-comparizon.txt" >> ../leastSquare.bash
   done
fi
