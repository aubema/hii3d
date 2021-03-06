#!/bin/bash
#This is make-hii3d, for compilate the programs
# hii3d compiling script
#
#cd $HOME/svn/hii3d
gfortran -mcmodel=medium -Wall -fcheck=all -g -fbacktrace -ffpe-trap=zero,overflow,underflow prog-hii3d-v1.f prog-CleanSIINII.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-dblshell.f prog-en-sigma.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o prog-hii3d
gfortran prog-simul-ratio.f prog-extrant2d.f -o prog-simul-ratio
gfortran prog-rms.f prog-intrants2d.f -o prog-rms
gfortran prog-solution.f -o prog-solution

# gfortran -mcmodel=large prog-hii3d-v1.f prog-SIINIIratio.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-dblshell.f prog-en-sigma.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o prog-hii3d
#Compiling done
# set mocassin path on mammouth
mopath=$HOME/hii3d
echo $mopath
rm -f $mopath/Transfer_to_mp2/leastSquare.bash
rm -f $mopath/Transfer_to_mp2/mocassin.bash
rm -f $mopath/Transfer_to_mp2/mocassinPlot.bash
rm -f $mopath/Transfer_to_mp2/cases-comparizon.txt
#
#
#
rm -f Ne3D*.txt
rm -fr $mopath/Transfer_to_mp2
mkdir $mopath/Transfer_to_mp2
# creer le repertoire de cas pour mocassin
mkdir $mopath/Transfer_to_mp2/mocassin_cases
list=`ls -1 *.fit` #create the list of all images available in the directory. #all the files must be in the local directory
n=0
for i in $list
do imstat $i > toto.tmp #for each image, exctract image size.
   read bidon bidon nx bidon ny bidon < toto.tmp
#   echo $i $nx $ny
   echo $nx $ny > geometry.tmp #extract size in geometry.
   imlist $i > $i.txt #extract pixels value.
   let n=n+1
done
echo $n >> geometry.tmp
for i in $list
do echo $i.txt >> geometry.tmp #add image name.txt in geometry.
done
#display $i         de-commenter pour retourner au mode interactif
#ATTENTION remettre les let sans commentaire lorsque tests finis.
#echo "Enter central star coordinate with the wheel button"          de-commenter pour retourner au mode interactif
#read xc yc         # de-commenter pour retourner au mode interactif
              #let xc=xc+1    EST_CE ENCORE UTILE
              #let yc=yc+1

#read rcirc          # de-commenter pour retourner au mode interactif
let xe=154         # commenter pour retourner au mode interactif
let ye=161         # commenter pour retourner au mode interactif
#let rcirc=70        # commenter pour retourner au mode interactif
ni=0
# ===============
# definition des cas a modeliser
angx="100."
#angz="1 10 20 30 40 50 60 70 80"
#distet="10 20 30 40 50 60 70 80"
#rcirc="40 50 60 70 80"
angz="10"
distet="40"
rcirc="80"
thickcstep="20"
ine="0."
ene="0."
tpix=6.E16   # taille d'un pixel en UNITES?
#
#
angx=`echo $angx`
angz=`echo $angz`
distet=`echo $distet`
rcirc=`echo $rcirc`
thickc=`echo $thickc`
ine=`echo $ine`
ene=`echo $ene`
echo $xe $ye > rond.in
echo $rcirc >> rond.in  # rayon externe des coquilles
# killall display
#

# debut des boucles sur tous les parametres
nn=0

for i in $angx
do 
   for j in $angz 
   do 
      for k in $distet
      do 
         for l in $rcirc
# m stands for thickness index
         do m=0
            while [ $m -lt $l ]
            do let m=m+thickcstep
               for n in $ine
               do 
                  for o in $ene
                  do #helping the random number to be random
                        sec=`date '+%S'| sed 's/^0*//'`
                        min=`date '+%M'| sed 's/^0*//'`
                        day=`date '+%d'| sed 's/^0*//'`
                        let ran=sec+min+day
                        echo $ran > random.tmp
                        echo "Input parameters for hii3d" > hii3d.input
                        echo $i >> hii3d.input
                        echo $j  >> hii3d.input
                        echo $k >>  hii3d.input
                        echo $l >>  hii3d.input
                        echo $m >>  hii3d.input
                        echo $n >> hii3d.input
                        echo $o >> hii3d.input
                        echo $tpix >> hii3d.input
                        echo "Running hii3d"
                        ./prog-hii3d
                        path="ax-"$i"_az-"$j"_de-"$k"_rc-"$l"_tc-"$m"_in-"$n"_en-"$o
                        echo $path
                        mkdir $mopath"/Transfer_to_mp2/mocassin_cases/"$path
                        cat densities.dat | sed 's/  / /g' | sed 's/  / /g' > densities.tmp
                        mkdir $mopath/"Transfer_to_mp2/mocassin_cases/"$path"/input"
                        mkdir $mopath/"Transfer_to_mp2/mocassin_cases/"$path"/output"
                        mv -f densities.tmp densities.dat 
                        cp -f densities.dat $mopath/"Transfer_to_mp2/mocassin_cases/"$path"/input"
                        cp -f SIIresol.pgm $mopath/"Transfer_to_mp2/mocassin_cases/"$path
                        cp -f NIIresol.pgm $mopath/"Transfer_to_mp2/mocassin_cases/"$path
                        cp -f Ne3D.txt $mopath/"Transfer_to_mp2/mocassin_cases/"$path
                        cp -f Te3D.txt $mopath/"Transfer_to_mp2/mocassin_cases/"$path
                        cp -f shape.txt $mopath/"Transfer_to_mp2/mocassin_cases/"$path
                        cp -f SIIratio3D.txt $mopath/"Transfer_to_mp2/mocassin_cases/"$path
                        cp -f NIIratio3D.txt $mopath/"Transfer_to_mp2/mocassin_cases/"$path
                        cp -f input.in $mopath/"Transfer_to_mp2/mocassin_cases/"$path"/input"
                        cp -f abun.in $mopath/"Transfer_to_mp2/mocassin_cases/"$path"/input"
# create the plot.in containing the spectral lines to simulate
                        echo "mono" > "Transfer_to_mp2/mocassin_cases/"$path"/input/plot.in"
                        echo "line 1         6583.   6583."  >> $mopath/"Transfer_to_mp2/mocassin_cases/"$path"/input/plot.in"
                        echo "line 2         5755.   5755."  >> $mopath/"Transfer_to_mp2/mocassin_cases/"$path"/input/plot.in"
                        echo "line 3         6716.   6716."  >> $mopath/"Transfer_to_mp2/mocassin_cases/"$path"/input/plot.in"
                        echo "line 4         6731.   6731."  >> $mopath/"Transfer_to_mp2/mocassin_cases/"$path"/input/plot.in"
#
# creation of the execute script
# file mocassin.bash
                        echo "cd " $mopath"/Transfer_to_mp2/mocassin_cases/"$path >> $mopath/Transfer_to_mp2/mocassin.bash
                        echo "qsub -W umask=0002 -q qwork@mp2 -l walltime=1:00:00,nodes=20 mpirun -np 20 mocassin" >>  $mopath/Transfer_to_mp2/mocassin.bash
                        echo "sleep 0.05"  >>  $mopath/Transfer_to_mp2/mocassin.bash
# file mocassinPlot.bash
                        echo "cd " $mopath"/Transfer_to_mp2/mocassin_cases/"$path >>  $mopath/Transfer_to_mp2/mocassinPlot.bash
                        echo "qsub -W umask=0002 -q qwork@mp2 -l walltime=1:00:00,nodes=1 mocassinPlot" >> $mopath/Transfer_to_mp2/mocassinPlot.bash
                        echo "sleep 0.05"  >> $mopath/Transfer_to_mp2/mocassinPlot.bash
# file leastSquare.bash
                        echo "cd "$mopath"/Transfer_to_mp2/mocassin_cases/"$path >> $mopath/Transfer_to_mp2/leastSquare.bash
                        echo "ln -s "$HOME"/hg/hii3d/prog-simul-ratio ." >> $mopath/Transfer_to_mp2/leastSquare.bash
                        echo "ln -s "$HOME"/hg/hii3d/prog-rms ." >> $mopath/Transfer_to_mp2/leastSquare.bash
                        echo "cp -f output/plot.out ." >> $mopath/Transfer_to_mp2/leastSquare.bash
                        echo "./prog-simul-ratio" >> $mopath/Transfer_to_mp2/leastSquare.bash
                        echo $path > $mopath"/Transfer_to_mp2/mocassin_cases/"$path/rms.tmp
                        echo "./prog-rms < rms.tmp" >> $mopath/Transfer_to_mp2/leastSquare.bash
                        echo "cat cases-comparizon.tmp >> "$mopath"/Transfer_to_mp2/cases-comparizon.txt" >> $mopath/Transfer_to_mp2/leastSquare.bash
                     done
                  done
               done
            done
         done
      done
   done





