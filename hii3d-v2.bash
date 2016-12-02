#!/bin/bash
#This is make-hii3d, for compilate the programs
# hii3d compiling script
#
#cd $HOME/svn/hii3d
gfortran -mcmodel=medium -Wall -fcheck=all -g -fbacktrace -ffpe-trap=zero,overflow,underflow prog-hii3d-v2.f prog-CleanSIINII.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-dblshell.f prog-en-sigma.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o prog-hii3d
gfortran prog-simul-ratio.f prog-extrant2d.f -o prog-simul-ratio
gfortran prog-rms.f prog-intrants2d.f -o prog-rms
gfortran prog-solution.f -o prog-solution
gfortran prog-SIINIIratio.f prog-extrant2d.f -o prog-SIINIIratio

# gfortran -mcmodel=large prog-hii3d-v2.f prog-SIINIIratio.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-dblshell.f prog-en-sigma.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o prog-hii3d
#Compiling done
# set execution path on mammouth. You will put the Transfer_to_mp2 there.
mopath=/mnt/parallel_scratch_mp2_wipe_on_august_2017/aube/aube_group
echo $mopath
rm -f Transfer_to_mp2/leastSquare.bash
rm -f Transfer_to_mp2/mocassin.bash
rm -f Transfer_to_mp2/mocassinPlot.bash
rm -f Transfer_to_mp2/cases-comparizon.txt
#
#
#
rm -f Ne3D*.txt
rm -fr Transfer_to_mp2
mkdir Transfer_to_mp2
# creer le repertoire de cas pour mocassin
mkdir Transfer_to_mp2/mocassin_cases
list=`ls -1 *.fit` #create the list of all images available in the directory. #all the files must be in the local directory
n=0
for i in $list
do imstat $i > toto.tmp #for each image, exctract image size.
   read bidon bidon nx bidon ny bidon < toto.tmp
   echo $nx $ny > geometry.tmp #extract size in geometry.
   imlist $i > $i.txt #extract pixels value.
   let n=n+1
done
echo $n >> geometry.tmp
for i in $list
do echo $i.txt >> geometry.tmp #add image name.txt in geometry.
done
let xe=154         # commenter pour retourner au mode interactif
let ye=161         # commenter pour retourner au mode interactif - coordonnees ny-position sur l'image
ni=0
# ===============
# definition des cas a modeliser
angx="110."
angz="1 10 20 30 40 50 60 70"
distet="1 10 20 30 40 50 60"
rcirc="30 40 50 60 70 80 90"
thickstep="10"
ine="0."
ene="0."
tpix=6.E16   # taille d'un pixel en UNITES?
#
#
angx=`echo $angx`
angz=`echo $angz`
distet=`echo $distet`
rcirc=`echo $rcirc`
thickstep=`echo $thickstep`
ine=`echo $ine`
ene=`echo $ene`
echo $xe $ye > rond.in
echo $rcirc >> rond.in  # rayon externe des coquilles
#
# creation des images de ratio full resolution
#
./prog-SIINIIratio
#
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
            while [ $m -lt $l ]               # on s'assure que l'epaisseur de la coquille ne depasse pas son rayon
            do let m=m+thickstep
               if [ $m -gt $l ]               # si le thickstep est superieur au rayon de la coquille on ne modelise qu'une sphere pleine
               then let m=l
               fi
               if [ $m -le $l ]
               then echo ""
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
                        path="ax-"$i"_az-"$j"_de-"$k"_rc-"$l"_tc-"$m"_in-"$n"_en-"$o
                        echo "case:" $path
                        echo "Running hii3d"
                        ./prog-hii3d

                        mkdir Transfer_to_mp2/mocassin_cases/$path
                        cat densities.dat | sed 's/  / /g' | sed 's/  / /g' > densities.tmp
                        mkdir Transfer_to_mp2/mocassin_cases/$path"/input"
                        mkdir Transfer_to_mp2/mocassin_cases/$path"/output"
                        mv -f densities.tmp densities.dat 
                        cp -f densities.dat Transfer_to_mp2/mocassin_cases/$path"/input"
                        cp -f SIIresol.pgm Transfer_to_mp2/mocassin_cases/$path
                        cp -f NIIresol.pgm Transfer_to_mp2/mocassin_cases/$path
                        cp -f Ne3D.txt Transfer_to_mp2/mocassin_cases/$path
                        cp -f Te3D.txt Transfer_to_mp2/mocassin_cases/$path
                        cp -f shape.txt Transfer_to_mp2/mocassin_cases/$path
                        cp -f SIIratio3D.txt Transfer_to_mp2/mocassin_cases/$path
                        cp -f NIIratio3D.txt Transfer_to_mp2/mocassin_cases/$path
                        cp -f input.in Transfer_to_mp2/mocassin_cases/$path"/input"
                        cp -f abun.in Transfer_to_mp2/mocassin_cases/$path"/input"
# create the plot.in containing the spectral lines to simulate
                        echo "mono" > Transfer_to_mp2/mocassin_cases/$path"/input/plot.in"
                        echo "line 1         6583.   6583."  >> Transfer_to_mp2/mocassin_cases/$path"/input/plot.in"
                        echo "line 2         5755.   5755."  >> Transfer_to_mp2/mocassin_cases/$path"/input/plot.in"
                        echo "line 3         6716.   6716."  >> Transfer_to_mp2/mocassin_cases/$path"/input/plot.in"
                        echo "line 4         6731.   6731."  >> Transfer_to_mp2/mocassin_cases/$path"/input/plot.in"
#
# creation of the execute script
# file mocassin.bash
                        cat $HOME/hg/hii3d/sub.pbs | sed 's/toto/mocassin/g' > submit.pbs
                        mv -f submit.pbs Transfer_to_mp2/mocassin_cases/$path
                        echo "cd " $mopath"/Transfer_to_mp2/mocassin_cases/"$path >> Transfer_to_mp2/mocassin.bash
                        echo "qsub ./submit.pbs" >>  Transfer_to_mp2/mocassin.bash
                        echo "sleep 0.05"  >>  Transfer_to_mp2/mocassin.bash
# file mocassinPlot.bash
                        cat $HOME/hg/hii3d/sub.pbs | sed 's/toto/mocassinPlot/g' > submitPlot.pbs
                        mv -f submitPlot.pbs Transfer_to_mp2/mocassin_cases/$path
                        echo "cd " $mopath"/Transfer_to_mp2/mocassin_cases/"$path >>  Transfer_to_mp2/mocassinPlot.bash
                        echo "qsub ./submitPlot.pbs" >> Transfer_to_mp2/mocassinPlot.bash
                        echo "sleep 0.05"  >> Transfer_to_mp2/mocassinPlot.bash
# file leastSquare.bash
                        echo "cd "$mopath"/Transfer_to_mp2/mocassin_cases/"$path >> Transfer_to_mp2/leastSquare.bash
                        echo "ln -s \$HOME/hg/hii3d/bin/prog-simul-ratio ." >> Transfer_to_mp2/leastSquare.bash
                        echo "ln -s \$HOME/hg/hii3d/bin/prog-rms ." >> Transfer_to_mp2/leastSquare.bash
                        echo "cp -f output/plot.out ." >> Transfer_to_mp2/leastSquare.bash
                        echo "./prog-simul-ratio" >> Transfer_to_mp2/leastSquare.bash
                        echo "echo \""$path" \"> rms.tmp" >> Transfer_to_mp2/leastSquare.bash
                        echo "./prog-rms < rms.tmp" >> Transfer_to_mp2/leastSquare.bash
                        echo "cat cases-comparizon.tmp >> "$mopath"/Transfer_to_mp2/cases-comparizon.txt" >> Transfer_to_mp2/leastSquare.bash
                        echo "rm -f "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/*.mod" >> Transfer_to_mp2/leastSquare.bash
                        echo "rm -fr "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/input" >> Transfer_to_mp2/leastSquare.bash
                        echo "rm -fr "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/output" >> Transfer_to_mp2/leastSquare.bash
                        echo "rm -f "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/*.gz" >> Transfer_to_mp2/leastSquare.bash
                        echo "rm -f "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/makefile*" >> Transfer_to_mp2/leastSquare.bash
                        echo "rm -f "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/*.dat" >> Transfer_to_mp2/leastSquare.bash
                        echo "rm -f "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/accessories" >> Transfer_to_mp2/leastSquare.bash 
                        echo "rm -f "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/benchmarks" >> Transfer_to_mp2/leastSquare.bash
                        echo "rm -f "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/examples" >> Transfer_to_mp2/leastSquare.bash
                        echo "rm -f "$mopath"/Transfer_to_mp2/mocassin_cases/"$path"/*ata" >> Transfer_to_mp2/leastSquare.bash
                   done
                  done
               else
                 echo "m="$m
               fi
            done
         done
      done
   done
done
