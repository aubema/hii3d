#!/bin/bash
#This is make-hii3d, for compilate the programs
# hii3d compiling script
#
#cd $HOME/svn/hii3d
gfortran -mcmodel=medium -Wall -fcheck=all -g -fbacktrace -ffpe-trap=zero,overflow,underflow prog-hii3d-v1.f prog-SIINIIratio.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-dblshell.f prog-en-sigma.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o prog-hii3d

# gfortran -mcmodel=large prog-hii3d-v1.f prog-SIINIIratio.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-dblshell.f prog-en-sigma.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o prog-hii3d
#Compiling done
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
angx="90."
angz="90."
distet="30."
rcirc="10."
thickc="5."
ine="0."
ene="0."
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
   do for j in $angz 
      do for k in $distet
         do for l in $rcirc
            do for m in $thickc
               do for n in $ine
                  do for o in $ene
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
                        echo "Running hii3d"
                        ./prog-hii3d
                        file="Ne3D_angx-"$i"_angz-"$j"_distet-"$k"_rcric-"$l"_thickc-"$m"_ine-"$n"_ene-"$o".txt"
                        echo $file
                        mv Ne3D.txt $file
                     done
                  done
               done
            done
         done
      done
   done



