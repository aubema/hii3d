#!/bin/bash
#This is make-hii3d, for compilate the programs
# hii3d compiling script
#
#cd $HOME/svn/hii3d
echo "toto"
# echo "gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d"
# gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d
gfortran  prog-hii3d.f prog-SIINIIratio.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-ellipse.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o prog-hii3d
# gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d
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
let rcirc=70        # commenter pour retourner au mode interactif

echo $xe $ye > rond.in
echo $rcirc >> rond.in  # rayon externe des coquilles
killall display
#helping the random number to be random
t=`date '+%S'`
echo $t > random.tmp

echo "Running hii3d"
./prog-hii3d


