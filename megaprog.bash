#!/bin/bash
list=`ls -1 *.fit` #create the list of all images available in the directory. #all the files must be in the local directory
n=0
for i in $list
do imstat $i > toto.tmp #for each image, exctract image size.
   read bidon bidon nx bidon ny bidon < toto.tmp
   echo $i $nx $ny
   echo $nx $ny > geometry.tmp #extract size in geometry.
   imlist $i > $i.txt #extract pixels value.
   let n=n+1
done
echo $n >> geometry.tmp
for i in $list
do echo $i.txt >> geometry.tmp #add image name.txt in geometry.
done
display $i &
echo "Enter central star coordinate with the wheel button"
read xc yc
let xc=xc+1
let yc=yc+1
echo $xc $yc > rond.in
echo "Enter radius coordinate with the wheel button"
read xr yr
let xr=xr+1
let yr=yr+1
echo $xr $yr >> rond.in
#on est pas sur de pkill
killall display
./lines2TNe
./statistik
#bash fitGaussian.bash
#donnesgaus4
#random

#        echo $nlines > nlines.tmp
#        echo $quad >> nlines.tmp
#	echo $gain >> nlines.tmp
#	echo $oorigin >> nlines.tmp
#        echo $list > imagetxt.tmp
