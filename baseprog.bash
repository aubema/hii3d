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
#ATTENTION remettre les let sans commentaire lorsque tests finis.
echo "Enter central star coordinate with the wheel button"
#read xc yc
#let xc=xc+1
#let yc=yc+1
let xc=154
let yc=161
echo $xc $yc > rond.in
echo "Enter radius coordinate with the wheel button"
#read xr yr
#let xr=xr+1
#let yr=yr+1
let xr=211
let yr=95
echo $xr $yr >> rond.in
killall display
./hii3d
echo "set style data lines" > sigmat.plot
echo "plot 'sigmat.txt'" >> sigmat.plot
# echo "replot 'circle.txt'" >> sigmat.plot
#gnuplot -p < sigmat.plot
echo "set style data lines" > sigman.plot
echo "plot 'sigman.txt'" >> sigman.plot
#gnuplot -p < sigman.plot
echo "set style data lines" > moyt.plot
echo "plot 'moyt.txt'" >> moyt.plot
#gnuplot -p < moyt.plot
echo "set style data lines" > moyn.plot
echo "plot 'moyn.txt'" >> moyn.plot
#gnuplot -p < moyn.plot

