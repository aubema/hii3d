#!/bin/bash

#This is make-hii3d, for compilate the programs
# hii3d compiling script
#
#cd $HOME/svn/hii3d
echo "gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d"
gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d

# gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d
#Compiling done

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

echo "Which type of planetary nebula do you want to modelise?"
echo "1)Simple nebula"
echo "2)Nested bipolar nebula"
echo "3)Distinct bipolar nebula"
read typneb

if typneb=1 then 
echo "Enter central star coordinate"
read xc yc
echo "Enter the most external coordinate"
read xr yr
fi

if typneb=2 then
echo "Enter one ellipse extremity's coordinate (you should write two pair of number)"
read xr1 yr1 xr2 yr2
echo "Enter the other ellipse extremity's coordinate (you should write two pair of number) "
read xr3 yr3 xr4 yr4
let xc=
fi

if typneb=3 then
echo "Enter one ellipse extremity's coordinate (you should write one pair of number)"
read xr1 yr1
echo "Enter the other ellipse extremity's coordinate (you should write one pair of number)"
read xr2 yr2
echo "Enter central star coordinate"
read xc yc
fi

echo "Enter the dimensions of the images"
read imagx imagy

let rcirc=sqrt(sqrt((xr-xc)**2.+(yr-yc)**2.))


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
echo "Running hii3d"
./prog-hii3d

