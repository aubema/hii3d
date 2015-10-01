#!/bin/bash
rm -f *.peaks
rm -f ParamA.txt
echo "#Harmonique moyenne FWHM">ParamA.txt
list=`ls -1 DistA*`
ici=`pwd`
for i in $list
do echo ${i:5:2}
echo "analyse file:"$i
echo "@0<'"$ici"/"$i"'">fitcommand.txt
echo "@0: guess %f1 = Gaussian">>fitcommand.txt
echo "fit">>fitcommand.txt
echo "info formula" >>fitcommand.txt
echo "info %f1" >>fitcommand.txt
echo "info peaks > '/home/graphycs/test/$i.peaks'">>fitcommand.txt
cfityk<fitcommand.txt
grep Gaussian $i".peaks">toto.tmp
read toto toto center toto toto FWHM toto<toto.tmp
echo ${i:5:2} $center $FWHM >> ParamA.txt
done
