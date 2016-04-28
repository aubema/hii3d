#!/bin/bash
#This is make-hii3d, for compilate the programs
# hii3d compiling script
#
#cd $HOME/svn/hii3d
gfortran -mcmodel=medium -Wall -fcheck=all -g -fbacktrace -ffpe-trap=zero,overflow,underflow prog-hii3d-v1.f prog-SIINIIratio.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-dblshell.f prog-en-sigma.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o prog-hii3d
gfortran prog-simul-ratio.f prog-extrant2d.f -o prog-simul-ratio

# gfortran -mcmodel=large prog-hii3d-v1.f prog-SIINIIratio.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-dblshell.f prog-en-sigma.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o prog-hii3d
#Compiling done
rm -f Ne3D*.txt
rm -f mocassin_cases_list
rm -fr Transfer_to_mp2
mkdir Transfer_to_mp2
# creer le repertoire de cas pour mocassin
rm -fr Transfer_to_mp2/mocassin_cases
mkdir Transfer_to_mp2/mocassin_cases
mkdir Transfer_to_mp2/input
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
angx="135."
#angz="1 10 20 30 40 50 60 70 80"
#distet="10 20 30 40 50 60 70 80"
#rcirc="40 50 60 70 80"
angz="10"
distet="40"
rcirc="80"
thickcstep="80"
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
                        file="Ne3D_angx-"$i"_angz-"$j"_distet-"$k"_rcric-"$l"_thickc-"$m"_ine-"$n"_ene-"$o".txt"
                        mocfile="Ne3D_angx-"$i"_angz-"$j"_distet-"$k"_rcric-"$l"_thickc-"$m"_ine-"$n"_ene-"$o".dat"
                        echo $file
                        mv Ne3D.txt $file
                        cat densities.dat | sed 's/  / /g' | sed 's/  / /g' > densities.tmp
                        mv -f densities.tmp densities.dat 
                        mv -f densities.dat Transfer_to_mp2/mocassin_cases/$mocfile
                        echo $mocfile >> Transfer_to_mp2/mocassin_cases_list
                     done
                  done
               done
            done
         done
      done
   done
   cat input.in | sed s'/densities.dat"/~~density~~"/g' | sed s'/input/$HOME\/mocassin_run/g'> input.tmp
   mv input.tmp Transfer_to_mp2/input/input.in
# creation of bqsubmit.dat files
#
# Nom du paquet
echo "batchName = scanning_mocassin_solutions" > Transfer_to_mp2/bqsubmit.dat
# Fichier de soumission des tâches
echo "templateFiles = input.in" >> Transfer_to_mp2/bqsubmit.dat
# Lien pour accéder aux fichiers d'entrée
#echo "linkFiles = mocassin_cases" >> Transfer_to_mp2/bqsubmit.dat
# Commande à exécuter sur le nœud de calcul
echo "command = /home/aube_group/hii3d/openmpi/bin/mpirun -np 20 mocassin" >> Transfer_to_mp2/bqsubmit.dat
# Combien de tâches par nœud en même temps
# runJobsPerNode = 2
# Combien de tâches seront soumises sur un même nœud
# accJobsPerNode = 4
# Ressources requises par chaque groupe de quatre tâches.
echo "submitOptions = -q qwork@mp2 -l walltime=1:00:00,nodes=20" >> Transfer_to_mp2/bqsubmit.dat
# Liste des paramètres pour chaque tâche
echo "param1 = density = load mocassin_cases_list" >> Transfer_to_mp2/bqsubmit.dat
# Combien de groupe de quatre tâches peuvent être exécutés en même temps.
# concurrentJobs = 100
mv -f abun.in Transfer_to_mp2/input
# create the plot.in containing the spectral lines to simulate
echo "mono" > Transfer_to_mp2/input/plot.in
echo "line 1         6583.   6583."  >> Transfer_to_mp2/input/plot.in
echo "line 2         5755.   5755."  >> Transfer_to_mp2/input/plot.in
echo "line 3         6716.   6716."  >> Transfer_to_mp2/input/plot.in
echo "line 4         6731.   6731."  >> Transfer_to_mp2/input/plot.in




