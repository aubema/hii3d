#!/bin/bash
# hii3d compiling script
#
#cd $HOME/svn/hii3d
echo "gfortran hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f rond.f  circledata.f -o lines2TNe"
gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f circle.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d

# gfortran  stat.f extractline.f extractcol.f extractrayon.f dephasage.f intrants2d.f tribubble.f fourier.f -o statistik
