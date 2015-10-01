#!/bin/bash
# hii3d compiling script
#
#cd $HOME/svn/hii3d
echo "gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d"
gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d

# gfortran  hii3d.f TNelines.f extrant2d.f  intersii.f TemperatureNII.f ellipse.f squaredata.f moyecart.f gaussienne.f histo.f writeIFrIT.f interp.f -o hii3d
