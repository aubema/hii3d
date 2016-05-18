rm -fr bin
mkdir bin
gfortran -mcmodel=medium -Wall -fcheck=all -g -fbacktrace -ffpe-trap=zero,overflow,underflow prog-hii3d-v2.f prog-CleanSIINII.f prog-extrant2d.f  prog-interSII.f prog-temperatureNII.f prog-dblshell.f prog-en-sigma.f prog-squaredata.f prog-moysigma.f prog-gaussienne.f prog-writeIFrIT.f -o bin/prog-hii3d
gfortran prog-simul-ratio.f prog-extrant2d.f -o bin/prog-simul-ratio
gfortran prog-rms.f prog-intrants2d.f -o bin/prog-rms
gfortran prog-solution.f -o bin/prog-solution
