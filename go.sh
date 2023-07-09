./prepare.sh
gfortran script.f90 -o executable -llapack -lblas
./executable
rm -f executable
python compare_with_reference.py