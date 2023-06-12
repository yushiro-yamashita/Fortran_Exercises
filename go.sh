if ! type gfortran >/dev/null 2>&1; then
    sudo apt install gfortran
fi

gfortran -ffree-form 1_example.f90 -o example
./example