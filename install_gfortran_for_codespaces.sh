if ! type gfortran >/dev/null 2>&1; then
    sudo apt update
    sudo apt install -y gfortran
fi
