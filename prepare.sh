dpkg --status liblapack-dev | grep -q not-installed
if [ $? -eq 0 ]; then
    sudo apt install libblas-dev liblapack-dev
fi
if [[ ! $(pip list|grep "sympy-plot-backends") ]]; then
    pip install sympy_plot_backends
fi