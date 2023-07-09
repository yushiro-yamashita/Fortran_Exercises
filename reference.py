import sympy as sym
# from sympy.plotting import plot
from matplotlib.pyplot import grid
from spb import plot

x = sym.Symbol("x")
y = sym.Function("y")
eq = sym.Eq(y(x).diff(x,2) + 3*y(x).diff(x,1) + 2*y(x), rhs=0)
ans = sym.dsolve(eq, ics={y(0):1, y(1):1})
ax = plot(ans.rhs, (x, 0, 1), size=(4,3)).ax
ax.set_xlabel("x")
ax.set_xlim([0, 1])
ax.set_ylabel("y")
ax.set_ylim([1.0, 1.3])
ax.get_figure().savefig("reference.png", bbox_inches="tight")
