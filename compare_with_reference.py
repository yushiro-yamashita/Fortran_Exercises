import sympy as sym
from spb import plot
import pandas as pd

x = sym.Symbol("x")
y = sym.Function("y")
eq = sym.Eq(y(x).diff(x,2) + 3*y(x).diff(x,1) + 2*y(x), rhs=0)
ans = sym.dsolve(eq, ics={y(0):1, y(1):1})
ax = plot(ans.rhs, (x, 0, 1), {"color":"red", "linestyle":"--"}, size=(4,3), label="sympy").ax
ax.set_xlabel("x")
ax.set_xlim([0, 1])
ax.set_ylabel("y")
ax.set_ylim([1.0, 1.3])

df = pd.read_csv("result.csv")
ax.plot(df.x, df.y, label="lapack")

ax.legend(loc="upper left", bbox_to_anchor=(1.05, 1.0))
ax.get_figure().savefig("compare_with_reference.png", bbox_inches="tight")