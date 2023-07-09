import pandas as pd

df = pd.read_csv("data.csv", index_col=0)
ax = df.plot(figsize=(4,3))
ax.set_xlim([0, 1])
ax.set_ylim([1, 1.3])
ax.grid()
ax.get_figure().savefig("data.png", bbox_inches="tight")