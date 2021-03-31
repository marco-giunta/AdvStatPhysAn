import pandas as pd
import os

if not os.path.exists("laghi_Scozia.txt") :
    s = """Loch Volume Area Length Max-depth Mean-depth
Loch Ness 7.45 56 39 230 132
Loch Lomond 2.6 71 36 190 37
Loch Morar 2.3 27 18.8 310 87
Loch Tay 1.6 26.4 23 150 60.6
Loch Awe 1.2 39 41 94 32
Loch Maree 1.09 28.6 20 114 38
Loch Ericht 1.08 18.6 23 156 57.6
Loch Lochy 1.07 16 16 162 70
Loch Rannoch 0.97 19 15.7 134 51
Loch Shiel 0.79 19.5 28 128 40
Loch Katrine 0.77 12.4 12.9 151 43.4
Loch Arkaig 0.75 16 19.3 109 46.5
Loch Shin 0.35 22.5 27.8 49 15.5"""

    with open("laghi_Scozia.txt","w") as fh :
        fh.write(s)

df = pd.read_csv("laghi_Scozia.txt", sep=" ")
df = df.reset_index(drop=True)

for i in range(1,df.shape[1]) :
    df1 = df[[df.columns[0],df.columns[i]]]
    df1.to_csv(df.columns[i]+".csv", index=None)