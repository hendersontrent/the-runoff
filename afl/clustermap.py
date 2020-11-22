#------------------------------------------
# This script sets out to produce a
# clustermap of the metrics that feed the
# RandomForest winner vs loser algorithm
#
# NOTE: This script requires setup.R and
# afl/clustermap-data.R to have been run
# first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 22 November 2020
#------------------------------------------

import pandas as pd
import numpy as np
from pathlib import Path
import matplotlib.pyplot as plt
import seaborn as sns

#------------------------ Set up seaborn palette -------------------

colours = colours = ['#FF686B', '#93E1D8']
sns.set_palette(sns.color_palette(colours))

#------------------------ Preprocessing ----------------------------

# Read in dataset

base_path = Path(__file__).parent
file_path = (base_path / "../afl/data/clustermap_data.csv").resolve()

with open(file_path) as f:
    d = pd.read_csv(f)

#------------------------ Data visualisation -----------------------

# Compute correlation matrix

corr = d.corr()

# Generate a mask for the upper triangle to make it blank

mask = np.triu(np.ones_like(corr, dtype = bool))

# Set up the matplotlib figure

f, ax = plt.subplots(figsize=(11, 9))

# Generate a custom diverging colormap

cmap = sns.diverging_palette(230, 20, as_cmap = True)

# Draw the heatmap with the mask and correct aspect ratio

sns.heatmap(corr, mask = mask, vmax = .3, center = 0,
            square = True, linewidths = .5, cbar_kws = {"shrink": .5})

plt.savefig('/Users/trenthenderson/Documents/Git/the-runoff/afl/output/clustermap.png', dpi = 600)
