#------------------------------------------
# This script sets out to produce a
# classification algorithm for winners vs
# losers
#
# NOTE: This script requires setup.R and
# afl/winner-loser-data.R to have been run
# first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 21 November 2020
#------------------------------------------

import pandas as pd
import numpy as np
from pathlib import Path
import pydotplus
import collections
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import export_graphviz
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score

#------------------------ Preprocessing ----------------------------

# Read in dataset

base_path = Path(__file__).parent
file_path = (base_path / "../afl/data/winner_loser_data.csv").resolve()

with open(file_path) as f:
    d = pd.read_csv(f)

#--------------------
# Feature engineering
#--------------------

# Matrix of predictors

X = d.drop('did_i_win', axis = 1)
feature_list = list(X.columns)
X = np.array(X)

# Outcome variable
y = d['did_i_win']
y = y.to_frame(name = 'did_i_win')
y = np.array(y)

# Scale predictors for consistency

sc = StandardScaler()
X = sc.fit_transform(X)

# Make test and training data splits

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 0)

#------------------------ Modelling --------------------------------

model = RandomForestClassifier(n_estimators = 100)
model.fit(X_train, y_train.ravel())

#------------------------ Model accuracy and validation ------------

# Predictions

y_pred = model.predict(X_test)
print(accuracy_score(y_test, y_pred))

#--------------------
# Variable importance
#--------------------

# Calculations

X_names = d.drop('did_i_win', axis = 1)
raw_importance = pd.Series(model.feature_importances_, index = X_names.columns)
raw_importance = pd.DataFrame(data = raw_importance)
raw_importance.reset_index(inplace = True)
importance_table = raw_importance.rename(columns = {'index': 'Variable'})
importance_table = importance_table.rename(columns = { importance_table.columns[1]: "Importance" })
importance_table = importance_table.sort_values(by = ['Importance'], ascending = False)

# Clean up variable names

importance_table['Variable'] = importance_table.Variable.str.title()
importance_table['Variable'] = importance_table.Variable.replace("_", " ")

# DataFrame to HTML table for website

def table_to_runoff_pal(val):
    color = '#FF686B' if val < 0.1 else '#93E1D8'
    return 'color: %s' % color

importance_html = importance_table.style.applymap(table_to_runoff_pal, subset = ['Importance']).render()
with open('/Users/trenthenderson/Documents/Git/the-runoff/afl/output/variable-importance.html', 'w') as f:
    f.write(importance_html)

#------------------------ Data visualisation -----------------------

# Build a decision tree plot

tree_to_plot = model.estimators_[5]
tree_dot = export_graphviz(tree_to_plot, out_file = None,
                           feature_names = feature_list, class_names = ['Lose', 'Win'],
                           rounded = True,filled = True,precision = 2)

# Pull the dot file into a graph format

graph = pydotplus.graph_from_dot_data(tree_dot)

# Set up The Runoff colour palette

colours = ('#FF686B', '#93E1D8')
edges = collections.defaultdict(list)

for edge in graph.get_edge_list():
    edges[edge.get_source()].append(int(edge.get_destination()))

for edge in edges:
    edges[edge].sort()
    for i in range(2):
        dest = graph.get_node(str(edges[edge][i]))[0]
        dest.set_fillcolor(colours[i])

# Render as PNG

graph.write_png("/Users/trenthenderson/Documents/Git/the-runoff/afl/output/winner-loser-tree-plot.png")
