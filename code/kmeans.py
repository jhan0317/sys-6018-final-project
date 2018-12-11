# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
import re
from sklearn.cluster import KMeans
from scipy.spatial.distance import cdist
from sklearn.metrics import silhouette_score
from sklearn.preprocessing import MinMaxScaler
import matplotlib.pyplot as plt

raw = pd.read_csv("cleaned_data_dummies.csv") # (5460, 150)
data = raw.copy()

# Extract the major from course
data['major'] = [re.findall('([\w]+)\s',course)[0] for course in raw['course']]

# Drop GPA related information and name
list(data.columns).index('Course GPA')        #15
list(data.columns).index('Total')             #28
col_drop = list(data.columns[15:28])
col_drop.append('name')
col_drop.append('course')

cleaned_data = data.drop(col_drop, axis=1)         # (5460, 136)
cleaned_data = pd.get_dummies(data)                # (5460, 268)

arr = np.array(cleaned_data)
scaler = MinMaxScaler()
scaler.fit(arr)
X = scaler.transform(arr)

# Elbow curve and silhouette score
distortions = []
K = range(2,10)
for k in K:
    kmeans = KMeans(n_clusters=k).fit(X)
    kmeans.fit(X)
    labels = kmeans.labels_
    centers = kmeans.cluster_centers_
    score = silhouette_score (X, labels, metric='euclidean')
    distortions.append(sum(np.min(cdist(X, kmeans.cluster_centers_, 'euclidean'), axis=1)) / X.shape[0])
    print ("For n_clusters = {}, silhouette score is {}".format(k, score)) # The closer the silhouette score is to one, the better

# Plot the elbow curve
plt.plot(K, distortions, 'bx-')
plt.xlabel('k')
plt.ylabel('Distortion')
plt.title('The Elbow Method showing the optimal k')
plt.show()

# Based on the elbow curve and silhouette score, we decided to use k = 3.

kmeans = KMeans(n_clusters=3, random_state=42).fit(X)
data['label'] = list(kmeans.labels_)

groups = data.groupby('label')
fig, ax=plt.subplots()
ax.margins(0.05)
for name, group in groups:
    ax.plot(group['Course GPA'], marker='o', markersize=4, linestyle='', label=name)
    ax.set_title("GPA with cluster")
    ax.set_ylabel('GPA')
    ax.set_ylim([2, 4])
    ax.legend()

# Unfortunately, it seems we cannot find any pattern in the clusters and GPA

    
