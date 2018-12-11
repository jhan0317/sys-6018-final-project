# -*- coding: utf-8 -*-
"""
Created on Sat Dec  8 16:31:37 2018

@author: nicol
"""


import pandas as pd
import numpy as np
import re
from sklearn.cluster import KMeans
from scipy.spatial.distance import cdist
from sklearn.metrics import silhouette_score
from sklearn.preprocessing import MinMaxScaler
import matplotlib.pyplot as plt
import seaborn as sns


raw = pd.read_csv(r"C:\Users\nicol\OneDrive\Desktop\2018 Fall\SYS 6018\Final_Project\FROMOthers\cleaned_data_newest.csv") # (5460, 150)
raw.shape

raw.columns
data = raw.copy()


data['major'] = [re.findall('([\w]+)\s',course)[0] for course in raw['course']]
data['course_year']=[re.findall((r'\d+'),course)[0][0] for course in raw['course']]
data['course_year']=data["course_year"].astype(int)

# Drop GPA related information and name
list(data.columns).index('Course GPA')        #15
list(data.columns).index('Total')             #28

col_drop = list(data.columns[15:28])
col_drop.append('name')
col_drop.append('course')



cleaned_data = data.drop(col_drop, axis=1)         # (5460, 136)
cleaned_data.shape
data2=data
data2.drop(col_drop,axis=1,inplace=True)
cleaned_data2 = pd.get_dummies(data2)                # (5460, 268)
cleaned_data2.shape
#(5460, 273)


def get_corrs(df):
    col_correlations = df.corr()
    col_correlations.loc[:, :] = np.tril(col_correlations, k=-1)
    cor_pairs = col_correlations.stack()
    return cor_pairs.to_dict()

dict = get_corrs(cleaned_data2)

correlated = set()
for i in dict: 
    if dict[i]>0.5:
        correlated.add(i[0])
        
correlated
from sklearn.preprocessing import StandardScaler
def clean_dataset(df):
    assert isinstance(df, pd.DataFrame), "df needs to be a pd.DataFrame"
    df.dropna(inplace=True)
    indices_to_keep = ~df.isin([np.nan, np.inf, -np.inf]).any(1)
    return df[indices_to_keep].astype(np.float64)


raw.index
cleaned_dataset=clean_dataset(cleaned_data2)
scaler = StandardScaler()
scaler.fit(cleaned_dataset)
X = scaler.transform(cleaned_dataset)

#import numpy as np
#from sklearn.decomposition import PCA
#pca = PCA(n_components='mle',svd_solver='full',whiten=True)
#pca.fit(X)
#X=pca.transform(X)







#Fitting the PCA algorithm with our Data
      
from sklearn.manifold import TSNE


X_transformed = TSNE(n_components=2).fit_transform(X)



# Elbow curve and silhouette score
distortions = []
K = range(2,10)
for k in K:
    kmeans = KMeans(n_clusters=k).fit(X_transformed)
    kmeans.fit(X_transformed)
    labels = kmeans.labels_
    centers = kmeans.cluster_centers_
    score = silhouette_score (X_transformed, labels, metric='euclidean')
    distortions.append(sum(np.min(cdist(X_transformed, kmeans.cluster_centers_, 'euclidean'), axis=1)) / X_transformed.shape[0])
    print ("For n_clusters = {}, silhouette score is {}".format(k, score)) # The closer the silhouette score is to one, the better

# Plot the elbow curve
#plt.plot(K, distortions, 'bx-')
#plt.xlabel('k')
#plt.ylabel('Distortion')
#plt.title('The Elbow Method showing the optimal k')
#plt.show()

# Based on the elbow curve and silhouette score, we decided to use k = 3.

kmeans = KMeans(n_clusters=10, random_state=42).fit(X_transformed)

len(kmeans.labels_)
clean_dataset(raw)  #need to run it to clean 
raw['label'] = list(kmeans.labels_)

raw.index[raw['Course GPA']==0]

raw=raw.drop([1985,1986])

raw['Course GPA1']=raw['Course GPA'].apply(lambda x: np.log(x))


#,

fig, ax = plt.subplots()

colors = {0:'blue', 1:'pink',2:'green',3:'black',4:'orange',5:'purple',6:'brown',7:'yellow',8:'pink',9:'grey'}
fig.suptitle('Cluster Analysis Compared with Log GPAs ', fontsize=20)

ax.scatter(raw.index, raw['Course GPA1'], c=raw['label'].apply(lambda x: colors[x]), alpha=0.05)


plt.xlabel('Index', fontsize=18)
plt.ylabel('Log GPAs', fontsize=16)
fig.savefig('2clusters.jpg')

plt.show()



Compared with Log GPAs
    
    
