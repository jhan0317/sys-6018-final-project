# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
import re
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from sklearn.ensemble import RandomForestRegressor

raw = pd.read_csv("cleaned_data_dummies.csv") # (5460, 150)
data = raw.copy()

# Extract the major from course to see if GPA has relationship with major
data['major'] = [re.findall('([\w]+)\s',course)[0] for course in raw['course']]  # (5460, 150)
data.to_csv("cleaned_data_dummies_with_major.csv", index=False)

# It is faster to perform on array
Y = np.array(data['Course GPA'])

# Random Forest with all columns
clf = RandomForestRegressor(n_estimators=100, max_depth=10,random_state=42)
cols_drop = list(data.columns[15:29])  # Drop GPA-related columns
cleaned_data = pd.get_dummies(data.drop(cols_drop, axis=1))
arr = np.array(cleaned_data)
scaler = MinMaxScaler()
scaler.fit(arr)
X = scaler.transform(arr)
clf.fit(X, Y)
feats = {} # a dict to hold feature_name: feature_importance
for feature, importance in zip(cleaned_data.columns, clf.feature_importances_):
    feats[feature] = importance

importances = pd.DataFrame.from_dict(feats, orient='index').rename(columns={0: 'Gini-importance'})
importances.sort_values(by='Gini-importance', ascending=True)[-10:].plot(kind='barh', title="Feature Importance of all columns")

# Random Forest with text-related information
cleaned_data = data.iloc[:,29:148]
arr = np.array(cleaned_data)
scaler.fit(arr)
X = scaler.transform(arr)
clf.fit(X, Y)
feats = {} # a dict to hold feature_name: feature_importance
for feature, importance in zip(cleaned_data.columns, clf.feature_importances_):
    feats[feature] = importance

importances = pd.DataFrame.from_dict(feats, orient='index').rename(columns={0: 'Gini-importance'})
importances.sort_values(by='Gini-importance', ascending=True)[-10:].plot(kind='barh', title="Feature Importance of text-related columns")

###################### Exploration of GPA and 'she' ###########################
she_gpa_major = data.groupby('major')['she','Course GPA'].mean()
she_gpa_major = she_gpa_major.sort_values(by='Course GPA', ascending=False)
plt.subplots(figsize=(15,5))
ax1 = plt.subplot(111)
plt.plot(np.arange(1,134),she_gpa_major['Course GPA'], color='orange', label='GPA')
ax2 = ax1.twinx()
ax2.plot(np.arange(1,134),she_gpa_major.she, label='tfidf of she')
ax1.set_xlabel('course index')
ax1.set_ylabel('GPA')
ax2.set_ylabel("tfidf of 'she'")
plt.title("GPA vs tfidf of 'she' by major")
plt.legend()

# Majors of top 10 GPA
she_gpa_major.index[:10]
#['MUBD', 'MUEN', 'BIMS', 'HEBR', 'ENPW', 'GNUR', 'EDIS', 'GDS', 'PORT','RELH']

# Majors of top 10 values of tfidf of 'she'
she_gpa_major.sort_values(by='she', ascending=False).index[:10]
# ['PORT', 'LATI', 'FREN', 'JPTR', 'GSGS', 'ENLT', 'ITTR', 'ENAM', 'ARTS','PHS']

