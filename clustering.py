# import libs
import pandas as pd
from sklearn.cluster import KMeans
import numpy as np

# read in data
csvfile = pd.read_csv("C:/Users/Kath/Documents/IAV_Outbreak_Analysis/condensed_candidates.csv", header=0, encoding='latin-1')
X = csvfile.iloc[:,-3:-1]

# cluster
kmeans = KMeans(n_clusters=9, random_state=0, n_init="auto").fit(X, sample_weight=csvfile.iloc[:,-1])
print(kmeans.cluster_centers_)

# save as csv for export to GIS
df = pd.DataFrame(kmeans.cluster_centers_)
df.columns = ['Latitude', 'Longitude']
df.to_csv("C:/Users/Kath/Documents/IAV_Outbreak_Analysis/9_km.csv", header=True, index=False)