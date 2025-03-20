# -*- coding: utf-8 -*-
"""
Created on Sun Dec 31 12:01:47 2023

@author: davyd
"""

import pandas as pd 
df = pd.read_csv("data_clean_feature_engineering.csv",index_col='Unnamed: 0')

df_als = df[df["state_Alaska"]==1]


# Séparation des variables indépendantes (X) et de la variable cible (y)
# X = df.drop(["mag","significance","state_Alaska","depth","longitude","latitude","state_California","state_Other"], axis=1)
y = df_als["mag"]
X = df_als[["time"]]

from xgboost import XGBRegressor
from sklearn.model_selection import GridSearchCV, RandomizedSearchCV, train_test_split

X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=0.1)
    
xgb = XGBRegressor()

# Entraînez votre modèle en spécifiant les colonnes catégorielles
xgb.fit(X_train, y_train)


from sklearn.metrics import mean_squared_error, mean_absolute_error
import numpy as np

# Faire des prédictions sur l'ensemble de test
y_pred = xgb.predict(X_test)

# Calculer la RMSE
rmse = np.sqrt(mean_squared_error(np.exp(y_test), np.exp(y_pred)))
print(f'RMSE: {rmse}')

# Calculer le MAPE
# mape = np.mean(np.abs(((y_test) - (y_pred)) / (y_test))) * 100
# print(f'MAPE: {mape}%')

mape = np.mean(np.abs((np.exp(y_test) - np.exp(y_pred)) / np.exp(y_test))) * 100
print(f'MAPE: {mape}%')

