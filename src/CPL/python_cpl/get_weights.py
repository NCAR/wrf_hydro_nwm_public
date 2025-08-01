import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from xgboost import XGBRegressor
import joblib

initialized = False

def init_weights():
    global initialized
    print("Initializing Weights")
    initialized = True

def get_weights(weights):
    global initialized
    if (not initialized):
        init_weights()

    print("Python side received:", weights)
    return [w + 1 for w in weights]
