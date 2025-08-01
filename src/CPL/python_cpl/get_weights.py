import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from xgboost import XGBRegressor
import joblib

initialized = False

def init_ml_object():
    dir = '/glade/work/soren/src/wrf-hydro/lanl-ml/'
    f = dir+"model.pkl"
    modelpath = f

    try:
        # Attempt with joblib (most common for sklearn)
        model = joblib.load(f)
        print("Loaded using joblib:", type(model))
    except Exception as e:
        print("joblib failed, trying pickle...", e)
        with open(model_path, "rb") as f:
            model = pickle.load(f)
            print("Loaded using pickle:", type(model))

    # Inspect model attributes
    if hasattr(model, "get_params"):
        print("Model parameters:", model.get_params())
    else:
        print("Model does not have get_params (might not be sklearn).")


def init_weights():
    global initialized
    print("Initializing Python")
    initialized = True
    init_ml_object()

def get_weights(weights):
    global initialized
    if (not initialized):
        init_weights()

    print("Python side received:", weights)
    return [w + 1 for w in weights]
