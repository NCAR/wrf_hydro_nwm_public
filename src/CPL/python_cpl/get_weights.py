import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from xgboost import XGBRegressor, Booster
import joblib

initialized = False
model = None

def init_ml_object():
    base_dir = '/glade/work/soren/src/wrf-hydro/lanl-ml/'
    model_name = 'model.pkl'
    # model_name = 'hello_world.pkl'
    model_path = base_dir + model_name
    print("Loading model:", model_name)

    model = None

    try:
        # Attempt joblib load (most common for sklearn-based XGBRegressor)
        model = joblib.load(model_path)
        print("Loaded using joblib:", type(model))
    except Exception as e:
        print("joblib failed, trying pickle...", e)
        with open(model_path, "rb") as f:
            model = pickle.load(f)
            print("Loaded using pickle:", type(model))

    # Check if model is an sklearn wrapper
    if isinstance(model, XGBRegressor):
        print("Model is an XGBRegressor with parameters:")
        # print(model.get_params())
        booster = model.get_booster()
    elif isinstance(model, Booster):
        print("Model is a raw XGBoost Booster")
        booster = model
    elif isinstance(model, dict) and 'model' in model:
        inner = model['model']
        if isinstance(inner, XGBRegressor):
            print("Dictionary contains XGBRegressor")
            # print(inner.get_params())
            # booster = inner.get_booster()
            if hasattr(inner, "get_booster"):
                booster = inner.get_booster()
                booster.save_model("model.json")  # safe portable format
            else:
                booster = None

        elif isinstance(inner, Booster):
            print("Dictionary contains raw Booster")
            booster = inner
        else:
            print("Dictionary contains unknown model type:", type(inner))
            booster = None
    else:
        print("Unknown model type:", type(model))
        booster = None

    if booster:
        # print("Booster best score:", booster.attr("best_score"))
        print("Weight:", booster.get_score(importance_type='weight'))
    else:
        print("No booster available")

    return model


def init_weights():
    global initialized, model
    print("Initializing Python")
    initialized = True
    model = init_ml_object()

def get_weights(weights):
    global initialized
    if (not initialized):
        init_weights()

    print("Python side received:", weights)
    return [w + 1 for w in weights]
