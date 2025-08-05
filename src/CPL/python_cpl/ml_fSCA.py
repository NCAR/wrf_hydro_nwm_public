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


def init_model():
    global initialized, model
    print("Initializing XGBoost Model")
    if (not initialized):
        model = init_ml_object()
        initialized = True
    return model


def ml_fSCA(T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year,
                HGT, slope, aspect, lat, lon, nx, ny):
    model = init_model()

    # --- Define inputs ---
    # Was (time, lat, lon), but we are passing one time period at a time
    # Must be (lat, lon)
    dynamic_vars = ['T2D', 'LWDOWN', 'SWDOWN', 'U2D', 'V2D', 'day_of_year']
    # Must be (lat, lon)
    static_vars = ['HGT', 'slope', "aspect", "lat", "lon"]
    target_var = 'fSCA'


    # Flatten each variable (column-major to row-major conversion)
    npoints = nx * ny
    features = np.stack([
        T2D.reshape(-1),
        LWDOWN.reshape(-1),
        SWDOWN.reshape(-1),
        U2D.reshape(-1),
        V2D.reshape(-1),
        day_of_year.reshape(-1),
        HGT.reshape(-1),
        slope.reshape(-1),
        aspect.reshape(-1),
        lat.reshape(-1),
        lon.reshape(-1)
    ], axis=1)

    # Predict using the model
    predictions = model["model"].predict(features)

    # Reshape back to grid (nx, ny)
    fSCA = np.ascontiguousarray(predictions.reshape((nx, ny)), dtype=np.float64)

    print("Python's fSCA:\n", fSCA)
    return fSCA
