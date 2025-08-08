import joblib
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from xgboost import XGBRegressor, Booster
import xarray as xr

initialized = False
model = None
slope = None
aspect = None
lat_s = None
lat_a = None
lon_s = None
lon_a = None


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

def init_aspect():
    aspect_f = '/glade/campaign/ral/hap/enzminger/snowmodel/topo_vege/domain_UCRB/aspect.nc'
    ds = xr.open_dataset(aspect_f)
    return ds["aspect"].values, ds["lat"].values, ds["lon"].values

def init_slope():
    slope_f = '/glade/campaign/ral/hap/enzminger/snowmodel/topo_vege/domain_UCRB/slope.nc'
    ds = xr.open_dataset(slope_f)
    return ds["slope"].values, ds["lat"].values, ds["lon"].values

def init_model():
    global initialized, model, aspect, slope, lat_a, lat_s, lon_a, lon_s
    print("Initializing XGBoost Model")
    if (not initialized):
        model = init_ml_object()
        aspect, lat_a, lon_a = init_aspect()
        slope, lat_s, lon_s = init_slope()
        initialized = True
    return model

def get_aspect(lat, lon, tol=1e-12):
    # Find the cell where both lat & lon match (within tol)
    mask = np.isclose(lat_a, lat, atol=tol) & np.isclose(lon_a, lon, atol=tol)
    hits = np.argwhere(mask)

    if hits.size == 0:
        raise ValueError("No exact (lat, lon) match found within tolerance.")
    i, j = map(int, hits[0])        # (south_north, west_east)

    val = aspect[i, j]
    if np.isnan(val):
        raise ValueError("Matched cell has NaN aspect.")
    return val

def get_slope(lat, lon, tol=1e-12):
    # Find the cell where both lat & lon match (within tol)
    mask = np.isclose(lat_s, lat, atol=tol) & np.isclose(lon_s, lon, atol=tol)
    hits = np.argwhere(mask)

    if hits.size == 0:
        raise ValueError("No exact (lat, lon) match found within tolerance.")
    i, j = map(int, hits[0])        # (south_north, west_east)

    val = slope[i, j]
    if np.isnan(val):
        raise ValueError("Matched cell has NaN aspect.")
    return val

def ml_fSCA_scalar(T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year,
                HGT, slope, aspect, lat, lon):
    model = init_model()

    # --- Define inputs ---
    # Was (time, lat, lon), but we are passing one time period at a time
    # Must be (lat, lon)
    dynamic_vars = ['T2D', 'LWDOWN', 'SWDOWN', 'U2D', 'V2D', 'day_of_year']
    # Must be (lat, lon)
    static_vars = ['HGT', 'slope', "aspect", "lat", "lon"]
    target_var = 'fSCA'

    slope = get_slope(lat, lon)
    aspect = get_aspect(lat, lon)

    # Flatten each variable (column-major to row-major conversion)
    features = np.array([[
        T2D,
        LWDOWN,
        SWDOWN,
        U2D,
        V2D,
        day_of_year,
        HGT,
        slope,
        aspect,
        lat,
        lon,
    ]], dtype=np.float64)  # shape (1, 11)

    # Predict using the model
    predictions = model["model"].predict(features)

    fSCA = float(predictions[0])

    print("Python's fSCA:\n", fSCA)
    return fSCA


def ml_fSCA_array(T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year,
                HGT, slope, aspect, lat, lon, nx, ny):
    model = init_model()

    # --- Define inputs ---
    # Was (time, lat, lon), but we are passing one time period at a time
    # Must be (lat, lon)
    dynamic_vars = ['T2D', 'LWDOWN', 'SWDOWN', 'U2D', 'V2D', 'day_of_year']
    # Must be (lat, lon)
    static_vars = ['HGT', 'slope', "aspect", "lat", "lon"]
    target_var = 'fSCA'


    # slope = get_slope(lat, lon)
    # aspect = get_aspect(lat, lon)
    error_s ="get_slope and get_aspect need to be implemented for arrays"
    raise ValueError(error_s)

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
