#include <Python.h>
#include <numpy/arrayobject.h>
#include <stdio.h>

static int python_initialized = 0;  // Global flag
static PyObject *pModule = NULL;
static PyObject *pFunc = NULL;

void py_ml_fSCA(double *fSCA,
                const double *T2D, const double *LWDOWN, const double *SWDOWN,
                const double *U2D, const double *V2D, const double *day_of_year,
                const double *HGT, const double *slope, const double *aspect,
                const double *lat, const double *lon,
                const int *nx, const int *ny) {
    int npoints = (*nx) * (*ny);
    int nx_i =(*nx);

    if (!python_initialized) {
        Py_Initialize();
        import_array1();
        // Add current directory to sys.path
        PyObject *sys_path = PySys_GetObject("path");
        PyList_Append(sys_path, PyUnicode_FromString("./"));
        PyList_Append(sys_path, PyUnicode_FromString("./bin"));

        // Import Python module and function
        PyObject *pName = PyUnicode_DecodeFSDefault("ml_fSCA");
        pModule = PyImport_Import(pName);
        Py_DECREF(pName);

        if (!pModule) {
            PyErr_Print();
            fprintf(stderr, "Failed to load ml_fSCA module\n");
            return;
        }
        pFunc = PyObject_GetAttrString(pModule, "ml_fSCA");
        if (!pFunc || !PyCallable_Check(pFunc)) {
            PyErr_Print();
            fprintf(stderr, "Cannot find Python function 'ml_fSCA'\n");
            return;
        }
        python_initialized = 1;
    }

    // Create NumPy arrays for each input (shape nx*ny)
    npy_intp dims[1] = {npoints};
    PyObject *arr_T2D = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)T2D);
    PyObject *arr_LWDOWN = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)LWDOWN);
    PyObject *arr_SWDOWN = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)SWDOWN);
    PyObject *arr_U2D = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)U2D);
    PyObject *arr_V2D = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)V2D);
    PyObject *arr_day_of_year = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)day_of_year);
    PyObject *arr_HGT = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)HGT);
    PyObject *arr_slope = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)slope);
    PyObject *arr_aspect = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)aspect);
    PyObject *arr_lat = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)lat);
    PyObject *arr_lon = PyArray_SimpleNewFromData(1, dims, NPY_DOUBLE, (void*)lon);

    // Prepare arguments tuple
    PyObject *pArgs = PyTuple_Pack(13,
        arr_T2D, arr_LWDOWN, arr_SWDOWN, arr_U2D, arr_V2D,
        arr_day_of_year, arr_HGT, arr_slope, arr_aspect,
        arr_lat, arr_lon,
        PyLong_FromLong(*nx), PyLong_FromLong(*ny));

    // Call Python function
    PyObject *pReturn = PyObject_CallObject(pFunc, pArgs);

    Py_DECREF(pArgs);
    Py_DECREF(arr_T2D); Py_DECREF(arr_LWDOWN); Py_DECREF(arr_SWDOWN);
    Py_DECREF(arr_U2D); Py_DECREF(arr_V2D); Py_DECREF(arr_day_of_year);
    Py_DECREF(arr_HGT); Py_DECREF(arr_slope); Py_DECREF(arr_aspect);
    Py_DECREF(arr_lat); Py_DECREF(arr_lon);

    if (pReturn && PyArray_Check(pReturn)) {
        PyArrayObject *np_ret = (PyArrayObject*)pReturn;
        double *data = (double*)PyArray_DATA(np_ret);

        printf("c = [");
        for (int i = 0; i < npoints; i++) {
            fSCA[i] = data[i];
            printf(" %f",  fSCA[i]);
            if ((i+1) % nx_i == 0) {
              printf("\n");
            }
        }
        printf("]\n");

            /* PyObject *item = PyList_GetItem(pReturn, i); */
            /* fSCA[i] = PyFloat_AsDouble(item); */

        /*         for (int i = 0; i < *n; i++) { */
/*             PyObject *item = PyList_GetItem(pReturn, i); */
/*             weights[i] = PyFloat_AsDouble(item); */
/*         } */


    } else {
        PyErr_Print();
        fprintf(stderr, "Python function did not return a NumPy array of floats\n");
    }
    Py_XDECREF(pReturn);
    // Do not finalize Python to keep interpreter alive for subsequent calls
}


/* #include <Python.h> */
/* #include <stdio.h> */

/* static int python_initialized = 0;  // Global flag */
/* static PyObject *pModule = NULL; */
/* static PyObject *pFunc = NULL; */

/* void py_ml_fSCA(double *weights, int *n) { */
/*     if (!python_initialized) { */
/*         Py_Initialize(); */

/*         // Add current directory to sys.path (so it finds get_weights.py) */
/*         PyObject *sys_path = PySys_GetObject("path"); */
/*         PyList_Append(sys_path, PyUnicode_FromString("./")); */
/*         PyList_Append(sys_path, PyUnicode_FromString("./bin")); */

/*         // Import Python module and function only once */
/*         PyObject *pName = PyUnicode_DecodeFSDefault("get_weights"); */
/*         pModule = PyImport_Import(pName); */
/*         Py_DECREF(pName); */

/*         if (pModule == NULL) { */
/*             PyErr_Print(); */
/*             fprintf(stderr, "Failed to load get_weights module\n"); */
/*             return; */
/*         } */

/*         pFunc = PyObject_GetAttrString(pModule, "get_weights"); */
/*         if (!pFunc || !PyCallable_Check(pFunc)) { */
/*             PyErr_Print(); */
/*             fprintf(stderr, "Cannot find Python function 'get_weights'\n"); */
/*             return; */
/*         } */

/*         python_initialized = 1; */
/*     } */

/*     printf("-- in C-Land\n"); */

/*     // Convert weights array to Python list */
/*     PyObject *pyList = PyList_New(*n); */
/*     for (int i = 0; i < *n; i++) { */
/*         PyList_SetItem(pyList, i, PyFloat_FromDouble(weights[i])); */
/*     } */

/*     // Call Python function */
/*     PyObject *pArgs = PyTuple_Pack(1, pyList); */
/*     PyObject *pReturn = PyObject_CallObject(pFunc, pArgs); */
/*     Py_DECREF(pArgs); */
/*     Py_DECREF(pyList); */

/*     if (pReturn && PyList_Check(pReturn)) { */
/*         for (int i = 0; i < *n; i++) { */
/*             PyObject *item = PyList_GetItem(pReturn, i); */
/*             weights[i] = PyFloat_AsDouble(item); */
/*         } */
/*     } else { */
/*         PyErr_Print(); */
/*         fprintf(stderr, "Python function did not return a list\n"); */
/*     } */
/*     Py_XDECREF(pReturn); */

/*     // Do NOT call Py_Finalize() here */
/* } */
