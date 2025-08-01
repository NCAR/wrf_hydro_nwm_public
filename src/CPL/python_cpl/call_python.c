#include <Python.h>
#include <stdio.h>

static int python_initialized = 0;  // Global flag
static PyObject *pModule = NULL;
static PyObject *pFunc = NULL;

void call_python(double *weights, int *n) {
    if (!python_initialized) {
        Py_Initialize();

        // Add current directory to sys.path (so it finds get_weights.py)
        PyObject *sys_path = PySys_GetObject("path");
        PyList_Append(sys_path, PyUnicode_FromString("./"));
        PyList_Append(sys_path, PyUnicode_FromString("./bin"));

        // Import Python module and function only once
        PyObject *pName = PyUnicode_DecodeFSDefault("get_weights");
        pModule = PyImport_Import(pName);
        Py_DECREF(pName);

        if (pModule == NULL) {
            PyErr_Print();
            fprintf(stderr, "Failed to load get_weights module\n");
            return;
        }

        pFunc = PyObject_GetAttrString(pModule, "get_weights");
        if (!pFunc || !PyCallable_Check(pFunc)) {
            PyErr_Print();
            fprintf(stderr, "Cannot find Python function 'get_weights'\n");
            return;
        }

        python_initialized = 1;
    }

    printf("-- in C-Land\n");

    // Convert weights array to Python list
    PyObject *pyList = PyList_New(*n);
    for (int i = 0; i < *n; i++) {
        PyList_SetItem(pyList, i, PyFloat_FromDouble(weights[i]));
    }

    // Call Python function
    PyObject *pArgs = PyTuple_Pack(1, pyList);
    PyObject *pReturn = PyObject_CallObject(pFunc, pArgs);
    Py_DECREF(pArgs);
    Py_DECREF(pyList);

    if (pReturn && PyList_Check(pReturn)) {
        for (int i = 0; i < *n; i++) {
            PyObject *item = PyList_GetItem(pReturn, i);
            weights[i] = PyFloat_AsDouble(item);
        }
    } else {
        PyErr_Print();
        fprintf(stderr, "Python function did not return a list\n");
    }
    Py_XDECREF(pReturn);

    // Do NOT call Py_Finalize() here
}
