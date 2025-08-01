/* #include <stdio.h> */
/* void call_python(double *weights, int *n) { */
/*     printf("C side: n = %d\n", *n); */

/*     printf("C side: weights = ["); */
/*     for (int i = 0; i < *n; i++) { */
/*         printf("%f", weights[i]); */
/*         if (i < *n - 1) printf(", "); */
/*     } */
/*     printf("]\n"); */

/*     // Example modification */
/*     for (int i = 0; i < *n; i++) { */
/*         weights[i] += 1.0;  // increment each element */
/*     } */
/* } */


#include <Python.h>
#include <stdio.h>

void call_python(double *weights, int *n) {
  printf("-- in C-Land\n");
  // Initialize Python (do this only once globally in real apps)
  Py_Initialize();

  PyObject *sys_path = PySys_GetObject("path");
  PyList_Append(sys_path, PyUnicode_FromString("."));
  PyList_Append(sys_path, PyUnicode_FromString("./bin"));

  // Import Python module
  PyObject *pName = PyUnicode_DecodeFSDefault("get_weights");
  PyObject *pModule = PyImport_Import(pName);
  Py_DECREF(pName);

  if (pModule != NULL) {
    PyObject *pFunc = PyObject_GetAttrString(pModule, "get_weights");
    if (pFunc && PyCallable_Check(pFunc)) {
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
        // Copy back modified values
        for (int i = 0; i < *n; i++) {
          PyObject *item = PyList_GetItem(pReturn, i);
          weights[i] = PyFloat_AsDouble(item);
        }
      } else {
        PyErr_Print();
        fprintf(stderr, "Python function did not return a list\n");
      }
      Py_XDECREF(pReturn);
    } else {
      PyErr_Print();
      fprintf(stderr, "Cannot find Python function 'modify_weights'\n");
    }
    Py_XDECREF(pFunc);
    Py_DECREF(pModule);
  } else {
    PyErr_Print();
    fprintf(stderr, "Failed to load Python module\n");
  }

  Py_Finalize();
}
