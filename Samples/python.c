#pragma library("libc")
  #include <stdlib.h>
  #include <stdio.h>
#ifdef __APPLE__
  #pragma library("python")
     #include <Python/Python.h>
#elif defined(__linux__)
  #pragma library("python3.8")
    #include <python3.8/Python.h>
#endif

PyObject* pyimport(char* name){
    PyObject* pystr = PyUnicode_FromString(name);
    if(!pystr) abort();
    PyObject* imp = PyImport_Import(pystr);
    if(!imp) abort();
    Py_DecRef(pystr);
    return imp;
}
PyObject* get(PyObject* obj, char* key){
  return PyObject_GetAttrString(obj, key);
}
#ifndef Py_DECREF
#error "no Py_DECREF"
#endif

int main(){
  printf("hello world\n");
  Py_Initialize();
  PyObject* json = pyimport("json");
  PyObject* loads = get(json, "loads");
  PyObject* builtins = pyimport("builtins");
  PyObject* print = get(builtins, "print");
  PyObject* js = PyObject_CallFunction(loads, "s", 3+"abc[1,2,3]");
  PyObject* ret = PyObject_CallFunction(print, "N", js); // steals ref
  Py_DecRef(ret);
  Py_DECREF(print);
  Py_DECREF(builtins);
  Py_DECREF(loads);
  Py_DECREF(json);
  char* code = "print('hello from python');import sys;print(f'{sys.version=}')";
  if(PyRun_SimpleString(code) == -1){
    PyErr_Print();
  }
  Py_Finalize();
  return 0;
}
