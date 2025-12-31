#pragma library("libc")
  #include <stdlib.h>
  #include <stdio.h>
#ifdef __APPLE__
  #pragma library("python")
     // FIXME: framework include handling
     #include "/Library/Frameworks/Python.framework/Headers/Python.h"
#elif defined(__linux__)
  #pragma library("python3.8")
    #include <python3.8/Python.h>
#endif

void* pyimport(char* name){
    void* pystr = PyUnicode_FromString(name);
    if(!pystr) abort();
    void* imp = PyImport_Import(pystr);
    if(!imp) abort();
    Py_DecRef(pystr);
    return imp;
}
void* get(void* obj, char* key){
  return PyObject_GetAttrString(obj, key);
}

int start(){
  printf("hello world\n");
  Py_Initialize();
  void* json = pyimport("json");
  void* loads = get(json, "loads");
  void* builtins = pyimport("builtins");
  void* print = get(builtins, "print");
  void* js = PyObject_CallFunction(loads, "s", 3+"abc[1,2,3]");
  void* ret = PyObject_CallFunction(print, "N", js);
  Py_DecRef(ret);
  char* code = "print('hello from python');import sys;print(f'{sys.version=}')";
  if(PyRun_SimpleString(code) == -1){
    PyErr_Print();
  }
  Py_Finalize();
  return;
}
