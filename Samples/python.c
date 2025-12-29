#pragma library("libc.so.6")
extern void abort();
#pragma library("libc.so.6")
extern int printf(const char* fmt, ...);
#pragma library("python3.8")
extern void Py_Initialize();
#pragma library("python3.8")
extern int PyRun_SimpleString(char* s);
#pragma library("python3.8")
extern void PyErr_Print();
#pragma library("python3.8")
extern void Py_Finalize();
#pragma library("python3.8")
extern void* PyUnicode_FromString(char* s);
#pragma library("python3.8")
extern void* PyImport_Import(void* o);
#pragma library("python3.8")
extern void Py_DecRef(void* o);
#pragma library("python3.8")
extern void* PyObject_GetAttrString(void* o, char* s);
#pragma library("python3.8")
extern void* PyObject_CallFunction(void* o, char* s, ...);

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
  void* js = PyObject_CallFunction(loads, "s", "[1,2,3]");
  void* ret = PyObject_CallFunction(print, "N", js);
  Py_DecRef(ret);
  char* code = "print('hello from python')";
  if(PyRun_SimpleString(code) == -1){
    PyErr_Print();
  }
  Py_Finalize();
  return;
}
