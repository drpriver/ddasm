// depth_test.h - tests magic macros from inside an include
int header_depth = __INCLUDE_DEPTH__;
const char* base_from_header = __BASE_FILE__;
const char* file_from_header = __FILE__;
const char* dir_from_header = __DIR__;
