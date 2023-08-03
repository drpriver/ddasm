/*
 * Copyright © 2021-2023, David Priver
 */
module dlib.term_util;
version(Posix){
import core.sys.posix.sys.ioctl;
import core.sys.posix.unistd;
import core.stdc.stdio;
import core.stdc.stdlib;
int
get_cols(){
    winsize w;
    int err = ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
    if(err != -1 && w.ws_col){
        return w.ws_col;
    }
    char* cols_s = getenv("COLUMNS");
    if(!cols_s) return 80;
    int cols = atoi(cols_s);
    if(!cols) return 80;
    return cols;
}

bool
stdin_is_interactive(){
    return isatty(fileno(stdin)) == 1;
}

}

version(Windows){
import core.sys.windows.winbase;
int get_cols(){
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    BOOL success = GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
    if(!success) return 80;
    int columns = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    if(!columns || columns < 0) return 80;
    return columns;
}
bool
stdin_is_interactive(){
    DWORD ft = GetFileType(GetStdHandle(STD_INPUT_HANDLE));
    return ft == FILE_TYPE_CHAR;
}
}
