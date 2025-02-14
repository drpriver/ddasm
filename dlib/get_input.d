/*
 * Copyright © 2021-2025, David Priver
 */
module dlib.get_input;
import dlib.allocator;
import dlib.aliases;
import core.stdc.stdio: fputs, fflush, stdout, snprintf;
import core.stdc.string: memcpy, memmove, memset, memchr;
version(Posix){
    import core.stdc.signal: raise;
    import core.sys.posix.signal: SIGTSTP;
}
import dlib.term_util: get_cols;
import dlib.file_util: read_file, write_file;
import dlib.stringbuilder;

// BUG: doesn't handle multibyte characters

struct LineHistory{
    enum LINE_HISTORY_MAX = 100;
    Allocator allocator;
    int count;
    int cursor;
    char[][LINE_HISTORY_MAX] history;

    // must be a C string.
    int
    dump(const char* filename){
        StringBuilder sb = {allocator:allocator};
        scope(exit) sb.cleanup;
        for(int i = 0; i < count; i++){
            sb.write(history[i]);
            sb.write("\0");
        }
        auto text = sb.borrow;
        int result = write_file(text, filename);
        return result;
    }
    // must be a C string.
    int
    load_history(const char* filename){
        auto r = read_file(filename);
        if(r.errored) return r.errored;
        auto bdata = r.value;
        scope(exit) bdata.dealloc;
        auto data = bdata.data;
        void* p = data.ptr;
        void* end = data.ptr + data.length;
        void* zed = null;
        for(;p != end;){
            zed = memchr(p, 0, end - p);
            if(!zed) break;
            void[] line = p[0..zed-p];
            add_line(cast(str)line);
            p = zed + 1;
        }
        return 0;
    }
    void
    remove_last_line(){
        if(!count) return;
        auto last = &history[--count];
        allocator.free(last.ptr, last.length);
    }
    str
    last_line(){
        if(!count) return null;
        return history[count-1];
    }

    void
    add_line(str line){
        if(count){
            auto last = &history[count-1];
            if(*last == line)
                return;
        }
        auto copy = cast(char[])allocator.alloc(line.length);
        memcpy(copy.ptr, line.ptr, line.length);
        if(count == LINE_HISTORY_MAX){
            allocator.free(history[0].ptr, history[0].length);
            memmove(history.ptr, history.ptr+1, (LINE_HISTORY_MAX-1)*history[0].sizeof);
            history[LINE_HISTORY_MAX-1] = copy;
            return;
        }
        history[count++] = copy;
    }

    void
    cleanup(){
        foreach(ref line; history){
            if(line.ptr){
                allocator.free(line.ptr, line.length);
                line = null;
            }
        }
    }

}

ptrdiff_t
get_input_line(LineHistory)(LineHistory* history, str prompt, char[] buff){
    history.cursor = history.count;
    if(!get_line_is_init)
        get_line_init();
    TermState termstate;
    termstate.enable_raw();
    ptrdiff_t length = get_line_internal_loop(history, buff, prompt);
    termstate.disable_raw();
    write_data("\r");
    return length;
}

private:

__gshared bool get_line_is_init;

void get_line_init(){
    get_line_is_init = true;
    version(WINDOWS){
        import core.sys.windows.windows;
        // In theory we should open "CONOUT$" instead, but idk.
        // TODO: report errors.
        HANDLE hnd = GetStdHandle(STD_OUTPUT_HANDLE);
        DWORD mode;
        BOOL success = GetConsoleMode(hnd, &mode);
        if(!success)
            return;
        mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING | DISABLE_NEWLINE_AUTO_RETURN;
        success = SetConsoleMode(hnd, mode);
        if(!success)
            return;
        hnd = GetStdHandle(STD_INPUT_HANDLE);
        success = GetConsoleMode(hnd, &mode);
        if(!success)
            return;
        mode |= ENABLE_VIRTUAL_TERMINAL_INPUT;
        success = SetConsoleMode(hnd, mode);
        if(!success)
            return;
    }
    // Someone might have hidden the cursor, which is annoying.
    enum SHOW_CURSOR = "\033[?25h";
    fputs(SHOW_CURSOR, stdout);
    fflush(stdout);
}

version(Windows){
    struct TermState {
        void enable_raw(){}
        void disable_raw(){}
    }
}
version(Posix){
    import core.sys.posix.sys.ioctl;
    import core.sys.posix.termios;
    import core.sys.posix.unistd;
    struct TermState {
        termios raw;
        termios orig;
        void enable_raw(){
            if(tcgetattr(STDIN_FILENO, &orig) == -1)
                return;
            raw = orig;
            raw.c_iflag &= ~(0LU
                    | BRKINT // no break
                    | ICRNL  // don't map CR to NL
                    | INPCK  // skip parity check
                    | ISTRIP // don't strip 8th bit off char
                    | IXON   // don't allow start/stop input control
                    );
            raw.c_oflag &= ~(0LU
                | OPOST // disable post processing
                );
            raw.c_cflag |= CS8; // 8 bit chars
            raw.c_lflag &= ~(0LU
                    | ECHO    // disable echo
                    | ICANON  // disable canonical processing
                    | IEXTEN  // no extended functions
                    // Currently allowing these so ^Z works, could disable them
                    | ISIG    // disable signals
                    );
            raw.c_cc[VMIN] = 1; // read every single byte
            raw.c_cc[VTIME] = 0; // no timeout

            // set and flush
            // Change will ocurr after all output has been transmitted.
            // Unread input will be discarded.
            if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) < 0)
                return;
        }
        void disable_raw(){
            tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig);
        }
    }
}


struct LineState {
    char[] buff;
    str prompt;
    size_t curr_pos;
    size_t length;
    size_t cols;
    int history_index;
    size_t completion_cookie;
}
ptrdiff_t
get_line_internal_loop(LineHistory)(LineHistory* history, char[]buff, str prompt){
    LineState ls = {
        buff, prompt, cols: get_cols(),
    };
    char[4092] original_buff;
    size_t original_curr_pos = 0;
    size_t original_used_len = 0;
    bool in_tab = false;
    write_data(prompt);
    memset(buff.ptr, 0, buff.length);
    redisplay(&ls);
    enum {
        CTRL_A = 1,         // Ctrl-a
        CTRL_B = 2,         // Ctrl-b
        CTRL_C = 3,         // Ctrl-c
        CTRL_D = 4,         // Ctrl-d
        CTRL_E = 5,         // Ctrl-e
        CTRL_F = 6,         // Ctrl-f
        CTRL_H = 8,         // Ctrl-h
        TAB    = 9,         // Tab
        CTRL_K = 11,        // Ctrl-k
        CTRL_L = 12,        // Ctrl-l
        ENTER  = 13,        // Enter
        CTRL_N = 14,        // Ctrl-n
        CTRL_O = 15,        // Ctrl-o
        CTRL_P = 16,        // Ctrl-p
        CTRL_T = 20,        // Ctrl-t
        CTRL_U = 21,        // Ctrl-u
        CTRL_V = 22,        // Ctrl-v
        CTRL_W = 23,        // Ctrl-w
        CTRL_Z = 26,        // Ctrl-z
        ESC    = 27,        // Escape
        BACKSPACE =  127    // Backspace
    }
    for(;;){
        ubyte c;
        ubyte[8] sequence;
        ptrdiff_t nread = read_one(&c);
        if(nread <= 0)
            return ls.length;
        if(c != TAB){
            in_tab = false;
            ls.completion_cookie = 0;
        }
        if(c == TAB){
            // TODO: completion func
            continue;
        }
        switch(c){
            case '\n':
            case ENTER:
                write_data("\n");
                return ls.length;
            case BACKSPACE: case CTRL_H:
                if(ls.curr_pos > 0 && ls.length > 0){
                    memmove(ls.buff.ptr+ls.curr_pos-1, ls.buff.ptr+ls.curr_pos, ls.length-ls.curr_pos);
                    ls.curr_pos--;
                    ls.buff[--ls.length] = '\0';
                    redisplay(&ls);
                }
                break;
            case CTRL_D:
                if(ls.length > 0){
                    delete_right(&ls);
                    redisplay(&ls);
                }
                else {
                    write_data("^D\r\n");
                    return -1;
                }
                break;
            case CTRL_T:
                if(ls.curr_pos > 0 && ls.curr_pos < ls.length){
                    // swap with previous
                    char temp = buff[ls.curr_pos-1];
                    buff[ls.curr_pos-1] = buff[ls.curr_pos];
                    buff[ls.curr_pos] = temp;
                    if (ls.curr_pos != ls.length-1)
                        ls.curr_pos++;
                    redisplay(&ls);
                }
                break;
            case CTRL_B:
                if(ls.curr_pos > 0){
                    ls.curr_pos--;
                    redisplay(&ls);
                }
                break;
            case CTRL_F:
                if(ls.curr_pos != ls.length){
                    ls.curr_pos++;
                    redisplay(&ls);
                }
                break;
            case CTRL_P:
                change_history(history, &ls, -1);
                redisplay(&ls);
                break;
            case CTRL_N:
                change_history(history, &ls, +1);
                redisplay(&ls);
                break;
            case ESC: // beginning of escape sequence
                if(read_one(&sequence[0]) == -1) return -1;
                if(read_one(&sequence[1]) == -1) return -1;

                // ESC [ sequences
                if(sequence[0] == '['){
                    if (sequence[1] >= '0' && sequence[1] <= '9'){
                        // Extended escape, read additional byte.
                        if (read_one(&sequence[2]) == -1) return -1;
                        if (sequence[2] == '~') {
                            switch(sequence[1]) {
                            case '3': /* Delete key. */
                                delete_right(&ls);
                                redisplay(&ls);
                                break;
                            default: break;
                            }
                        }
                    }
                    else {
                        switch(sequence[1]) {
                        default: break;
                        case 'A': // Up
                            change_history(history, &ls, -1);
                            redisplay(&ls);
                            break;
                        case 'B': // Down
                            change_history(history, &ls, +1);
                            redisplay(&ls);
                            break;
                        case 'C': // Right
                            if(ls.curr_pos != ls.length){
                                ls.curr_pos++;
                                redisplay(&ls);
                            }
                            break;
                        case 'D': // Left
                            if(ls.curr_pos > 0){
                                ls.curr_pos--;
                                redisplay(&ls);
                            }
                            break;
                        case 'H': // Home
                            ls.curr_pos = 0;
                            redisplay(&ls);
                            break;
                        case 'F': // End
                            ls.curr_pos = ls.length;
                            redisplay(&ls);
                            break;
                        case 'Z': // Shift-tab
                            break;
                        }
                    }
                }
                else if(sequence[0] == 'O'){
                    switch(sequence[1]){
                        default: break;
                        case 'H': // Home
                            ls.curr_pos = 0;
                            redisplay(&ls);
                            break;
                        case 'F': // End
                            ls.curr_pos = ls.length;
                            redisplay(&ls);
                            break;
                        }
                    }
                break;
            default:
                if(c < 27)
                    continue;
                insert_char_into_line(&ls, c);
                redisplay(&ls);
                break;
            case CTRL_C:
                buff[0] = '\0';
                ls.curr_pos = 0;
                ls.length = 0;
                redisplay(&ls);
                break;
            case CTRL_U: // Delete entire line
                buff[0] = '\0';
                ls.curr_pos = 0;
                ls.length = 0;
                redisplay(&ls);
                break;
            case CTRL_K: // Delete to end of line
                buff[ls.curr_pos] = '\0';
                ls.length = ls.curr_pos;
                redisplay(&ls);
                break;
            case CTRL_A: // Home
                ls.curr_pos = 0;
                redisplay(&ls);
                break;
            case CTRL_E: // End
                ls.curr_pos = ls.length;
                redisplay(&ls);
                break;
            case CTRL_L: // Clear entire screen
                enum CLEARSCREEN = "\x1b[H\x1b[2J";
                write_data(CLEARSCREEN);
                redisplay(&ls);
                break;
            case CTRL_W:{ // Delete previous word
                size_t old_pos = ls.curr_pos;
                size_t diff;
                size_t pos = ls.curr_pos;
                // Backup until we hit a nonspace.
                while(pos > 0 && buff[pos-1] == ' ')
                    pos--;
                // Backup until we hit a space.
                while(pos > 0 && buff[pos-1] != ' ')
                    pos--;
                diff = old_pos - pos;
                memmove(buff.ptr+pos, buff.ptr+old_pos, ls.length-old_pos+1);
                ls.curr_pos = pos;
                ls.length -= diff;
                redisplay(&ls);
            }break;
            case CTRL_Z:{
                write_data("^Z\r\n");
                version(Posix){
                    raise(SIGTSTP);
                }
                redisplay(&ls);
            }break;
        }
    }
}

ptrdiff_t
write_data(str buff){
    version(Windows){
        fwrite(buff.ptr, buff.length, 1, stdout);
        fflush(stdout);
        return buff.length;
    }
    version(Posix){
        return write(STDOUT_FILENO, buff.ptr, buff.length);
    }
}

void
redisplay(LineState* ls){
    enum LINESIZE=1024;
    ubyte[LINESIZE] seq;
    int seq_pos = 0;
    size_t plen = ls.prompt.length;
    char* buff = ls.buff.ptr;
    size_t len = ls.length;
    size_t pos = ls.curr_pos;
    size_t cols = ls.cols;
    assert(cols);
    // Scroll the text right until the current cursor position
    // fits on screen.
    while((plen+pos) >= cols) {
        buff++;
        len--;
        pos--;
    }
    // Truncate the string so it fits on screen.
    while (plen+len > cols) {
        len--;
    }
    // Move to left.
    seq[seq_pos++] = '\r';
    // Copy the prompt.
    if(plen+seq_pos < LINESIZE){
        memcpy(seq.ptr+seq_pos, ls.prompt.ptr, plen);
        seq_pos += plen;
    }
    else
        return;
    // Copy the visible section of the buffer.
    if(seq_pos + len < LINESIZE){
        memcpy(seq.ptr+seq_pos, buff, len);
        seq_pos += len;
        }
    else
        return;
    // Erase anything remaining on this line to the right.
    enum ERASERIGHT = "\x1b[0K";
    if(seq_pos + ERASERIGHT.length < LINESIZE){
        memcpy(seq.ptr+seq_pos, ERASERIGHT.ptr, ERASERIGHT.length);
        seq_pos += ERASERIGHT.length;
        }
    else
        return;
    // Move cursor back to original position.
    int printsize = snprintf(cast(char*)seq.ptr+seq_pos, LINESIZE-seq_pos, "\r\x1b[%zuC", pos+plen);
    if(printsize > LINESIZE-seq_pos)
        return;
    else
        seq_pos += printsize;
    write_data(cast(char[])seq[0..seq_pos]);
}

ptrdiff_t
read_one(ubyte* buff){
    version(Windows){
    static const char* remaining;
    if(remaining){
        *buff = *remaining++;
        if(!*remaining)
            remaining = null;
        return 1;
        }
    for(;;){
        int c = _getch();
        switch(c){
            case 224:{
                int next = _getch();
                switch(next){
                    // left cursor
                    case 'K':
                        *buff = '\033';
                        remaining = "[D";
                        return 1;
                    // up
                    case 'H':
                        *buff = '\033';
                        remaining = "[A";
                        return 1;
                    // down
                    case 'P':
                        *buff = '\033';
                        remaining = "[B";
                        return 1;
                    // right
                    case 'M':
                        *buff = '\033';
                        remaining = "[C";
                        return 1;
                    // home
                    case 'G':
                        *buff = '\x01';
                        return 1;
                    // end
                    case 'O':
                        *buff = '\x05';
                        return 1;
                    case 'S': // del
                        *buff = '\x7f';
                        return 1;

                    // insert
                    // case 'R':

                    // pgdown
                    // case 'Q':

                    // pgup
                    // case 'I':

                    default:
                        continue;
                    }
                }
            default:
                *buff = c;
                return 1;
            }
    }
    return 1;
    }
    version(Posix)
        return read(STDIN_FILENO, buff, 1);
    }

void
delete_right(LineState* ls){
    if(ls.length > 0 && ls.curr_pos < ls.length){
        char* buff = ls.buff.ptr;
        size_t pos = ls.curr_pos;
        memmove(buff+pos, buff+pos+1,ls.length-pos-1);
        buff[--ls.length] = '\0';
    }
}

void
change_history(LineHistory)(LineHistory* history, LineState* ls, int magnitude){
    history.cursor += magnitude;
    if(history.cursor < 0)
        history.cursor = 0;
    if(history.cursor >= history.count){
        history.cursor = history.count;
        ls.length = 0;
        ls.curr_pos = 0;
        ls.buff[ls.length] = '\0';
        return;
        }
    if(history.cursor < 0)
        return;
    str old = history.history[history.cursor];
    size_t length = old.length < ls.buff.length? old.length : ls.buff.length;
    if(length)
        memcpy(ls.buff.ptr, old.ptr, length);
    ls.buff[length] = '\0';
    ls.length = length;
    ls.curr_pos = length;
}

void
insert_char_into_line(LineState* ls, char c){
    if(ls.length >= ls.buff.length)
        return;
    // At the end of the line anyway
    if(ls.length == ls.curr_pos){
        ls.buff[ls.curr_pos++] = c;
        ls.buff[++ls.length] = '\0';
        return;
    }
    // Write into the middle of the buffer
    memmove(ls.buff.ptr+ls.curr_pos+1,ls.buff.ptr+ls.curr_pos,ls.length-ls.curr_pos);
    ls.buff[ls.curr_pos++] = c;
    ls.buff[++ls.length] = '\0';
}
