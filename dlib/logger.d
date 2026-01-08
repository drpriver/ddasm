module dlib.logger;
import dlib.aliases;
import dlib.stringbuilder;
enum LogLevel {
    DEBUG = 0,
    INFO  = 1,
    WARN  = 2,
    ERROR = 3,
    OFF   = 4,
}

alias Logger = LoggerT!(true, true);
struct LoggerT(bool use_stdio, bool use_malloc) {
    @disable this(this);
    @disable this(ref typeof(this));
    @disable void opAssign(typeof(this));

    LogLevel level;
    // NOTE: you are allowed to write to `buff`.
    static if(use_malloc){
        import dlib.allocator: MALLOCATOR;
        StringBuilder buff = {allocator: MALLOCATOR};
    }
    else
        StringBuilder buff;
    static if(use_stdio){
        void sink(str msg){
            import core.stdc.stdio: fwrite, stderr;
            fwrite(msg.ptr, msg.length, 1, stderr);
        }
    }
    else {
        // provide your own sink.
        void delegate(str) sink;
    }

    // For manual logging (to avoid needing your own temporary string
    // builders), you can write to the buff and then call flush.
    void flush(LogLevel lvl){
        scope(exit) buff.reset;
        if(lvl < level) return;
        sink(buff.borrow());
    }
    void msg(A...)(LogLevel lvl, A args){
        if(lvl < level) return;
        scope(exit) buff.reset;
        buff.FORMAT(args);
        str msg = buff.borrow();
        sink(msg);
    }
    void msgf(A...)(LogLevel lvl, str fmt, A args){
        if(lvl < level) return;
        scope(exit) buff.reset;
        buff.writef(fmt, args);
        str msg = buff.borrow();
        sink(msg);
    }
    void error(A...)(A args){
        msg(LogLevel.ERROR, args);
    }
    void errorf(A...)(str fmt, A args){
        msgf(LogLevel.ERROR, fmt, args);
    }
    void info(A...)(A args){
        msg(LogLevel.INFO, args);
    }
    void infof(A...)(str fmt, A args){
        msgf(LogLevel.INFO, fmt, args);
    }
    void warn(A...)(A args){
        msg(LogLevel.WARN, args);
    }
    void warnf(A...)(str fmt, A args){
        msgf(LogLevel.WARN, fmt, args);
    }
    void debug_(A...)(A args){
        msg(LogLevel.DEBUG, args);
    }
    void debugf(A...)(str fmt, A args){
        msgf(LogLevel.DEBUG, fmt, args);
    }
}
