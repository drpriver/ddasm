// SKIP: uses BSD/macOS-specific __swbuf
typedef struct {
    int _w;
    int _lbfsize;
    int* _p;
} FILE;
int __swbuf(int, FILE*);
int __sputc(int _c, FILE *_p) {
	if (--_p->_w >= 0 || (_p->_w >= _p->_lbfsize && (char)_c != '\n'))
		return (*_p->_p++ = _c);
	else
		return (__swbuf(_c, _p));
}
int main(){ return 0; }
