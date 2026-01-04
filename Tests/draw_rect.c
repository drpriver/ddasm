

#define NULL 0
typedef struct {
    int x, y, w, h;
} Rect;

void DrawRect(void* p, Rect* r){
    // nothing
}

void draw_rect_local(int x, int y, int w, int h){
    Rect r = {x, y, w, h};
    DrawRect(NULL, &r);
}

void draw_rect_literal(int x, int y, int w, int h){
    DrawRect(NULL, &(Rect){x, y, w, h});
}

void start(){
    draw_rect_local(1, 2, 3, 4);
    draw_rect_literal(5, 6, 7, 8);
}
int main(){ return 0; }
