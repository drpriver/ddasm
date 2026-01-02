// Snek game
#pragma library("libc")
  #include <stdlib.h>
  #include <string.h>
  #include <stdio.h>
  #include <time.h>
#pragma library("SDL2")
  #include <SDL2/SDL.h>

#ifdef __DDASM__
void abort(void){ __dasm { dump; abort } }
#endif

SDL_Window* gwindow;
SDL_Renderer* grenderer;
typedef signed char Tile;
Tile* gboard;
int gwinlose = 0;
Tile** gsnake;
int glen;
int gpaused;
void main_loop(void);
void render_and_present(int sx, int sy, int dx, int dy);
enum {BOARD_SIZE=10, HALF_BOARD_SIZE=5};
int start(int width, int height){
  srand(time(NULL));
  void* window;
  void* renderer;
  if(width < 400) width = 400;
  if(width > 1200) width = 1200;
  if(height < 400) height = 400;
  if(height > 1200) height = 1200;
  SDL_Init(SDL_INIT_VIDEO);
  window = SDL_CreateWindow(
    "Snek!",
    SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
    width, height,
    SDL_WINDOW_SHOWN | SDL_WINDOW_ALLOW_HIGHDPI | SDL_WINDOW_RESIZABLE
  );
  if(!window) {printf("no window\n"); abort();}
  renderer = SDL_CreateRenderer(window, -1, 0);
  if(!renderer) {printf("no renderer\n"); abort();}
  SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
  gwindow = window;
  grenderer = renderer;
  Tile* board = calloc(BOARD_SIZE*BOARD_SIZE, sizeof *board);
  gboard = board;
  Tile** snake = calloc(BOARD_SIZE*BOARD_SIZE, sizeof *snake);
  gsnake = snake;
  main_loop();
  free(board);
  free(snake);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
#ifdef __DDASM__
  // hack, this will break once we have dead code elimination, but this
  // generates a reference to this function that is otherwise only
  // referenced from inline dasm and I don't feel like actually parsing
  // inline dasm in the c front end yet.
  SDL_RenderFillRect(renderer, &(SDL_Rect){0});
#endif
}

void grow_window(){
  int w; int h;
  SDL_GetWindowSize(gwindow, &w, &h);
  w = w + 50;
  h = h + 50;
  if(w > 1200) w = 1200;
  if(h > 1200) h = 1200;
  SDL_SetWindowSize(gwindow, w, h);
}
void shrink_window(){
  int w; int h;
  SDL_GetWindowSize(gwindow, &w, &h);
  w = w - 50;
  h = h - 50;
  if(w < 100) w = 100;
  if(h < 100) h = 100;
  SDL_SetWindowSize(gwindow, w, h);
}

int has_apple(Tile* board){
  for(int y = 0; y < BOARD_SIZE; y = y + 1){
    for(int x = 0; x < BOARD_SIZE; x = x + 1){
      Tile* p = y*BOARD_SIZE+x+board;
      if(*p == -1){
        return 1;
      }
    }
  }
  return 0;
}
void place_apple(Tile* board){
  for(;;){
    int y = rand() % BOARD_SIZE;
    int x = rand() % BOARD_SIZE;
    Tile* p = y*BOARD_SIZE+x+board;
    if(!*p){
      *p = -1;
      return;
    }
  }
}

void move_snake(Tile* board, Tile** snake, int x, int y){
  Tile* p;
  p = board + BOARD_SIZE*y+x;
  Tile b = *p;
  *p = 1;
  if(b == -1){
    snake[glen] = p;
    glen = glen + 1;
    return;
  }
  if(b == 1){
    if(glen == BOARD_SIZE * BOARD_SIZE){
      gwinlose = 1;
      return;
    }
    gwinlose = 2;
    return;
  }
  int l = glen-1;
  Tile* ps = *snake;
  *ps = 0;
  for(int i = 0; i < l; i = i + 1){
    snake[i] = snake[i+1];
  }
  snake[glen-1] = p;
}

void simulate(Tile* board, int x, int y){
  move_snake(board, gsnake, x, y);
  if(gwinlose) return;
  int apple = has_apple(board);
  if(!apple) place_apple(board);
}

void main_loop(void){
  Tile* board = gboard;
  SDL_Event* event = calloc(1, sizeof(SDL_Event));
  int poll = 1;
  int t = SDL_GetTicks();
  int trigger = 10;
  int tick = trigger-1;
  board[HALF_BOARD_SIZE*BOARD_SIZE+HALF_BOARD_SIZE] = 1;
  gsnake[0] = board +HALF_BOARD_SIZE*BOARD_SIZE+HALF_BOARD_SIZE;
  glen = 1;
  int dx = 0;
  int dy = 1;
  int x = HALF_BOARD_SIZE;
  int y = HALF_BOARD_SIZE;
  int turned = 0;
  for(;;){
    int got_event = 0;
    if(poll){
      if(!SDL_PollEvent(event)){
        tick = tick + 1;
        if(tick >= trigger){
          tick = 0;
          if(!gwinlose){
            if(!gpaused){
              x = x + dx;
              y = y + dy;
              if(x == -1) x = BOARD_SIZE-1;
              if(y == -1) y = BOARD_SIZE-1;
              if(x == BOARD_SIZE) x = 0;
              if(y == BOARD_SIZE) y = 0;
              simulate(board, x, y);
              turned = 0;
            }
            render_and_present(x, y, dx, dy);
          }
        }
        // render_and_present(x, y, dx, dy)
        int t2 = SDL_GetTicks();
        int diff = t2-t;
        if(diff < 16) SDL_Delay(16-diff);
        t = SDL_GetTicks();
        poll = 1;
        continue;
      }
    }
    else {
      SDL_WaitEvent(event);
      poll = 1;
    }
    int type = event->type;
    if(type == SDL_QUIT){
      break;
    }
    else if(type == SDL_KEYDOWN){
      SDL_Keycode code = event->key.keysym.sym;
      if(code == 'q') break;
      else if(code == ' ') gpaused = !gpaused;
      else if(code == '=') grow_window();
      else if(code == '-') shrink_window();
      else if(code == 'a'){
        if(dx != 1){
          dx = -1;
          dy = 0;
        }
      }
      else if(code == 'r'){
        gwinlose = 0;
        tick = trigger-1;
        memset(board, 0, BOARD_SIZE*BOARD_SIZE* sizeof *board);
        x = HALF_BOARD_SIZE;
        y = HALF_BOARD_SIZE;
        board[HALF_BOARD_SIZE*BOARD_SIZE+HALF_BOARD_SIZE] = 1;
        gsnake[0] = board +HALF_BOARD_SIZE*BOARD_SIZE+HALF_BOARD_SIZE;
        glen = 1;
      }
      else if(!turned){
        switch(code){
          case SDLK_LEFT:
          case 'a':
            if(dx != 1){
              dx = -1;
              dy = 0;
              turned = 1;
            }
            break;
          case SDLK_RIGHT:
          case 'd':
            if(dx != -1){
              dx = 1;
              dy = 0;
              turned = 1;
            }
            break;
          case SDLK_UP:
          case 'w':
            if(dy != 1){
              dx = 0;
              dy = -1;
              turned = 1;
            }
            break;
          case SDLK_DOWN:
          case 's':
            if(dy != -1){
              dx = 0;
              dy = 1;
              turned = 1;
            }
            break;
          default:
            break;
        }
      }
    }
  }
  free(event);
}

void draw_rect(void* renderer, int x, int y, int w, int h){
    #ifdef __DDASM__
    __dasm__ {
        shl rarg3 rarg3 32
        or rarg2 rarg2 rarg3
        write rsp rarg2
        shl rarg5 rarg5 32
        or rarg4 rarg4 rarg5
        add rarg2 rsp 8
        write rarg2 rarg4
        move rarg2 rsp
        call function SDL2.SDL_RenderFillRect
        ret
    }
    #else
    // SDL_Rect r = {x, y, w, h};
    // SDL_RenderFillRect(renderer, &r);
    SDL_RenderFillRect(renderer, &(SDL_Rect){x, y, w, h});
    #endif
}

void render_and_present(int sx, int sy, int dx, int dy){
  int w = 640;
  int h = 480;
  void* window = gwindow;
  void* renderer = grenderer;
  Tile* board = gboard;
  int winlose = gwinlose;
  SDL_GetWindowSize(window, &w, &h);
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0xff);
  SDL_RenderClear(renderer);
  SDL_SetRenderDrawColor(renderer, 0xff, 0xff, 0, 0xff);
  int ww = w/20;
  int hh = h/20;
  if(winlose == 1){
    SDL_SetRenderDrawColor(renderer, 0, 0xff, 0, 0xff);
    draw_rect(renderer, 3*ww, 1*hh, ww, 3*hh);
    draw_rect(renderer, 5*ww, 1*hh, ww, 3*hh);
    draw_rect(renderer, 4*ww, 4*hh, ww, 2*hh);

    draw_rect(renderer, 7*ww, 1*hh, ww, 5*hh);
    draw_rect(renderer, 8*ww, 1*hh, ww, 2*hh);
    draw_rect(renderer, 9*ww, 1*hh, ww, 5*hh);

    draw_rect(renderer, 11*ww, 1*hh, ww, 5*hh);
    draw_rect(renderer, 12*ww, 5*hh, ww, hh);
    draw_rect(renderer, 13*ww, 1*hh, ww, 5*hh);

    draw_rect(renderer, 2*ww, 7*hh, ww, 5*hh);
    draw_rect(renderer, 3*ww, 11*hh, ww, hh);
    draw_rect(renderer, 4*ww, 7*hh, ww, 5*hh);
    draw_rect(renderer, 5*ww, 11*hh, ww, hh);
    draw_rect(renderer, 6*ww, 7*hh, ww, 5*hh);

    draw_rect(renderer, 8*ww, 7*hh, ww, hh);
    draw_rect(renderer, 10*ww, 7*hh, ww, hh);
    draw_rect(renderer, 9*ww, 7*hh, ww, 5*hh);
    draw_rect(renderer, 8*ww, 11*hh, ww, hh);
    draw_rect(renderer, 10*ww, 11*hh, ww, hh);

    draw_rect(renderer, 12*ww, 7*hh, ww, 5*hh);
    draw_rect(renderer, 13*ww, 8*hh, ww, hh);
    draw_rect(renderer, 14*ww, 9*hh, ww, hh);
    draw_rect(renderer, 15*ww, 10*hh, ww, hh);
    draw_rect(renderer, 16*ww, 7*hh, ww, 5*hh);
  }
  else if(winlose == 2){
    SDL_SetRenderDrawColor(renderer, 0xff, 0, 0, 0xff);
    //....................
    //...#.#.###.#.#......
    //...#.#.#.#.#.#......
    //...#.#.#.#.#.#......
    //....#..#.#.#.#......
    //....#..###.###......
    //....................
    //..#...###.###.###...
    //..#...#.#.#...#.....
    //..#...#.#.###.###...
    //..#...#.#...#.#.....
    //..###.###.###.###...
    //....................
    //....................
    //....................
    //....................
    //....................
    //....................
    //....................
    //....................

    // Y
    draw_rect(renderer, 4*ww, 4*hh, ww, 3*hh);
    draw_rect(renderer, 6*ww, 4*hh, ww, 3*hh);
    draw_rect(renderer, 5*ww, 7*hh, ww, 2*hh);

    // O
    draw_rect(renderer, 8*ww,  4*hh, ww, 5*hh);
    draw_rect(renderer, 9*ww,  4*hh, ww, 1*hh);
    draw_rect(renderer, 9*ww,  8*hh, ww, 1*hh);
    draw_rect(renderer, 10*ww, 4*hh, ww, 5*hh);

    // U
    draw_rect(renderer, 12*ww, 4*hh, ww, 5*hh);
    draw_rect(renderer, 13*ww, 8*hh, ww, hh);
    draw_rect(renderer, 14*ww, 4*hh, ww, 5*hh);

    // L
    draw_rect(renderer, 2*ww, 10*hh, ww, 5*hh);
    draw_rect(renderer, 3*ww, 14*hh, ww, hh);
    draw_rect(renderer, 4*ww, 14*hh, ww, hh);

    // O
    draw_rect(renderer, 6*ww, 10*hh, ww, 5*hh);
    draw_rect(renderer, 7*ww, 10*hh, ww, hh);
    draw_rect(renderer, 7*ww, 14*hh, ww, hh);
    draw_rect(renderer, 8*ww, 10*hh, ww, 5*hh);

    // S
    draw_rect(renderer, 10*ww, 10*hh, ww, 3*hh);
    draw_rect(renderer, 11*ww, 10*hh, ww, hh);
    draw_rect(renderer, 11*ww, 12*hh, ww, hh);
    draw_rect(renderer, 10*ww, 14*hh, 2*ww, hh);
    draw_rect(renderer, 12*ww, 10*hh, ww, hh);
    draw_rect(renderer, 12*ww, 12*hh, ww, 3*hh);

    // E
    draw_rect(renderer, 14*ww, 10*hh, ww, 5*hh);
    draw_rect(renderer, 15*ww, 10*hh, ww, hh);
    draw_rect(renderer, 15*ww, 12*hh, ww, hh);
    draw_rect(renderer, 15*ww, 14*hh, ww, hh);
    draw_rect(renderer, 16*ww, 10*hh, ww, hh);
    draw_rect(renderer, 16*ww, 12*hh, ww, hh);
    draw_rect(renderer, 16*ww, 14*hh, ww, hh);
  }
  else {
    int rw = w/BOARD_SIZE;
    int rh = h/BOARD_SIZE;
    for(int y = 0; y < BOARD_SIZE; y = y + 1){
      for(int x = 0; x < BOARD_SIZE; x = x + 1){
        Tile* p = y*BOARD_SIZE+x+board;
        Tile val = *p;
        if((x == sx) & (y == sy)){
          SDL_SetRenderDrawColor(renderer, 0x0, 0xff, 0xff, 0xff);
        }
        else if(!val) {
            SDL_SetRenderDrawColor(renderer, 0x0, 0x0, 0xff, 0xff);
        }
        else if(val == -1){
          SDL_SetRenderDrawColor(renderer, 0xff, 0x0, 0x0, 0xff);
        }
        else {
          SDL_SetRenderDrawColor(renderer, 0x0, 0xff, 0x0, 0xff);
        }
        draw_rect(renderer, x*rw, y*rh, rw, rh);
      }
    }
  }
  SDL_RenderPresent(renderer);
}
int main(){
    start(640, 640);
}
