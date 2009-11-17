#ifndef APP_SYS_H
#define APP_SYS_H

#include <SDL/SDL.h>

extern SDL_Surface *window_surface;
extern int window_width;
extern int window_height;
extern int vid_fullscreen;
extern SDL_Event cur_event;

int sys_init (char *title);
void sys_shutdown (void);

#endif
