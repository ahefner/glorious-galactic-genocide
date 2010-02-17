#ifndef APP_SYS_H
#define APP_SYS_H

#include <SDL/SDL.h>

extern SDL_Surface *window_surface;
extern int window_width;
extern int window_height;
extern int vid_fullscreen;
extern int sound_initialized;

int sys_init (char *title);
void sys_shutdown (void);
unsigned usectime (void);

#endif
