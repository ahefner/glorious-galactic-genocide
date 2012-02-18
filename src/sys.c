#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

#include <GL/glew.h>

SDL_Surface *window_surface;
int window_width = 0;
int window_height = 0;
int vid_fullscreen = 0;
static char *apptitle = NULL;

void describe_pixel_format (SDL_PixelFormat *fmt)
{
    printf("  %i bpp, %4X %4X %4X %4X\n", fmt->BitsPerPixel, fmt->Rmask, fmt->Gmask, fmt->Bmask, fmt->Amask);
}

int sys_setvideomode (void)
{
    int surfaceflags = SDL_OPENGL | SDL_DOUBLEBUF | SDL_HWACCEL | SDL_RESIZABLE 
        | (vid_fullscreen ? SDL_FULLSCREEN : 0);

    window_surface = SDL_SetVideoMode(window_width, window_height, 32, surfaceflags);
    if (window_surface == NULL) {
        printf("Could not set desired display mode!\n");
        return 1;
    }

    printf("Window pixel format:\n");
    describe_pixel_format(window_surface->format);

    SDL_WM_SetCaption (apptitle, apptitle);
    SDL_FillRect(window_surface, NULL, SDL_MapRGB(window_surface->format, 0, 0, 0));
    SDL_Flip(window_surface);
    SDL_ShowCursor(SDL_ENABLE);
    SDL_EnableUNICODE(1);
    SDL_GL_SetSwapInterval(1); // Enable VSync

    if (GLEW_OK != glewInit()) {
      printf("OpenGL init error.\n");
    }

    if (!GL_VERSION_1_4) printf("OpenGL is too old (need version 1.4 or newer). This will end badly.\n");

    glViewport(0, 0, window_width, window_height);
    if (glGetError() != GL_NO_ERROR) {
        printf("OpenGL error.\n");
        return 1;
    }

    return 0;
}

int sys_init (char *title)
{
    int i, tmp;
    // My SDL_image is too old for this. It will supposedly make things faster.
    //IMG_Init(IMG_INIT_PNG | IMG_INIT_JPG);

    apptitle = title;

    if (SDL_Init (SDL_INIT_NOPARACHUTE | SDL_INIT_AUDIO | SDL_INIT_VIDEO)) {
        printf ("Could not initialize SDL!\n");
        return 1;
    }

    audio_init();

    SDL_VideoInfo *vinf = SDL_GetVideoInfo();

    printf("Desktop pixel format: ");
    describe_pixel_format(vinf->vfmt);

    if (!(vid_fullscreen)) {
//        window_width = vinf->current_w - 64;
//        window_height = vinf->current_h - 90;
        
        // Minimum supported resolution:
        window_width = 800;
        window_height = 480;

        // Expected typical resolution:
//        window_width = 1280;
//        window_height = 800;
    }   

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 0);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 0);

    return sys_setvideomode();
}

void sys_shutdown (void)
{
    audio_shutdown();
    SDL_FreeSurface(window_surface);
    SDL_Quit();
}

static long long tv_to_micros (struct timeval *tv)
{
    return (((long long)tv->tv_sec) * 1000000ll) + ((long long)tv->tv_usec);
}

unsigned usectime (void)
{
    struct timeval tv;
    if (gettimeofday(&tv, 0)) {
        perror("gettimeofday");
        exit(1);
    }
    return (unsigned)(tv_to_micros(&tv) & 0xFFFFFFFF);
}

void unfuck_image (SDL_Surface *surface)
{
    // Fuck you, SDL_image, or whoever is responsible.
    if (surface->format->Rmask == 0xFF0000) {
        unsigned *pixels = surface->pixels;
        for (unsigned idx = 0; idx < surface->w * surface->h; idx++) {
            unsigned tmp = pixels[idx];
            unsigned foo = tmp & 0xFF00FF00;
            pixels[idx] = foo | ((tmp>>16)&255) | ((tmp&255)<<16);
        }
        surface->format->Rmask=0xFF;
        surface->format->Bmask=0xFF0000;
    }
}
