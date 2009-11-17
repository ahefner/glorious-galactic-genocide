#ifndef APP_TEXT_RENDER_H
#define APP_TEXT_RENDER_H

#include <stdint.h>
#include "c-util.h"

struct image
{
    int w, h, pitch, x_origin, y_origin;
    uint32_t *pixels;
};

typedef struct image *image_t;
image_t sans_label (uint32_t color, unsigned text_height, char *string);


#endif
