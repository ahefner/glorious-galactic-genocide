#ifndef APP_C_UTIL_H
#define APP_C_UTIL_H

#include <stdint.h>

#define max(x,y) ((x)>(y)?(x):(y))
#define min(x,y) ((x)>(y)?(y):(x))

static inline uint8_t clamp_byte (int x)
{
    if (x < 0) return 0;
    if (x > 255) return 255;
    else return x;
}

#endif
