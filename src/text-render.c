#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include "text-render.h"

/*** Freetype glue ***/

FT_Library ftlibrary;
FT_Face face_sans;
int freetype_init = 0;

char *asset (char *name) 
{
    /* QUICK HACK FIXME? */
    return name;
}

int ensure_freetype (void)
{
    if (!freetype_init) {
        freetype_init = -1;
        int error = FT_Init_FreeType(&ftlibrary);
        if (error) {
            fprintf(stderr, "Unable to initialize freetype.\n");
            return -1;
        }

        char *filename = asset("data/DejaVuSans.ttf");
        error = FT_New_Face(ftlibrary, filename, 0, &face_sans);
        if (error) {
            fprintf(stderr, "Error opening %s\n", filename);
            return -1;
        }

        freetype_init = 1;
    }

    return freetype_init;
}

image_t sans_label (uint32_t color, unsigned text_height, char *string)
{
    static uint8_t *rtmp = NULL;
    int rwidth = 4096;          /* KLUDGE */
    static unsigned tmpheight = 0;
    static unsigned rheight = 0;
    int baseline = text_height + 4;
    int error;
    int first_char = 1;
    int min_x = 0;
    int min_y = baseline;
    int max_y = baseline + 1;
    int max_x = 1;

    ensure_freetype();

    assert(text_height > 0);
    if (text_height > tmpheight) {
        if (rtmp) free(rtmp);
        // This is what you might call a hack.
        rheight = (text_height*2 + 4);
        rtmp = calloc(1, rwidth * rheight);
        assert(rtmp != NULL);
        tmpheight = text_height;
    } else memset(rtmp, 0, rwidth * rheight);

    FT_Set_Pixel_Sizes(face_sans, 0, text_height);

    int pen_x = 0;
    FT_UInt last_glyph_index = 0;

    char *str = string;
    for (char c = *str++; c; c=*str++) {
        // Beg Freetype to render the glyph
        FT_UInt glyph_index = FT_Get_Char_Index(face_sans, c); 

        if (first_char) first_char = 0;
        else {
            FT_Vector delta;
            FT_Get_Kerning(face_sans, last_glyph_index, glyph_index, 
                           FT_KERNING_DEFAULT, &delta);
            pen_x += delta.x >> 6;
            last_glyph_index = glyph_index;
        }

        error = FT_Load_Glyph(face_sans, glyph_index, 
                              FT_LOAD_RENDER | 
                              FT_LOAD_NO_HINTING | 
                              FT_LOAD_TARGET_LIGHT);
        if (error) continue;

        FT_Bitmap *bmp = &face_sans->glyph->bitmap;
        /*
        printf("  x=%3i: Char %c:   %ix%i + %i + %i   advance=%f\n",
               pen_x, c, bmp->width, bmp->rows,
               face_sans->glyph->bitmap_left,
               face_sans->glyph->bitmap_top,
               ((float)face_sans->glyph->advance.x) / 64.0);
        */        

        /* Transfer the glyph to the temporary buffer.. */
        int ox0 = pen_x + face_sans->glyph->bitmap_left;
        int ix0 = 0;
        int ox1 = ox0 + bmp->width;
        int ix1 = bmp->width;

        // Clip to left edge
        if (ox0 < 0) {
            printf("PRECLIP %s\n", string);
            ix0 -= ox0;
            ox0 -= ox0;
        }

        if (ox0 >= rwidth) break;

        // Clip to right edge
        int overflow = max(0, ox1 - rwidth);
        if (overflow) printf("POSTCLIP\n");
        ox1 -= overflow;
        ix1 -= overflow;
        int width = ox1 - ox0;
        //printf("Compute width \"%s\" char '%c' -- %i - %i = %i\n", string, c, ox1, ox0, width);
        assert(width >= 0);
        assert(ix0 >= 0);

        int oy0 = baseline - face_sans->glyph->bitmap_top;
        int height = bmp->rows;

        // Update bounding rectangle
        //min_x = min(min_x, cx0);
        max_x = max(max_x, ox1);
        min_y = max(0, min(min_y, oy0));
        max_y = min(rheight, max(max_y, oy0+height));

        // Ick?
        unsigned char *input = bmp->buffer;
        int input_pitch = bmp->pitch;
        for (int y=0 ; y<height; y++) {
            int y_out = oy0 + y;
            if ((y_out >= 0) && (y_out < rheight)) {
                uint8_t *ptr = rtmp + y_out*rwidth;
                unsigned char *row = input + ix0 + y*input_pitch;

                for (int xoff=0; xoff<(ox1-ox0); xoff++) {
                    int tmp = ptr[ox0+xoff];
                    tmp += row[xoff];
                    ptr[ox0+xoff] = clamp_byte(tmp);
                }
            } else printf("    fuck off at %i\n", y);
        }

        pen_x += face_sans->glyph->advance.x >> 6;
    }

    uint32_t *data = malloc(4*(max_x - min_x)*(max_y - min_y));

    printf("Final bounds: %i %i %i %i baseline=%i\n", 0, min_y, max_x, max_y, baseline);

    if (data == NULL) {
        printf("Can't allocate final memory for size %i label \"%s\"\n", text_height, string);
        printf("Final bounds: %i %i %i %i baseline=%i\n", 0, min_y, max_x, max_y, baseline);
        return NULL;
    } else {
        image_t img = malloc(sizeof(*img));
        assert(img);
        int w = max_x - min_x;

        for (int y=min_y; y<max_y; y++)
            for (int x=min_x; x<max_x; x++)
                data[(y-min_y)*w+(x-min_x)] = color | (rtmp[y*rwidth+x] << 24);

        img->w = w;
        img->h = max_y - min_y;
        img->x_origin = 0;
        img->y_origin = baseline - min_y;
        img->pixels = data;

//        printf("New label \"%s\" is %i x %i\n", string, img->w, img->h);
/*
        img->x_origin = -((img->w*align_x[ALIGN_MAX]) >> align_x[ALIGN_MAX_SHIFT]);
        img->y_origin = -((img->h*align_y[ALIGN_MAX]) >> align_y[ALIGN_MAX_SHIFT])
            - (min_y - baseline)*align_y[ALIGN_AXIS];
*/
        return img;
    }
}


