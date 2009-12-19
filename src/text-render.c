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
int freetype_init = 0;

static FT_Face faces[5];

static int load_face (int index, char *filename)
{
    if (FT_New_Face(ftlibrary, filename, 0, &faces[index])) {
        fprintf(stderr, "Error opening %s\n", filename);
        return 0;
    }

    return 1;
}

static int ensure_freetype (void)
{
    char *filename;
    if (!freetype_init) {
        freetype_init = -1;
        int error = FT_Init_FreeType(&ftlibrary);

        if (error) {
            fprintf(stderr, "Unable to initialize freetype.\n");
            return -1;
        }

        freetype_init = 
            load_face(0, "data/Vera.ttf") &&
            load_face(1, "data/VeraBd.ttf") &&
            load_face(2, "data/VeraIt.ttf") &&
            load_face(3, "data/VeraBI.ttf") &&
            load_face(4, "data/URWGothicL-Book.ttf");

/*
        for (int i=0; i<sizeof(faces)/sizeof(faces[0]); i++) 
        {
            FT_Select_Charmap(faces[i], FT_ENCODING_ADOBE_LATIN_1);
        }
*/


    }

    return freetype_init;
}

struct cached_glyph
{
    int cached, width, height, bitmap_left, bitmap_top, pitch, advance;
    unsigned char *buffer;
};

struct cached_face
{
    unsigned facenum, text_height;
    struct cached_glyph glyphs[256];
};

static struct cached_face cached_faces[32];
static int num_cached_faces = 0;

static struct cached_face *
ensure_cached_face (unsigned facenum, unsigned text_height)
{
    for (int i=0; i<num_cached_faces;i++) {
        if ((cached_faces[i].facenum == facenum) && 
            (cached_faces[i].text_height == text_height)) return cached_faces+i;
    }
    
    assert(num_cached_faces < sizeof(cached_faces) / sizeof(cached_faces[0]));

    struct cached_face *f = cached_faces + num_cached_faces;
    num_cached_faces++;

    f->facenum = facenum;
    f->text_height = text_height;
    memset(f->glyphs, 0, sizeof(f->glyphs));

    return f;
}

static struct cached_glyph *
ensure_cached_glyph (struct cached_face *cface, unsigned char code, FT_UInt index)
{
    if (!cface->glyphs[code].cached) 
    {
        struct cached_glyph *gl = &cface->glyphs[code];
        FT_Face face = faces[cface->facenum];
        FT_Set_Pixel_Sizes(face, 0, cface->text_height);
        int error = FT_Load_Glyph(face, index, 
                              FT_LOAD_RENDER | 
                              FT_LOAD_NO_HINTING | 
                              FT_LOAD_TARGET_LIGHT);
        if (error) return NULL;
        
        gl->bitmap_left = face->glyph->bitmap_left;
        gl->bitmap_top = face->glyph->bitmap_top;
        gl->pitch = face->glyph->bitmap.pitch;
        gl->width = face->glyph->bitmap.width;
        gl->height = face->glyph->bitmap.rows;
        gl->advance = face->glyph->advance.x >> 6;

        if (gl->width * gl->height) {
            gl->buffer = malloc(gl->width * gl->height);
            memcpy(gl->buffer, face->glyph->bitmap.buffer, gl->width * gl->height);
        }
        
        gl->cached = 1;
    }

    return &cface->glyphs[code];
}

image_t render_label (unsigned facenum, uint32_t color, unsigned text_height, char *string)
{
    static uint8_t *rtmp = NULL;
    int rwidth = 4096;          /* KLUDGE */
    static unsigned tmpheight = 0;
    static unsigned rheight = 0;
    int baseline = text_height + 4;
    int first_char = 1;
    int min_x = 0;
    int min_y = baseline;
    int max_y = baseline + 1;
    int max_x = 1;

    if (!ensure_freetype()) {
        fprintf(stderr, "Unable to load fonts.\n");
        exit(1);
    }

    FT_Face face = faces[facenum];
    struct cached_face *cface = ensure_cached_face(facenum, text_height);
    assert(cface != NULL);

    assert(text_height > 0);
    if (text_height > tmpheight) {
        if (rtmp) free(rtmp);
        // This is what you might call a hack.
        rheight = (text_height*2 + 4);
        rtmp = calloc(1, rwidth * rheight);
        assert(rtmp != NULL);
        tmpheight = text_height;
    } else memset(rtmp, 0, rwidth * rheight);

    FT_Set_Pixel_Sizes(face, 0, text_height);

    int pen_x = 0;
    FT_UInt last_glyph_index = 0;

    unsigned char *str = string;
    for (unsigned char c = *str++; c; c=*str++) {
        // Beg Freetype to render the glyph
        FT_UInt glyph_index = FT_Get_Char_Index(face, c);

        if (first_char) first_char = 0;
        else {
            FT_Vector delta;
            FT_Get_Kerning(face, last_glyph_index, glyph_index, 
                           FT_KERNING_DEFAULT, &delta);
            pen_x += delta.x >> 6;
        }

        last_glyph_index = glyph_index;

        struct cached_glyph *glyph = ensure_cached_glyph(cface, c, glyph_index);
        if (!glyph) continue;

        //FT_Bitmap *bmp = &glyph->bitmap;
        /*
        printf("  x=%3i: Char %c:   %ix%i + %i + %i   advance=%f\n",
               pen_x, c, bmp->width, bmp->rows,
               glyph->bitmap_left,
               glyph->bitmap_top,
               ((float)glyph->advance.x) / 64.0);
        */        

        /* Transfer the glyph to the temporary buffer.. */
        int ox0 = pen_x + glyph->bitmap_left;
        int ix0 = 0;
        int ox1 = ox0 + glyph->width;
        int ix1 = glyph->width;

        // Clip to left edge
        if (ox0 < 0) {
            //printf("PRECLIP %s\n", string);
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

        int oy0 = baseline - glyph->bitmap_top;
        int height = glyph->height;

        // Update bounding rectangle
        max_x = max(max_x, ox1);
        min_y = max(0, min(min_y, oy0));
        max_y = min(rheight, max(max_y, oy0+height));

        // Ick?
        unsigned char *input = glyph->buffer;
        int input_pitch = glyph->pitch;
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
            }
        }

        pen_x += glyph->advance;
    }

    uint32_t *data = malloc(4*(max_x - min_x)*(max_y - min_y));

    //printf("Final bounds: %i %i %i %i baseline=%i\n", 0, min_y, max_x, max_y, baseline);

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


