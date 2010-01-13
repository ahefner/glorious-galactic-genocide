#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <SDL/SDL.h>
#include <vorbis/vorbisfile.h>

int sound_initialized = 0;

static void audio_mixer (void *udata, Sint16 *stream, int len);

#define MAX_SOUND_EFFECTS 32

static struct effect
{
    Sint16 *data;
    int position;
    int length;                 /* Zero length indicates unused effect stream */
} effects[MAX_SOUND_EFFECTS];

static struct song
{
    int init;
    char *filename;
    OggVorbis_File file;
    int playing;
} songs[1];

void music_init (void)
{
    memset(songs, 0, sizeof(songs));

    songs[0].filename = "/tmp/01.ogg";

    for (int i=0; i<(sizeof(songs) / sizeof(songs[0])); i++) {
        if (ov_fopen(songs[i].filename, &songs[i].file)) {
            fprintf(stderr, "Error opening track %i: %s\n", songs[i].filename);
        } else {
            songs[i].init = 1;
        }
    }

    //songs[0].playing = 1;
}

void music_shutdown (void)
{
    for (int i=0; i<(sizeof(songs) / sizeof(songs[0])); i++) {
        if (songs[i].init) {
            songs[i].init = 0;
            ov_clear(&songs[i].file);
        }
    }
}

int audio_init (void)
{
    memset(effects, 0, sizeof(effects));

    SDL_AudioSpec tmp;
    //SDL_LoadWAV("sfx/gong.wav", &tmp, &effects[0].data, &effects[0].length);
    //effects[0].length /= 2;
    effects[0].length = 0;

    SDL_AudioSpec desired, obtained;

    desired.freq = 48000;
    desired.format = AUDIO_S16;
    desired.samples = 4096;
    desired.callback = audio_mixer;
    desired.channels = 2;
    desired.userdata = NULL;

    if (SDL_OpenAudio(&desired, &obtained)) {
        printf("SDL_OpenAudio failed: %s", SDL_GetError());
        sound_initialized = 0;
        return 0;
    } else {
        sound_initialized = 1;
        printf("Audio init: freq=%i, buffer size=%i, channels=%i\n",
               obtained.freq, obtained.samples, obtained.channels);
        SDL_PauseAudio(0);

        music_init();

        return 1;
    }
}

void audio_shutdown (void)
{    
    if (sound_initialized) SDL_CloseAudio();
    music_shutdown();
}

void play_sound_effect (Sint16 *samples, int length)
{
    if (!sound_initialized) return;
    assert(length >= 0);
    for (int i=0; i<MAX_SOUND_EFFECTS; i++) {
        if (0 == effects[i].length) {
            effects[i].data = samples;
            effects[i].position = 0;
            effects[i].length = length;
            return;
        }
    }
    
    fprintf(stderr, "Too many sounds playing.\n");
}

static void audio_mixer (void *udata, Sint16 *stream, int len)
{
    Sint16 buffer[2048];
    int numfx = 0;
    int nsamples = len / 2;
    memset(stream, 0, len);

    /* Mix sound effects */
    for (int i=0; i<MAX_SOUND_EFFECTS; i++) {
        if (effects[i].length) {
            //printf("%i: %i / %i\n", i, effects[i].position, effects[i].length);
            numfx++;
            int playing = effects[i].length - effects[i].position;
            if (playing > nsamples) playing = nsamples;
            Sint16 *ptr = effects[i].data + effects[i].position;
            effects[i].position += playing;
            for (int j=0; j<playing; j++) stream[j] += ptr[j];
            /* Don't retire sounds until after mixing, otherwise we race with the UI thread. */
            if (effects[i].position == effects[i].length) effects[i].length = 0;
        }
    }

    /* Play songs */
    for (int i=0; i<sizeof(songs)/sizeof(songs[0]); i++) {
        if (songs[i].init && songs[i].playing) {
            int mixpos = 0;
            int need_bytes = len;
            while (need_bytes) {
                int sect = 0;       /* Who gives a shit! */
                int nread = ov_read(&songs[i].file, buffer, 
                                    (need_bytes>sizeof(buffer))? sizeof(buffer) : need_bytes, 
                                    0 /* little endian */, 2 /* 16-bit */, 1 /* Signed */, &sect);
                assert(nread <= need_bytes);
                if (nread < 0) {
                    fprintf(stderr, "Track %i: Ogg error %i\n", i, nread);
                    break;
                }
                else if (nread == 0) {
                    songs[i].playing = 0;
                    fprintf(stderr, "Track %i: End of file.\n", i);
                    break;
                } else {
                    need_bytes -= nread;
                    nread /= 2; /* Convert bytes to samples */
                    //fprintf(stderr, "read %i at %i\n", nread, mixpos);                    
                    for (int idx=0; idx < nread; idx++) stream[mixpos+idx] += buffer[idx];                        
                    mixpos += nread;
                }
            }
        }
    }

    //printf("Mixed %i effects.\n", numfx);
}
