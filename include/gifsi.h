/* GifSI - the C/C++ library API for working with GIF images.

   Originally, based on LCDF GIF library https://www.lcdf.org/gifsicle
    ~ Copyright (C) 1997-2017 Eddie Kohler, ekohler@gmail.com
    ~ Modified by OpenA special for https://github.com/OpenA/gifsi

   The GIF library is free software. It is distributed under the GNU General
   Public License, version 2; you can copy, distribute, or alter it at will,
   as long as this notice is kept intact and this source code is made
   available. There is no warranty, express or implied. */

#ifndef GIFSI_H
#define GIFSI_H

#include <stdio.h>
#include <stdlib.h>

#if HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define GIF_UNOPTIMIZE_SIMPLEST_DISPOSAL 1
#define GIF_MAX_CODE_BITS     12
#define GIF_MAX_CODE          0x1000
#define GIF_MAX_BLOCK         0xFF
#define GIF_MAX_SCREEN_WIDTH  0xFFFF
#define GIF_MAX_SCREEN_HEIGHT 0xFFFF

#define GIF_DISPOSAL_NONE       0
#define GIF_DISPOSAL_ASIS       1
#define GIF_DISPOSAL_BACKGROUND 2
#define GIF_DISPOSAL_PREVIOUS   3

typedef struct Gif_Stream     Gif_Stream;
typedef struct Gif_Image      Gif_Image;
typedef struct Gif_Colormap   Gif_Colormap;
typedef struct Gif_Comment    Gif_Comment;
typedef struct Gif_Extension  Gif_Extension;
typedef struct Gif_Record     Gif_Record;

typedef unsigned short Gif_Code;


/** GIF_STREAM **/
struct Gif_Stream {
	Gif_Image **images;
	int nimages, imagescap;

	Gif_Colormap *global;
	unsigned short background;  /* 256 means no background */

	unsigned short screen_width, screen_height;
	long loopcount;       /* -1 means no loop count */

	Gif_Comment* end_comment;
	Gif_Extension* end_extension_list;

	unsigned int errors;
	unsigned int user_flags;

	const char* landmark;
	int refcount;
};

Gif_Stream * Gif_NewStream(void);
Gif_Stream * Gif_CopyStreamSkeleton (Gif_Stream *);
Gif_Stream * Gif_CopyStreamImages   (Gif_Stream *);
void         Gif_DeleteStream       (Gif_Stream *);

#define Gif_ScreenHeight(gfs) (gfs->screen_height)
#define Gif_ScreenWidth(gfs)  (gfs->screen_width)
#define Gif_ImageCount(gfs)   (gfs->nimages)

void Gif_CalculateScreenSize (Gif_Stream *, int force);
int  Gif_Unoptimize          (Gif_Stream *);
int  Gif_FullUnoptimize      (Gif_Stream *, int flags);


/** GIF_IMAGE **/
struct Gif_Image {
	unsigned char **img;     /* img[y][x] == image byte (x,y) */
	unsigned char *image_data;

	unsigned short width, height;
	unsigned short left , top;
	unsigned short delay;
	unsigned char  disposal, interlace;

	short transparent;          /* -1 means no transparent index */
	Gif_Colormap *local;

	char* identifier;
	Gif_Comment* comment;
	Gif_Extension* extension_list;

	void (*free_image_data)(void *);

	unsigned int   compressed_len;
	unsigned int   compressed_errors;
	unsigned char* compressed;
	void (*free_compressed)(void *);

	unsigned int user_flags;
	void* user_data;
	void (*free_user_data)(void *);
	int refcount;
};

Gif_Image * Gif_NewImage(void);
Gif_Image * Gif_CopyImage       (Gif_Image *gfi);
void        Gif_DeleteImage     (Gif_Image *gfi);
void        Gif_MakeImageEmpty  (Gif_Image* gfi);
int         Gif_ImageColorBound (const Gif_Image* gfi);

int         Gif_AddImage      (Gif_Stream *gfs, Gif_Image *gfi);
void        Gif_RemoveImage   (Gif_Stream *gfs, int i);
Gif_Image * Gif_GetImage      (Gif_Stream *gfs, int i);
Gif_Image * Gif_GetNamedImage (Gif_Stream *gfs, const char *name);
int         Gif_ImageNumber   (Gif_Stream *gfs, Gif_Image *gfi);

#define Gif_ImageDelay(gfi)         (gfi->delay)
#define Gif_ImageWidth(gfi)         (gfi->width)
#define Gif_ImageHeight(gfi)        (gfi->height)
#define Gif_ImageUserData(gfi)      (gfi->userdata)
#define Gif_SetImageUserData(gfi,v) (gfi->userdata = v)
#define Gif_UncompressImage(gfs,gfi) Gif_FullUncompressImage(gfs,gfi,0)

typedef void (*Gif_ReadErrorHandler)(
	Gif_Stream* gfs,
	Gif_Image*  gfi,
	int is_error,
	const char* error_text
);

typedef struct {
	int flags, loss;
	void *padding[7];
} Gif_CompressInfo;

int  Gif_FullUncompressImage      (Gif_Stream* gfs, Gif_Image* gfi, Gif_ReadErrorHandler handler);
int  Gif_FullCompressImage        (Gif_Stream *gfs, Gif_Image *gfi, const Gif_CompressInfo *gcinfo);

void Gif_ReleaseUncompressedImage (Gif_Image *gfi);
void Gif_ReleaseCompressedImage   (Gif_Image *gfi);
int  Gif_SetUncompressedImage     (Gif_Image *gfi, unsigned char *data, void (*free_data)(void *), int data_interlaced);
int  Gif_CreateUncompressedImage  (Gif_Image* gfi, int data_interlaced);
int  Gif_ClipImage                (Gif_Image *gfi, int l, int t, int w, int h);

void Gif_InitCompressInfo         (Gif_CompressInfo *gcinfo);

/** GIF_COLORMAP **/
typedef struct {
	unsigned char haspixel;  /* semantics assigned by user */
	unsigned char gfc_red;   /* red component (0-255) */
	unsigned char gfc_green; /* green component (0-255) */
	unsigned char gfc_blue;  /* blue component (0-255) */
	unsigned int  pixel;     /* semantics assigned by user */
} Gif_Color;

struct Gif_Colormap {
	int ncol, capacity;
	unsigned int user_flags;
	int refcount;
	Gif_Color *col;
};

Gif_Colormap * Gif_NewColormap     (void);
Gif_Colormap * Gif_NewFullColormap (int count, int capacity);
void           Gif_DeleteColormap  (Gif_Colormap *);
Gif_Colormap * Gif_CopyColormap    (Gif_Colormap *);
int            Gif_ColorEq         (Gif_Color *, Gif_Color *);

#define GIF_COLOREQ(c1, c2)(\
	(c1)->gfc_red   == (c2)->gfc_red   && \
	(c1)->gfc_green == (c2)->gfc_green && \
	(c1)->gfc_blue  == (c2)->gfc_blue )

#define GIF_SETCOLOR(c,r,g,b)(\
	(c)->gfc_red   = (r),\
	(c)->gfc_green = (g),\
	(c)->gfc_blue  = (b))

int Gif_FindColor (Gif_Colormap *, Gif_Color *);
int Gif_AddColor  (Gif_Colormap *, Gif_Color *, int look_from);

/** GIF_COMMENT **/
struct Gif_Comment {
    char **str;
    int *len;
    int count;
    int cap;
};

Gif_Comment *
     Gif_NewComment     (void);
void Gif_DeleteComment  (Gif_Comment *);
int  Gif_AddCommentTake (Gif_Comment *, char *, int);
int  Gif_AddComment     (Gif_Comment *, const char *, int);


/** GIF_EXTENSION **/
struct Gif_Extension {
	int kind; /* negative kinds are reserved */
	char* appname;
	int applength;
	unsigned char* data;
	unsigned int length;
	int packetized;

	Gif_Stream *stream;
	Gif_Image *image;
	Gif_Extension *next;
	void (*free_data)(void *);
};

Gif_Extension*  Gif_NewExtension    (int kind, const char* appname, int applength);
void            Gif_DeleteExtension (Gif_Extension* gfex);
Gif_Extension*  Gif_CopyExtension   (Gif_Extension* gfex);
int             Gif_AddExtension    (Gif_Stream* gfs, Gif_Image* gfi, Gif_Extension* gfex);


/** READING AND WRITING **/
struct Gif_Record {
	const unsigned char *data;
	unsigned int length;
};

#define GIF_READ_COMPRESSED             1
#define GIF_READ_UNCOMPRESSED           2
#define GIF_READ_CONST_RECORD           4
#define GIF_READ_TRAILING_GARBAGE_OK    8

#define GIF_WRITE_CAREFUL_MIN_CODE_SIZE 1
#define GIF_WRITE_EAGER_CLEAR           2
#define GIF_WRITE_OPTIMIZE              4
#define GIF_WRITE_SHRINK                8

void        Gif_SetErrorHandler (Gif_ReadErrorHandler handler);
Gif_Stream* Gif_FullReadFile    (FILE* f, int flags, const char* landmark, Gif_ReadErrorHandler handler);
Gif_Stream* Gif_FullReadRecord  (const Gif_Record* record, int flags, const char* landmark,  Gif_ReadErrorHandler handler);
int         Gif_FullWriteFile   (Gif_Stream *gfs, const Gif_CompressInfo *gcinfo, FILE *f);

#define Gif_ReadRecord(r) Gif_FullReadRecord (r,GIF_READ_UNCOMPRESSED,0,0)
#define Gif_ReadFile(f)   Gif_FullReadFile   (f,GIF_READ_UNCOMPRESSED,0,0)

#define Gif_CompressImage(s,i) Gif_FullCompressImage (s,i,0)
#define Gif_WriteFile(s,f)     Gif_FullWriteFile     (s,0,f)

typedef struct Gif_Writer Gif_Writer;
Gif_Writer* Gif_IncrementalWriteFileInit (Gif_Stream* gfs, const Gif_CompressInfo* gcinfo, FILE *f);
int         Gif_IncrementalWriteImage    (Gif_Writer* grr, Gif_Stream* gfs, Gif_Image* gfi);
int         Gif_IncrementalWriteComplete (Gif_Writer* grr, Gif_Stream* gfs);


/** HOOKS AND MISCELLANEOUS **/
int    Gif_InterlaceLine (int y, int height);
char * Gif_CopyString    (const char *);

#define GIF_T_STREAM   0
#define GIF_T_IMAGE    1
#define GIF_T_COLORMAP 2

typedef void (*Gif_DeletionHookFunc)(int, void *, void *);

int  Gif_AddDeletionHook    (int, Gif_DeletionHookFunc, void *);
void Gif_RemoveDeletionHook (int, Gif_DeletionHookFunc, void *);

#ifdef GIF_DEBUGGING
# define GIF_DEBUG(x) Gif_Debug x
void     Gif_Debug(char *x, ...);
#else
#define GIF_DEBUG(x)
#endif

void* Gif_Realloc (void* p, size_t s, size_t n, const char* file, int line);
void  Gif_Free    (void* p);

#ifndef  Gif_New
# define Gif_New(t)         (    (t*) Gif_Realloc((void*) 0, sizeof(t), 1, __FILE__, __LINE__))
# define Gif_NewArray(t,n)  (    (t*) Gif_Realloc((void*) 0, sizeof(t), n, __FILE__, __LINE__))
# define Gif_ReArray(p,t,n) ((p)=(t*) Gif_Realloc((void*) p, sizeof(t), n, __FILE__, __LINE__))
# define Gif_Delete(p)                Gif_Free   ((void*) p)
# define Gif_DeleteArray(p)           Gif_Free   ((void*) p)
#endif

#ifdef __cplusplus
}
#endif

#endif //GIFSI_H
