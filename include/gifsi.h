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

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if HAVE_CONFIG_H
#include <config.h>
#endif

/*compile the C++ version (you can disable the C++ wrapper here even when compiling for C++)*/
#ifdef __cplusplus
# define GIFSI_COMPILE_CPP
# include <string>
# include <vector>
#endif

#if WITH_FILE_IO
# define USE_FILE_IO
#endif

#define GIF_UNOPTIMIZE_SIMPLEST_DISPOSAL 1
#define GIF_MAX_CODE_BITS     12
#define GIF_MAX_CODE          0x1000
#define GIF_MAX_BLOCK         0xFF
#define GIF_MAX_SCREEN_WIDTH  0xFFFF
#define GIF_MAX_SCREEN_HEIGHT 0xFFFF
#define GIF_MAX_VALUE_UINT    0xFFFFFFFF

enum Gif_Disposal {
	GD_None = 0,
	GD_Asis,
	GD_Background,
	GD_Previous
};

typedef enum   Gif_Disposal   Gif_Disposal;
typedef struct Gif_Stream     Gif_Stream;
typedef struct Gif_Image      Gif_Image;
typedef struct Gif_Colormap   Gif_Colormap;
typedef struct Gif_Comment    Gif_Comment;
typedef struct Gif_Extension  Gif_Extension;
typedef struct Gif_Record     Gif_Record;

typedef unsigned short Gif_Code;


//  Stream class
struct Gif_Stream {
	Gif_Image **images;
	unsigned int nimages, imagescap;

	Gif_Colormap  *global;
	Gif_Comment   *end_comment;
	Gif_Extension *end_extension_list;

	unsigned short background;  /* 256 means no background */
	unsigned short screen_width, screen_height;

	int loopcount, refcount;    /* -1 means no loop count */

	unsigned int errors;
	unsigned int user_flags;

	const char* landmark;

#ifdef GIFSI_COMPILE_CPP
	~Gif_Stream();
#endif
};

//  Stream construct, copy, destroy fn declare
Gif_Stream *Gif_NewStream       (void);
Gif_Stream *Gif_NewStreamFrom   (const Gif_Stream *);
bool        Gif_CopyStream      (const Gif_Stream *, Gif_Stream *);
bool        Gif_CopyStreamImages(const Gif_Stream *, Gif_Stream *);
void        Gif_InitStream            (Gif_Stream *);
void        Gif_DeleteStream          (Gif_Stream *);

//  Stream getters/setters declare
#define Gif_ScreenHeight(gfs) (gfs->screen_height)
#define Gif_ScreenWidth(gfs)  (gfs->screen_width)
#define Gif_ImageCount(gfs)   (gfs->nimages)

//  Stream others methods declare
Gif_Image * Gif_GetImage        (const Gif_Stream *, const unsigned);
Gif_Image * Gif_GetNamedImage   (const Gif_Stream *, const char *);
int         Gif_GetImageNum     (const Gif_Stream *, const Gif_Image *);
bool        Gif_AddExtension          (Gif_Stream *, Gif_Image *, Gif_Extension *);
bool        Gif_AddImage              (Gif_Stream *, Gif_Image *);
void        Gif_RemoveImage           (Gif_Stream *, unsigned);
void        Gif_CalculateScreenSize   (Gif_Stream *, bool);
bool        Gif_FullUnoptimize        (Gif_Stream *, int);

//  Stream renamed functions
#define Gif_Unoptimize(gfs)    Gif_FullUnoptimize(gfs, 0)


//  Image class
struct Gif_Image {
	unsigned char **img;     /* img[y][x] == image byte (x,y) */
	unsigned char *image_data;

	unsigned short width, height;
	unsigned short left , top;
	unsigned short delay;
	unsigned char  interlace;

	short transparent;       /* -1 means no transparent index */
	Gif_Colormap *local;
	Gif_Disposal disposal;

	char* identifier;
	Gif_Comment* comment;
	Gif_Extension* extension_list;

	void (*free_image_data)(void *);

	unsigned int   compressed_len;
	unsigned int   compressed_errors;
	unsigned char* compressed;
	void (*free_compressed)(void *);

	unsigned int user_flags;
	int refcount;
};

//  Image construct, copy, destroy fn declare
Gif_Image * Gif_NewImage(void);
Gif_Image * Gif_NewImageFrom    (const Gif_Image *);
bool        Gif_CopyImage       (const Gif_Image *, Gif_Image *);
void        Gif_InitImage             (Gif_Image *);
void        Gif_DeleteImage           (Gif_Image *);

//  Image getters/setters declare
#define Gif_ImageDelay(gfi)         (gfi->delay)
#define Gif_ImageWidth(gfi)         (gfi->width)
#define Gif_ImageHeight(gfi)        (gfi->height)
#define Gif_ImageUserData(gfi)      (gfi->userdata)
#define Gif_SetImageUserData(gfi,v) (gfi->userdata = v)

//  Image others methods declare
int      Gif_ImageColorBound    (const Gif_Image *);
bool     Gif_ClipImage                (Gif_Image *, int, int, int, int);
bool     Gif_SetUncompressedImage     (Gif_Image *, unsigned char *, void (*free_data)(void *), bool);
bool     Gif_CreateUncompressedImage  (Gif_Image *, bool);
void     Gif_ReleaseUncompressedImage (Gif_Image *);
void     Gif_ReleaseCompressedImage   (Gif_Image *);
void     Gif_MakeImageEmpty           (Gif_Image *);


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

Gif_CompressInfo * Gif_NewCompressInfo(void);

#define Gif_UncompressImage(gfs,gfi) Gif_FullUncompressImage(gfs,gfi,NULL)

int  Gif_FullUncompressImage (Gif_Stream *, Gif_Image *, Gif_ReadErrorHandler);
int  Gif_FullCompressImage   (Gif_Stream *, Gif_Image *, const Gif_CompressInfo *);
void Gif_InitCompressInfo                                     (Gif_CompressInfo *);


//  Color object
typedef struct {
	unsigned char haspixel;  /* semantics assigned by user */
	unsigned char gfc_red;   /* red component (0-255) */
	unsigned char gfc_green; /* green component (0-255) */
	unsigned char gfc_blue;  /* blue component (0-255) */
	unsigned int  pixel;     /* semantics assigned by user */
} Gif_Color;

//  Colormap class
struct Gif_Colormap {
	int ncol, capacity;
	unsigned int user_flags;
	int refcount;
	Gif_Color *col;
};

//  Colormap construct, copy, destroy fn declare
Gif_Colormap *Gif_NewColormap     (int, int);
Gif_Colormap *Gif_NewColormapFrom (const Gif_Colormap *);
bool          Gif_FillColormap          (Gif_Colormap *, int, int);
void          Gif_InitColormap          (Gif_Colormap *);
void          Gif_DeleteColormap        (Gif_Colormap *);

//  Colormap getters/setters/eq declare
#define GIF_COLOREQ(c1, c2)(\
	(c1)->gfc_red   == (c2)->gfc_red   && \
	(c1)->gfc_green == (c2)->gfc_green && \
	(c1)->gfc_blue  == (c2)->gfc_blue )

#define GIF_SETCOLOR(c,r,g,b)(\
	(c)->gfc_red   = (r),\
	(c)->gfc_green = (g),\
	(c)->gfc_blue  = (b))

//  Colormap others methods declare
int Gif_FindColor (Gif_Colormap *, Gif_Color *);
int Gif_AddColor  (Gif_Colormap *, Gif_Color *, int look_from);


// Comment class
struct Gif_Comment {
	char **str;
	int   *len;
	int count, cap;
};

// Comment class
Gif_Comment *
     Gif_NewComment     (void);
void Gif_InitComment    (Gif_Comment *);
void Gif_DeleteComment  (Gif_Comment *);
bool Gif_AddCommentTake (Gif_Comment *, char *, int);
bool Gif_AddComment     (Gif_Comment *, const char *, int);


//  Extension class
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

Gif_Extension * Gif_NewExtension    (int, const char *, int);
Gif_Extension * Gif_NewExtensionFrom(const Gif_Extension *);
void            Gif_DeleteExtension (Gif_Extension *);

/* Optimizer */
#define GIF_OPT_MASK      0xFFFF
#define GIF_OPT_KEEPEMPTY 0x10000

#define Gif_OptimizeFragments(gfs,gfi) Gif_FullOptimizeFragments(gfs,f,h,NULL)

void Gif_FullOptimizeFragments(Gif_Stream *, int, int, Gif_CompressInfo *);


/** READING AND WRITING **/
#define GIF_READ_COMPRESSED             1
#define GIF_READ_UNCOMPRESSED           2
#define GIF_READ_CONST_RECORD           4
#define GIF_READ_TRAILING_GARBAGE_OK    8

#define GIF_WRITE_CAREFUL_MIN_CODE_SIZE 1
#define GIF_WRITE_EAGER_CLEAR           2
#define GIF_WRITE_OPTIMIZE              4
#define GIF_WRITE_SHRINK                8

void        Gif_SetErrorHandler (Gif_ReadErrorHandler);
Gif_Stream* Gif_FullReadData    (const unsigned char *, unsigned, int, const char *, Gif_ReadErrorHandler);

Gif_Stream* Gif_FullReadFile    (FILE *, int, const char *, Gif_ReadErrorHandler);
int         Gif_FullWriteFile   (Gif_Stream *, const Gif_CompressInfo *, FILE *);

#define Gif_ReadData(d,l) Gif_FullReadData (d,l, GIF_READ_UNCOMPRESSED, NULL, NULL)
#define Gif_ReadFile(f)   Gif_FullReadFile (f,   GIF_READ_UNCOMPRESSED, NULL, NULL)

#define Gif_CompressImage(s,i) Gif_FullCompressImage (s,i,NULL)
#define Gif_WriteFile(s,f)     Gif_FullWriteFile     (s,NULL,f)

typedef struct Gif_Writer Gif_Writer;
Gif_Writer* Gif_IncrementalWriteFileInit (Gif_Stream* gfs, const Gif_CompressInfo* gcinfo, FILE *f);
int         Gif_IncrementalWriteImage    (Gif_Writer* grr, Gif_Stream* gfs, Gif_Image* gfi);
int         Gif_IncrementalWriteComplete (Gif_Writer* grr, Gif_Stream* gfs);


/** HOOKS AND MISCELLANEOUS **/
unsigned Gif_InterlaceLine (unsigned, unsigned);
char   * Gif_CopyString    (const char *);
void     Gif_Free          (void *);

#define GIF_T_STREAM   0
#define GIF_T_IMAGE    1
#define GIF_T_COLORMAP 2

typedef void (*Gif_DeletionHookFunc)(int, void *, void *);

bool Gif_AddDeletionHook    (int, Gif_DeletionHookFunc, void *);
void Gif_RemoveDeletionHook (int, Gif_DeletionHookFunc, void *);

#ifdef GIF_DEBUGGING
# include <stdarg.h>
# define GIF_DEBUG(x) Gif_Debug x
void     Gif_Debug(char *x, ...);
#else
#define GIF_DEBUG(x)
#endif

/* Legacy */
#define Gif_CopyStreamSkeleton Gif_NewStreamFrom
#define Gif_NewFullColormap    Gif_NewColormap
#define Gif_CopyColormap       Gif_NewColormapFrom
#define Gif_ImageNumber        Gif_GetImageNum

#define Gif_New(t)         (              (t*) malloc(            sizeof(t)))
#define Gif_NewArray(t,n)  (    (n) > 0 ? (t*) malloc(            sizeof(t) * (n)) : NULL)
#define Gif_ReArray(p,t,n) ((p)=(n) > 0 ? (t*)realloc((void*)(p), sizeof(t) * (n)) : NULL)
#define Gif_Delete(p)                            free((void*)(p))
#define Gif_DeleteArray(p)                       free((void*)(p))

#ifdef GIFSI_COMPILE_CPP

namespace GifSI {
	class Stream : public Gif_Stream {
	  public:
	    Stream();
	    Stream(const Stream& other);
	    virtual ~Stream();
	    Stream& operator=(const Stream& other);
	};
} /* namespace GifSI */
#endif //GIFSI_COMPILE_CPP

#endif //GIFSI_H
