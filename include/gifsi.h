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
#define GIF_MAX_SCREEN_WIDTH  0xFFFF
#define GIF_MAX_SCREEN_HEIGHT 0xFFFF

#define NO_COPY_GIF_IMAGES     1
#define NO_COPY_GIF_COLORMAP   2
#define NO_COPY_GIF_COMMENTS   4
#define NO_COPY_GIF_EXTENSIONS 8

typedef struct Gif_Stream     Gif_Stream;
typedef struct Gif_Image      Gif_Image;
typedef struct Gif_Colormap   Gif_Colormap;
typedef struct Gif_Comment    Gif_Comment;
typedef struct Gif_Extension  Gif_Extension;


//  Stream class
struct Gif_Stream {
	Gif_Image **images;
	unsigned int nimages, imagescap;

	Gif_Colormap  *global;
	Gif_Comment   *end_comment;
	Gif_Extension *end_extension_list;

	unsigned short background;  /* 256 means no background */
	unsigned short screen_width, screen_height;

	bool has_local_colors;

	int loopcount, refcount;    /* -1 means no loop count */

	unsigned int errors;
	unsigned int user_flags;

	const char* landmark;

#ifdef GIFSI_COMPILE_CPP
	~Gif_Stream();
#endif
};

//  Stream init/copy/destroy
bool Gif_InitStream(Gif_Stream *);
bool Gif_CopyStream(Gif_Stream *dest, const Gif_Stream *src, char no_copy_flags);
void Gif_FreeStream(Gif_Stream *);

//  Stream getters
#define Gif_GetImagesCount(gfs)    (gfs->nimages)
#define Gif_GetImageByIndex(gfs,i) (gfs->nimages <= i || i < 0 ? NULL :      gfs->images[i])
#define Gif_GetImageByName(gfs,n)  (gfs->nimages <= 0          ? NULL : !n ? gfs->images[0] : Gif_ImageByName( gfs->images, gfs->nimages, n))
#define Gif_GetIndexOfImage(gfs,m) (gfs->nimages <= 0 || !m    ?   -1                       : Gif_IndexOfImage(gfs->images, gfs->nimages, m))

//  Stream other methods
void Gif_AddExtension  (Gif_Stream *, Gif_Image *, Gif_Extension *);
int  Gif_PutImage      (Gif_Stream *, Gif_Image *);
void Gif_RemoveImage   (Gif_Stream *, int  index);
void Gif_CalcScreenSize(Gif_Stream *, bool force);
bool Gif_FullUnoptimize(Gif_Stream *, char unopt_flags);

//  Stream substitution macroses
#define Gif_AddImage(gst,gim) (void)Gif_PutImage(gst,gim)
#define Gif_Unoptimize(gfs)    Gif_FullUnoptimize(gfs, 0)


//  Image class
typedef enum Gif_Disposal Gif_Disposal;

enum Gif_Disposal {
	GD_None = 0,
	GD_Asis,
	GD_Background,
	GD_Previous
};

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

	unsigned int   compressed_len;
	unsigned int   compressed_errors;
	unsigned char* compressed;
	void (*free_compressed)(void *);

	unsigned int user_flags;
	int refcount;
};

//  Image init/copy/destroy
bool Gif_InitImage(Gif_Image *);
bool Gif_CopyImage(Gif_Image *dest, const Gif_Image *src, char no_copy_flags);
void Gif_FreeImage(Gif_Image *);

//  Image getters
#define Gif_GetImageColorBound(gfi) (gfi->compressed && gfi->compressed[0] > 0 && gfi->compressed[0] < 8 ? 1 << gfi->compressed[0] : 256)

//  Image others methods declare
void     Gif_ClipImage                (Gif_Image *, int, int, int, int);
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
	int flags, lossy;
} Gif_CompressInfo;

Gif_CompressInfo * Gif_NewCompressInfo(void);

#define Gif_UncompressImage(gfs,gfi) Gif_FullUncompressImage(gfs,gfi,NULL)

int  Gif_FullUncompressImage (Gif_Stream *, Gif_Image *, Gif_ReadErrorHandler);
int  Gif_FullCompressImage   (Gif_Stream *, Gif_Image *, const Gif_CompressInfo *);
void Gif_InitCompressInfo                                     (Gif_CompressInfo *);


//  Color object
typedef struct {
	unsigned char haspixel;  /* semantics assigned by user */
	unsigned char R, G, B;   /* RGB color component (0-255) */
	unsigned int  pixel;     /* semantics assigned by user */
} Gif_Color;

//  Colormap class
struct Gif_Colormap {
	int ncol, capacity;
	unsigned int user_flags;
	int refcount;
	Gif_Color *col;
};

//  Colormap init/copy/destroy
bool Gif_InitColormap(Gif_Colormap *gfcm, const int ncols, int capacity);
bool Gif_CopyColormap(Gif_Colormap *dest, const Gif_Colormap *src);
void Gif_FreeColormap(Gif_Colormap *);

//  Colormap getters/setters/eq declare
#define Gif_ColorEq(col1, col2)(\
	col1.R == col2.R && \
	col1.G == col2.G && \
	col1.B == col2.B )

#define Gif_SetColor(col,r,g,b)\
	col.R = r,\
	col.G = g,\
	col.B = b

#define Gif_FindColor(cm,sc) Gif_IndexOfColor(cm->col, cm->ncol, sc)
#define Gif_AddColor(cm,pc) (void)Gif_PutColor(cm, -1, pc)

//  Colormap others methods declare
int Gif_PutColor(Gif_Colormap *, int look_from, Gif_Color);


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
};

//  Extension init/copy/destroy
bool Gif_InitExtension(Gif_Extension *gfex, const int kind, const char *name, unsigned len);
bool Gif_CopyExtension(Gif_Extension *dest, const Gif_Extension *src);
void Gif_FreeExtension(Gif_Extension *);


/* Optimizer */
#define GIF_OPT_MASK      0xFFFF
#define GIF_OPT_KEEPEMPTY 0x10000

#define Gif_OptimizeFragments(gfs,gfi) Gif_FullOptimizeFragments(gfs,f,h,NULL)

void Gif_FullOptimizeFragments(Gif_Stream *, int, int, Gif_CompressInfo *);

/* Quantization */
enum Gif_Dither {
	DiP_Posterize = 0,
	DiP_FloydSteinberg,
	DiP_3x3_Ordered,
	DiP_4x4_Ordered,
	DiP_8x8_Ordered,
	DiP_45_Diagonal,
	DiP_64x64_ReOrdered,
	DiP_SquareHalftone,
	DiP_TriangleHalftone
};

typedef struct Gif_DitherPlan Gif_DitherPlan;
typedef enum   Gif_Dither     Gif_Dither;
typedef void(*_dith_work_fn)( Gif_Image *, unsigned char *, Gif_Colormap *,
                              void *, unsigned *, Gif_DitherPlan *);

struct Gif_DitherPlan {
	Gif_Dither type;
	const unsigned char *matrix;
	_dith_work_fn  doWork;
};

void Gif_InitDitherPlan(Gif_DitherPlan *, Gif_Dither, unsigned char, unsigned char, unsigned);
void Gif_FreeDitherPlan(Gif_DitherPlan *);

#define COLORMAP_DIVERSITY_FLAT  0
#define COLORMAP_DIVERSITY_BLEND 1
#define COLORMAP_MEDIAN_CUT      2

Gif_Colormap *Gif_NewDiverseColormap(Gif_Stream *, unsigned *ncol, char alg, Gif_DitherPlan *);
void          Gif_FullQuantizeColors(Gif_Stream *, Gif_Colormap *new_colmap, Gif_DitherPlan *);


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

unsigned int Gif_FullWriteFile  (Gif_Stream *, FILE *         , Gif_CompressInfo *);
unsigned int Gif_FullWriteData  (Gif_Stream *, unsigned char *, Gif_CompressInfo *);

#define Gif_ReadData(d,l) Gif_FullReadData (d,l, GIF_READ_UNCOMPRESSED, NULL, NULL)
#define Gif_ReadFile(f)   Gif_FullReadFile (f,   GIF_READ_UNCOMPRESSED, NULL, NULL)

#define Gif_CompressImage(s,i) Gif_FullCompressImage (s,i,NULL)
#define Gif_WriteFile(s,f)     Gif_FullWriteFile     (s,f,NULL)
#define Gif_WriteData(s,d)     Gif_FullWriteData     (s,d,NULL)


/** HOOKS AND MISCELLANEOUS **/
unsigned Gif_InterlaceLine (unsigned, unsigned);
char   * Gif_CopyString    (const char *);
void     Gif_Free          (void *);

Gif_Image *Gif_ImageByName (Gif_Image **arr, const int len, const char *name);
int        Gif_IndexOfImage(Gif_Image **arr, const int len, const Gif_Image *img);
int        Gif_IndexOfColor(Gif_Color * arr, const int len, const Gif_Color  col);

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
#define Gif_CalculateScreenSize Gif_CalcScreenSize
#define Gif_ImageCount         Gif_GetImagesCount
#define Gif_GetImage           Gif_GetImageByIndex
#define Gif_GetNamedImage      Gif_GetImageByName
#define Gif_ImageNumber        Gif_GetIndexOfImage

#define Gif_New(t)         (    (t*) malloc(            sizeof(t)      ))
#define Gif_NewArray(t,n)  (    (t*) malloc(            sizeof(t) * (n)))
#define Gif_ReArray(p,t,n) (p = (t*)realloc((void*)(p), sizeof(t) * (n)))
#define Gif_Delete(p)                  free((void*)(p))
#define Gif_DeleteArray(p)             free((void*)(p))

#define Gif_NewExtension(ex,k) Gif_InitExtension(ex = Gif_New(Gif_Extension),k,NULL,0)
#define Gif_NewColormap(gcm,n) Gif_InitColormap(gcm = Gif_New(Gif_Colormap),n,256)
#define Gif_NewStream(gst)     Gif_InitStream(  gst = Gif_New(Gif_Stream))
#define Gif_NewImage(gim)      Gif_InitImage(   gim = Gif_New(Gif_Image))

#define Gif_DeleteExtension    Gif_FreeExtension
#define Gif_DeleteColormap     Gif_FreeColormap
#define Gif_DeleteStream       Gif_FreeStream
#define Gif_DeleteImage        Gif_FreeImage

#ifdef GIFSI_COMPILE_CPP

# define Gif_New(_T_)           (new _T_)
# define Gif_NewArray(_T_, res) (new std::vector<_T_>(res))

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
