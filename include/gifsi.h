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
#include <stdlib.h>
#include <string.h>

#if HAVE_CONFIG_H
#include <config.h>
#endif


/*compile the C++ version (you can disable the C++ wrapper here even when compiling for C++)*/
#ifdef __cplusplus
# define GIFSI_HPP
extern "C" {
#endif

#if WITH_FILE_IO
# include <stdio.h>
#endif

#define GIF_MAX_SCREEN_WIDTH  0xFFFF
#define GIF_MAX_SCREEN_HEIGHT 0xFFFF
#define GIF_COLOR_TRANSPARENT 0x100

#define NO_COPY_GIF_IMAGES     0x01
#define NO_COPY_GIF_COLORMAP   0x02
#define NO_COPY_GIF_COMMENTS   0x04
#define NO_COPY_GIF_EXTENSIONS 0x08

#define NO_COPY_EXTENSION_APP  0x10
#define NO_COPY_EXTENSION_NEXT 0x20

//#define NO_COPY__RESERVED1__ 0x40
//#define NO_COPY__RESERVED2__ 0x80

typedef struct Gif_Stream     Gif_Stream;
typedef struct Gif_Image      Gif_Image;
typedef struct Gif_Colormap   Gif_Colormap;
typedef struct Gif_Comment    Gif_Comment;
typedef struct Gif_Extension  Gif_Extension;
typedef struct Gif_Error      Gif_Error;



/* - - - - - - - *
   Error object
 * - - - - - - - */
typedef enum {
	GE_Log = -1,
	GE_Warning,
	GE_Error,
	GE_Fatal
} Gif_eLevel;

typedef enum {
	GmE_Read       = 0xA,
	GmE_Optimize, // 0xB
	GmE_Quantize, // 0xC
	GmE_Write     // 0xD
} Gif_eModule;

typedef void (*Gif_eHandler)(Gif_Stream *, Gif_eModule, Gif_Error);

struct Gif_Error {
	Gif_eLevel  lvl;
	signed int  num;
	const char *msg;
};


/* - - - - - - - *
   Stream object
 * - - - - - - - */
struct Gif_Stream {
	Gif_Image **images;
	unsigned int imgscap;

	Gif_Error      errors;
	Gif_eHandler   handler;
	Gif_Colormap  *global;
	Gif_Comment   *end_comment;
	Gif_Extension *end_extension_list;

	unsigned short background;  /* 256 means no background */
	unsigned short screen_width, screen_height;

	bool has_local_colors;

	int loopcount, nimages; /* -1 means no loop count */

	int user_flags, refcount;
};

//  Stream init/copy/destroy
bool Gif_InitStream(Gif_Stream * gst, const char *lmark);
bool Gif_CopyStream(Gif_Stream *dest, const Gif_Stream *src, char no_copy_flags);
void Gif_FreeStream(Gif_Stream *);

//  Stream getters
#define Gif_GetLandmarker(gfs)     (gfs->errors.msg)
#define Gif_GetImagesCount(gfs)    (gfs->nimages)
#define Gif_GetImageAtRange(gfs,r) (gfs->nimages <= r          ? NULL :      gfs->images[r < 0 ? gfs->nimages + r : r])
#define Gif_GetImageAtIndex(gfs,i) (gfs->nimages <= i || i < 0 ? NULL :      gfs->images[i])
#define Gif_GetImageByName(gfs,n)  (gfs->nimages <= 0          ? NULL : !n ? gfs->images[0] : Gif_ImageByName( gfs->images, gfs->nimages, n))
#define Gif_GetIndexOfImage(gfs,m) (gfs->nimages <= 0 || !m    ?   -1                       : Gif_IndexOfImage(gfs->images, gfs->nimages, m))

//  Stream setters
#define Gif_SetErrorHandler(gst,log,h)\
	gst->errors.lvl = log,\
	gst->handler    = h

#define Gif_AddStreamExtension(gst,ex)\
	if (gst->end_extension_list)\
		(void)Gif_PutExtension(gst->end_extension_list, ex);\
	else\
		gst->end_extension_list = ex

//  Stream other methods
int  Gif_PutImage      (Gif_Stream *, Gif_Image *);
void Gif_RemoveImage   (Gif_Stream *, int  index);
void Gif_CalcScreenSize(Gif_Stream *, bool force);

//  Stream substitution macroses
#define Gif_AddImage(gst,gim) (void)Gif_PutImage(gst,gim)


//  Image class
typedef enum {
	GD_None = 0,
	GD_Asis,
	GD_Background,
	GD_Previous
} Gif_Disposal;

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

	int user_flags, refcount;
};

//  Image init/copy/destroy
bool Gif_InitImage(Gif_Image *);
bool Gif_CopyImage(Gif_Image *dest, const Gif_Image *src, char no_copy_flags);
void Gif_FreeImage(Gif_Image *);

//  Image getters
#define Gif_GetImageColorBound(gfi) (gfi->compressed && gfi->compressed[0] > 0 && gfi->compressed[0] < 8 ? 1 << gfi->compressed[0] : 256)

#define Gif_AddImageExtension(gim,ex)\
	if (gim->extension_list)\
		(void)Gif_PutExtension(gim->extension_list, ex);\
	else\
		gim->extension_list = ex

//  Image others methods declare
void Gif_ClipImage               (Gif_Image *, int, int, int, int);
void Gif_SetUncompressedImage    (Gif_Image *, bool is_interlaced, unsigned char *data);
bool Gif_CreateUncompressedImage (Gif_Image *, bool is_interlaced);
void Gif_ReleaseUncompressedImage(Gif_Image *);
void Gif_ReleaseCompressedImage  (Gif_Image *);
void Gif_MakeImageEmpty          (Gif_Image *);



//  Color object
typedef struct {
	unsigned char haspixel;  /* semantics assigned by user */
	unsigned char R, G, B;   /* RGB color component (0-255) */
	unsigned int  pixel;     /* semantics assigned by user */
} Gif_Color;

//  Colormap class
struct Gif_Colormap {
	int ncol, capacity;
	int user_flags, refcount;
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



/* - - - - - - - *
   Comment object
 * - - - - - - - */
struct Gif_Comment {
	const char  **str;
	unsigned int *len, cap;
	  signed int indents;
};

//  Comment init/copy/destroy
bool Gif_InitComment(Gif_Comment *gcom, const int icount);
bool Gif_CopyComment(Gif_Comment *dest, const Gif_Comment *src);
void Gif_FreeComment(Gif_Comment *);

#define Gif_AddComment(com,s,l) (void)Gif_CpyIndent(com, s, (l <= 0 ? strlen(s) : l))

int Gif_CatIndent(Gif_Comment *, const char *str, unsigned len);
int Gif_CpyIndent(Gif_Comment *, const char *str, unsigned len);



/* - - - - - - - - - - - - *
   Extension (list) object
 * - - - - - - - - - - - - */
struct Gif_Extension {
	short kind; // negative kinds are reserved
	const char *appname;
	unsigned char *data;
	unsigned int length, applength;
	bool packetized;

	Gif_Extension *prev, *next;
};

//  Extension init/copy/destroy
bool Gif_InitExtension(Gif_Extension *gfex, short kind, const char *name, unsigned len);
bool Gif_CopyExtension(Gif_Extension *dest, const Gif_Extension *src, char no_copy_flags);
void Gif_FreeExtension(Gif_Extension *);

//  Extension other methods
int  Gif_PutExtension (Gif_Extension *list, Gif_Extension *gfex);



/* - - - - - - - - - - - - *
   Compression settings
 * - - - - - - - - - - - - */
typedef struct {
	short flags; // write and optiz flags
	float lossy; // range 0-655.35 equals 100-0% quality
} Gif_CompressInfo;

#define GIF_OPTIZ_LVL1 0x104
#define GIF_OPTIZ_LVL2 0x304
#define GIF_OPTIZ_LVL3 0x704
// reserved flags -----0x800, 0x4000,0x8000
#define GIF_OPTIZ_SAVE_UNCOMP 0x1000
#define GIF_OPTIZ_KEEP_EMPTY  0x2000

#define GIF_UNOPT_SIMPLEST_DISPOSAL 0x100

#define Gif_Unoptimize(gst) Gif_FullUnoptimize(gst, 0)
#define Gif_Optimize(gst)   Gif_FullOptimize(gst, (Gif_CompressInfo){GIF_OPTIZ_LVL1,0})

bool Gif_FullUnoptimize(Gif_Stream *, short unopt_flags);
void Gif_FullOptimize  (Gif_Stream *, Gif_CompressInfo);

#define Gif_UncompressImage(gst,gim) Gif_FullUncompressImage(gst, gim, 0)
#define Gif_CompressImage(gst,gim)   Gif_FullCompressImage(gst, gim, (Gif_CompressInfo){0,0})

int  Gif_FullUncompressImage(Gif_Stream *, Gif_Image *, char read_flags);
void Gif_FullCompressImage  (Gif_Stream *, Gif_Image *, Gif_CompressInfo);



/* - - - - - - - - - *
   Color Transforms
 * - - - - - - - - - */
typedef struct Gif_ColorTransform Gif_ColorTransform;

/* Gamma type */
enum Gif_Gamma {
	GK_SRGB = 0,
	GK_Numeric
};
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

typedef enum Gif_Dither Gif_Dither;
typedef enum Gif_Gamma  Gif_Gamma;

struct Gif_ColorTransform {

	Gif_Dither dither_plan;
	Gif_Gamma  gamma_type;

	double gamma_range;

	const unsigned short *GammaTab, *RGammaTab;
	const unsigned char  *dpMatrix;
};

bool Gif_InitColorTransform(Gif_ColorTransform *);
void Gif_FreeColorTransform(Gif_ColorTransform *);

// sets the quantization plan with WxH matrix size and num colors in pallete
void Gif_SetDitherPlan(Gif_ColorTransform *, Gif_Dither plan, unsigned char w, unsigned char h, unsigned ncols);

// sets the gamma type and range
void Gif_SetGamma(Gif_ColorTransform *, Gif_Gamma type, double range);

typedef enum {
	CD_Flat = 0,
	CD_Blend,
	CD_MedianCut
} Gif_CDiversity;

Gif_Colormap *Gif_NewDiverseColormap(Gif_Stream *, Gif_ColorTransform *, Gif_CDiversity, unsigned *ncol);
void          Gif_FullQuantizeColors(Gif_Stream *, Gif_ColorTransform *, Gif_Colormap *new_colmap,  Gif_CompressInfo);



/** READING AND WRITING **/
#define GIF_READ_INFO_ONLY           0
#define GIF_READ_IMAGE_RAW           1
#define GIF_READ_IMAGE_RAW_CONST     3
#define GIF_READ_IMAGE_DECODED       4
#define GIF_READ_TRAILING_GARBAGE_OK 8

#define GIF_WRITE_TRUNC_PADS 1
#define GIF_WRITE_CAREFUL    2
#define GIF_WRITE_MINIMAL    4
#define GIF_WRITE_DROP_EXTRA 8

bool     Gif_FullReadData (Gif_Stream *, char read_flags , const unsigned char *data, unsigned len);
unsigned Gif_FullWriteData(Gif_Stream *, Gif_CompressInfo,       unsigned char **out);

#define Gif_ReadData(gst,d,l) Gif_FullReadData(gst,GIF_READ_IMAGE_DECODED,d,l)
#define Gif_WriteData(gst,d)  Gif_FullWriteData(gst,(Gif_CompressInfo){0,0},d)

#if WITH_FILE_IO
bool     Gif_FullReadFile (Gif_Stream *, char read_flags , FILE *);
unsigned Gif_FullWriteFile(Gif_Stream *, Gif_CompressInfo, FILE *);

#define Gif_ReadFile(gst,f)  Gif_FullReadFile(gst,GIF_READ_IMAGE_DECODED,f)
#define Gif_WriteFile(gst,f) Gif_FullWriteFile(gst,(Gif_CompressInfo){0,0},f)
#endif


/** HOOKS AND MISCELLANEOUS **/
unsigned Gif_InterlaceLine (unsigned, unsigned);
char   * Gif_CopyString    (const char *);

Gif_Image *Gif_ImageByName (Gif_Image **arr, const int len, const char *name);
int        Gif_IndexOfImage(Gif_Image **arr, const int len, const Gif_Image *img);
int        Gif_IndexOfColor(Gif_Color * arr, const int len, const Gif_Color  col);

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
#define Gif_GetImage           Gif_GetImageAtIndex
#define Gif_GetNamedImage      Gif_GetImageByName
#define Gif_ImageNumber        Gif_GetIndexOfImage

#ifdef GIFSI_HPP
# define Gif_New(_T)           new _T
# define Gif_NewArray(_T, n)   new _T[n]
# define Gif_Free(obj)         delete   obj
# define Gif_FreeArray(arr)    delete[] arr
# define Gif_ReArray(arr,_T,n) {\
	size_t cnt = (n);\
	_T *tmp = new _T *[cnt];\
	memcpy(tmp, arr, sizeof(_T) * cnt);\
	delete[] arr, arr = tmp;\
}
#else
#define Gif_New(t)         (    (t*) malloc(            sizeof(t)      ))
#define Gif_NewArray(t,n)  (    (t*) malloc(            sizeof(t) * (n)))
#define Gif_ReArray(p,t,n) (p = (t*)realloc((void*)(p), sizeof(t) * (n)))
#define Gif_Free(p)                    free((void*)(p))
#define Gif_FreeArray(p)               free((void*)(p))
#endif //GIFSI_HPP

#define Gif_NewExtension(ex,k) Gif_InitExtension(ex = Gif_New(Gif_Extension),k,NULL,0)
#define Gif_NewColormap(gcm,n) Gif_InitColormap(gcm = Gif_New(Gif_Colormap),n,256)
#define Gif_NewComment(com)    Gif_InitComment( com = Gif_New(Gif_Comment),1)
#define Gif_NewStream(gst,m)   Gif_InitStream(  gst = Gif_New(Gif_Stream),m)
#define Gif_NewImage(gim)      Gif_InitImage(   gim = Gif_New(Gif_Image))

// Delete ~ free pointer and set it to the NULL
#define Gif_DeleteArray(p)     Gif_FreeArray(p) , p = NULL
#define Gif_Delete(p)          Gif_Free(p)      , p = NULL

#define Gif_DeleteExtension    Gif_FreeExtension
#define Gif_DeleteColormap     Gif_FreeColormap
#define Gif_DeleteComment      Gif_FreeComment
#define Gif_DeleteStream       Gif_FreeStream
#define Gif_DeleteImage        Gif_FreeImage

#ifdef __cplusplus
} // extern "C"
#endif

#endif //GIFSI_H
