/* giffunc.c - General functions for the GIF library.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of the LCDF GIF library.

   The LCDF GIF library is free software. It is distributed under the GNU
   General Public License, version 2; you can copy, distribute, or alter it at
   will, as long as this notice is kept intact and this source code is made
   available. There is no warranty, express or implied. */

#include <gifsi.h>

/*
  Stream constructor functions
*/
Gif_Stream *
Gif_NewStream(void) {
	Gif_Stream *gfs = Gif_New(Gif_Stream);
	if (gfs != NULL)
		Gif_InitStream(gfs);
	return gfs;
}

void Gif_InitStream(Gif_Stream *gfs)
{
	gfs->images = NULL;
	gfs->nimages = gfs->imagescap = 0;
	gfs->global = NULL;
	gfs->background = 256;
	gfs->screen_width = gfs->screen_height = 0;
	gfs->loopcount = -1;
	gfs->end_comment = NULL;
	gfs->end_extension_list = NULL;
	gfs->errors = 0;
	gfs->user_flags = 0;
	gfs->refcount = 0;
	gfs->landmark = NULL;
}

Gif_Stream *
Gif_NewStreamFrom(const Gif_Stream *src) {
	Gif_Stream *dest = Gif_New(Gif_Stream);
	if (dest != NULL) {
		if (!Gif_CopyStream(src, dest)) {
			Gif_DeleteStream(dest);
			return NULL;
		}
	}
	return dest;
}

bool Gif_CopyStream(const Gif_Stream *src, Gif_Stream *dest)
{
	dest->background    = src->background;
	dest->screen_width  = src->screen_width;
	dest->screen_height = src->screen_height;
	dest->loopcount     = src->loopcount;
	if (!(dest->global  = Gif_NewColormapFrom(src->global)))
		return false;
	return true;
}

bool Gif_CopyStreamImages(const Gif_Stream *src, Gif_Stream *dest)
{
	for (int i = 0; i < src->nimages; i++) {
		Gif_Image *gfi = Gif_NewImageFrom(src->images[i]);
		if (!(gfi && Gif_AddImage(dest, gfi)))
			return false;
	}
	return true;
}


/*
  Image constructor functions
*/
Gif_Image *
Gif_NewImage(void) {
	Gif_Image *gfi = Gif_New(Gif_Image);
	if (gfi != NULL)
		Gif_InitImage(gfi);
	return gfi;
}

void Gif_InitImage(Gif_Image *gfi)
{
	gfi->width = gfi->height = 0;
	gfi->img = NULL;
	gfi->image_data = NULL;
	gfi->left = gfi->top = 0;
	gfi->delay = 0;
	gfi->disposal = GD_None;
	gfi->interlace = 0;
	gfi->local = NULL;
	gfi->transparent = -1;
	gfi->user_flags = 0;
	gfi->identifier = NULL;
	gfi->comment = NULL;
	gfi->extension_list = NULL;
	gfi->free_image_data = Gif_Free;
	gfi->compressed_len = 0;
	gfi->compressed_errors = 0;
	gfi->compressed = NULL;
	gfi->free_compressed = NULL;
	gfi->user_data = NULL;
	gfi->free_user_data = NULL;
	gfi->refcount = 0;
}

Gif_Image *
Gif_NewImageFrom(const Gif_Image *src) {
	if (!src)
		return NULL;
	Gif_Image *dest = Gif_NewImage();
	if (!(dest && Gif_CopyImage(src, dest))) {
		Gif_DeleteImage(dest);
		return NULL;
	}
	return dest;
}

bool Gif_CopyImage(const Gif_Image *src, Gif_Image *dest)
{
	if (src->identifier && !(dest->identifier = Gif_CopyString(src->identifier)))
		return false;
	if (src->comment) {
		if (!(dest->comment = Gif_NewComment()))
			return false;
		for (int i = 0; i < src->comment->count; i++) {
			if (!Gif_AddComment(dest->comment, src->comment->str[i], src->comment->len[i]))
				return false;
		}
	}
	if (src->extension_list) {
		Gif_Extension* gfex = src->extension_list;
		while (gfex) {
			Gif_Extension* dstex = Gif_NewExtensionFrom(gfex);
			if (!(dstex && Gif_AddExtension(NULL, dest, dstex)))
				return false;
			gfex = gfex->next;
		}
	}
	if (src->local && !(dest->local = Gif_NewColormapFrom(src->local)))
		return false;

	dest->transparent = src->transparent;
	dest->delay       = src->delay;
	dest->disposal    = src->disposal;
	dest->left        = src->left;
	dest->top         = src->top;
	dest->width       = src->width;
	dest->height      = src->height;
	dest->interlace   = src->interlace;

	if (src->img) {
		dest->img = Gif_NewArray(unsigned char *, dest->height + 1);
		dest->image_data = Gif_NewArray(unsigned char, (size_t)dest->width * (size_t)dest->height);
		unsigned char *data = dest->image_data;
		if (!dest->img || !dest->image_data)
			return false;
		for (int i = 0; i < dest->height; i++) {
			memcpy(data, src->img[i], dest->width);
			dest->img[i] = data;
			data += dest->width;
		}
		dest->free_image_data   = Gif_Free;
		dest->img[dest->height] = 0;
	}
	if (src->compressed) {
		if (src->free_compressed == 0) {
			dest->compressed = src->compressed;
		} else {
			dest->compressed = Gif_NewArray(unsigned char, src->compressed_len);
			dest->free_compressed = Gif_Free;
			memcpy(dest->compressed, src->compressed, src->compressed_len);
		}
		dest->compressed_len = src->compressed_len;
		dest->compressed_errors = src->compressed_errors;
	}
	return true;
}


/*
  Colormap constructor functions & methods
*/
Gif_Colormap *
Gif_NewColormap(int count, int capacity) {
	if (capacity <= 0 || count < 0)
		return NULL;

	Gif_Colormap *gfcm = Gif_New(Gif_Colormap);
	if (gfcm != NULL) {
		if (!Gif_FillColormap(gfcm, count, capacity)) {
			Gif_DeleteColormap(gfcm);
			return NULL;
		}
	}
	return gfcm;
}

void Gif_InitColormap(Gif_Colormap *gfcm)
{
	gfcm->capacity = gfcm->ncol = 0;
	gfcm->refcount = gfcm->user_flags = 0;
	gfcm->col = NULL;
}

bool Gif_FillColormap(Gif_Colormap *gfcm, int count, int capacity) {
	if (count > capacity)
		capacity    = count;
	gfcm->ncol      = count;
	gfcm->capacity  = capacity;
	gfcm->refcount  = gfcm->user_flags = 0;
	if (!(gfcm->col = Gif_NewArray(Gif_Color, capacity)))
		return false;
	return true;
}

Gif_Colormap *
Gif_NewColormapFrom(const Gif_Colormap *src) {
	if (!src)
		return NULL;
	Gif_Colormap *dest = Gif_NewColormap(src->ncol, src->capacity);
	if (!dest)
		return NULL;
	memcpy(dest->col, src->col, sizeof(src->col[0]) * src->ncol);
	return dest;
}

int Gif_FindColor(Gif_Colormap *gfcm, Gif_Color *c)
{
	for (int i = 0; i < gfcm->ncol; i++) {
		if(GIF_COLOREQ(&gfcm->col[i], c))
			return i;
	}
	return -1;
}

int Gif_AddColor(Gif_Colormap *gfcm, Gif_Color *c, int look_from)
{
	const int ncols = gfcm->ncol;
	if (look_from >= 0) {
		for (int i = look_from; i < ncols; i++) {
			if (GIF_COLOREQ(&gfcm->col[i], c))
				return i;
		}
	}
	if (ncols >= gfcm->capacity) {
		Gif_ReArray(gfcm->col, Gif_Color, (gfcm->capacity *= 2));
		if (!gfcm->col)
			return -1;
	}
	gfcm->col[ncols] = *c;
	gfcm->ncol++;
	return ncols;
}


/*
  Comment constructor functions & methods
*/
Gif_Comment *
Gif_NewComment(void) {
	Gif_Comment *gfcom = Gif_New(Gif_Comment);
	if (gfcom != NULL)
		Gif_InitComment(gfcom);
	return gfcom;
}

void Gif_InitComment(Gif_Comment *gfcom)
{
	gfcom->str = NULL;
	gfcom->len = NULL;
	gfcom->count = gfcom->cap = 0;
}

bool Gif_AddCommentTake(Gif_Comment *gfcom, char *str, int len)
{
	int count = gfcom->count,
	      cap = gfcom->cap;
	if (count >= cap) {
		gfcom->cap = cap = (cap ? cap * 2 : 2);
		Gif_ReArray(gfcom->str, char *, cap);
		Gif_ReArray(gfcom->len, int   , cap);
		if (!gfcom->str || !gfcom->len)
			return false;
	}
	gfcom->str[ count ] = str;
	gfcom->len[ count ] = len < 0 ? strlen(str) : len;
	gfcom->count++;
	return true;
}

bool Gif_AddComment(Gif_Comment *gfcom, const char *str, int len)
{
	if (len < 0)
		len = strlen(str);

	char *comm = Gif_NewArray(char, len);
	if ( !comm )
		return false;

	memcpy(comm, str, len);

	if (!Gif_AddCommentTake(gfcom, comm, len)) {
		Gif_DeleteArray(comm);
		return false;
	}
	return true;
}


/*
  Extension constructor functions
*/
Gif_Extension *
Gif_NewExtension(int kind, const char* appname, int applength)
{
	Gif_Extension *gfex = Gif_New(Gif_Extension);

	if (gfex != NULL) { /* Gif_InitExtension */
		if(appname && (gfex->appname = Gif_NewArray(char, applength + 1))) {
			memcpy(gfex->appname, appname, applength);
			gfex->appname[applength] = '\0';
			gfex->applength = applength;
		} else {
			gfex->appname = NULL;
			gfex->applength = 0;
		}
		gfex->kind = kind;
		gfex->data = NULL;
		gfex->stream = NULL;
		gfex->image = NULL;
		gfex->next = NULL;
		gfex->free_data = NULL;
		gfex->packetized = 0;
	}
	return gfex;
}

Gif_Extension *
Gif_NewExtensionFrom(const Gif_Extension* src)
{
	Gif_Extension* dest = Gif_NewExtension(src->kind, src->appname, src->applength);
	if (dest != NULL) { /* Gif_CopyExtension */
		if (!src->data || !src->free_data) {
			dest->data   = src->data;
			dest->length = src->length;
		} else {
			if (!(dest->data = Gif_NewArray(unsigned char, src->length))){
				Gif_DeleteExtension(dest);
				return NULL;
			}
			memcpy(dest->data, src->data, src->length);
			dest->length     = src->length;
			dest->free_data  = Gif_Free;
		}
		dest->packetized = src->packetized;
	}
	return dest;
}

/*
  CompressInfo constructor functions & methods
*/
Gif_CompressInfo *
Gif_NewCompressInfo(void) {
	Gif_CompressInfo *gcinfo = Gif_New(Gif_CompressInfo);
	if (gcinfo != NULL)
		Gif_InitCompressInfo(gcinfo);
	return gcinfo;
}

void Gif_InitCompressInfo(Gif_CompressInfo *gcinfo)
{
	gcinfo->flags = gcinfo->loss = 0;
}


/* MISC FUNCTIONS */
char *Gif_CopyString(const char *str)
{
	if (str != NULL) {
		int   len   = strlen(str) + 1;
		char *copy  = Gif_NewArray(char, len);
		if (  copy != NULL) {
			memcpy(copy, str, len);
			return copy;
		}
	}
	return NULL;
}

unsigned Gif_InterlaceLine(unsigned line, unsigned height)
{
	height--;
	return (
		line > height / 2 ? line * 2 - ( height       | 1) :
		line > height / 4 ? line * 4 - ((height & ~1) | 2) :
		line > height / 8 ? line * 8 - ((height & ~3) | 4) :
	line * 8);
}

void Gif_Free(void* ptr) {
	free(ptr);
}


/*
  Stream methods & working functions
*/
Gif_Image *
Gif_GetImage(const Gif_Stream *gfs, const unsigned inum)
{
	return inum < gfs->nimages ? gfs->images[inum] : NULL;
}

Gif_Image *
Gif_GetNamedImage(const Gif_Stream *gfs, const char *name)
{
	if (!name)
		return gfs->nimages ? gfs->images[0] : NULL;
	for (int i = 0; i < gfs->nimages; i++) {
		if (!gfs->images[i]->identifier)
			continue;
		if (!strcmp(gfs->images[i]->identifier, name))
			return gfs->images[i];
	}
	return NULL;
}

int Gif_GetImageNum(const Gif_Stream *gfs, const Gif_Image *gfi)
{
	if (!gfs || !gfi)
		return -1;
	for (int i = 0; i < gfs->nimages; i++) {
		if (gfs->images[i] == gfi)
			return i;
	}
	return -1;
}

bool Gif_AddExtension(Gif_Stream* gfs, Gif_Image* gfi, Gif_Extension* gfex)
{
	if (gfex->stream || gfex->image)
		return false;
	Gif_Extension **pprev = (
		gfi ? &gfi->extension_list : &gfs->end_extension_list);
	while(*pprev)
	       pprev = &(*pprev)->next;
	      *pprev = gfex;
	gfex->stream = gfs;
	gfex->image  = gfi;
	gfex->next   = NULL;
	return true;
}

bool Gif_AddImage(Gif_Stream *gfs, Gif_Image *gfi)
{
	if (gfs->nimages  >= gfs->imagescap) {
		gfs->imagescap = gfs->imagescap ? gfs->imagescap * 2 : 2;
		Gif_ReArray(gfs->images, Gif_Image *, gfs->imagescap);
		if (!gfs->images)
			return false;
	}
	gfs->images[gfs->nimages] = gfi;
	gfs->nimages++, gfi->refcount++;
	return true;
}

void Gif_RemoveImage(Gif_Stream *gfs, unsigned inum)
{
	if (inum >= gfs->nimages)
		return;
	Gif_DeleteImage(gfs->images[inum]);
	for (int j = inum; j < gfs->nimages - 1; j++)
		gfs->images[j] = gfs->images[j + 1];
	gfs->nimages--;
}

void Gif_CalculateScreenSize(Gif_Stream *gfs, bool force)
{
	unsigned screen_width = 0, screen_height = 0;

	for (int i = 0; i < gfs->nimages; i++) {
		Gif_Image *gfi = gfs->images[i];
		/* 17.Dec.1999 - I find this old behavior annoying. */
		/* if (gfi->left != 0 || gfi->top != 0) continue; */
		if (screen_width < gfi->left + gfi->width)
			screen_width = gfi->left + gfi->width;
		if (screen_height < gfi->top + gfi->height)
			screen_height = gfi->top + gfi->height;
	}
	/* Only use the default 640x480 screen size if we are being forced to create
		a new screen size or there's no screen size currently. */
	if (screen_width == 0 && (gfs->screen_width == 0 || force))
		screen_width = 640;
	if (screen_height == 0 && (gfs->screen_height == 0 || force))
		screen_height = 480;

	if (gfs->screen_width < screen_width || force)
		gfs->screen_width = screen_width;
	if (gfs->screen_height < screen_height || force)
		gfs->screen_height = screen_height;
}


/*
  Image methods & working functions
*/
int Gif_ImageColorBound(const Gif_Image* gfi) {
	unsigned char compss = gfi->compressed ? gfi->compressed[0] : 0;
	return (compss > 0 && compss < 8 ? 1 << compss : 256);
}

bool Gif_ClipImage(Gif_Image *gfi, int left, int top, int width, int height)
{
	int new_width = gfi->width, new_height = gfi->height;

	if (!gfi->img)
		return false;
	if (gfi->left < left) {
		int shift = left - gfi->left;
		for (int y = 0; y < gfi->height; y++)
			gfi->img[y] += shift;
		gfi->left += shift;
		new_width -= shift;
	}
	if (gfi->top < top) {
		int shift = top - gfi->top,
		        y = gfi->height - 1;
		for (; y >= shift; y++)
			gfi->img[y - shift] = gfi->img[y];
		gfi->top   += shift;
		new_height -= shift;
	}
	if (gfi->left + new_width >= width)
		new_width = width - gfi->left;
	if (gfi->top + new_height >= height)
		new_height = height - gfi->top;

	gfi->width  = new_width  < 0 ? 0 : new_width;
	gfi->height = new_height < 0 ? 0 : new_height;
	return true;
}

bool Gif_SetUncompressedImage(Gif_Image *gfi, unsigned char *image_data,
                         void (*free_data)(void *), bool data_interlaced) {
	/* NB does not affect compressed image (and must not) */
	unsigned i;
	unsigned width  = gfi->width;
	unsigned height = gfi->height;
	unsigned char **img;

	Gif_ReleaseUncompressedImage(gfi);

	if (!image_data)
		return false;
	if (!(img = Gif_NewArray(unsigned char *, height + 1)))
		return false;

	if (data_interlaced){
		for (i = 0; i < height; i++)
			img[ Gif_InterlaceLine(i, height) ] = image_data + width * i;
	} else {
		for (i = 0; i < height; i++)
			img[i] = image_data + width * i;
	}
	img[height] = 0;
	gfi->img = img;
	gfi->image_data = image_data;
	gfi->free_image_data = free_data;
	return true;
}

bool Gif_CreateUncompressedImage(Gif_Image *gfi, bool data_interlaced) {
	size_t sz = (size_t)gfi->width * (size_t)gfi->height;
	unsigned char *data = Gif_NewArray(unsigned char, sz ? sz : 1);
	return Gif_SetUncompressedImage(gfi, data, Gif_Free, data_interlaced);
}

void Gif_ReleaseUncompressedImage(Gif_Image *gfi) {
	Gif_DeleteArray(gfi->img);
	if (gfi->image_data && gfi->free_image_data)
		(*gfi->free_image_data)(gfi->image_data);
	gfi->img = NULL;
	gfi->image_data = NULL;
	gfi->free_image_data = NULL;
}

void Gif_ReleaseCompressedImage(Gif_Image *gfi) {
	if (gfi->compressed && gfi->free_compressed)
		(*gfi->free_compressed)(gfi->compressed);
	gfi->compressed        = NULL;
	gfi->compressed_len    = 0;
	gfi->compressed_errors = 0;
	gfi->free_compressed   = NULL;
}

void Gif_MakeImageEmpty(Gif_Image* gfi) {
	Gif_ReleaseUncompressedImage(gfi);
	Gif_ReleaseCompressedImage(gfi);
	gfi->width = gfi->height = 1;
	gfi->transparent = 0;
	Gif_CreateUncompressedImage(gfi, 0);
	gfi->img[0][0] = 0;
}


/* DELETION HOOKS */
typedef struct Gif_DeletionHook {
	int kind;
	Gif_DeletionHookFunc func;
	void *callback_data;
	struct Gif_DeletionHook *next;
} Gif_DeletionHook;

static Gif_DeletionHook *all_hooks;

#define DELETE_HOOK(obj, _TYPE_) \
	if (!obj || --obj->refcount > 0) \
		return; \
	for (Gif_DeletionHook *hook = all_hooks; hook; hook = hook->next) { \
		if (hook->kind == _TYPE_) \
			(*hook->func)(_TYPE_, obj, hook->callback_data); \
	}


bool Gif_AddDeletionHook(int kind, void (*func)(int, void *, void *), void *cb)
{
	Gif_DeletionHook *hook = Gif_New(Gif_DeletionHook);
	if (!hook)
		return false;
	Gif_RemoveDeletionHook(kind, func, cb);
	hook->callback_data = cb;
	hook->kind = kind;
	hook->func = func;
	hook->next = all_hooks;
	all_hooks  = hook;
	return true;
}

void Gif_RemoveDeletionHook(int kind, void (*func)(int, void *, void *), void *cb)
{
	Gif_DeletionHook *hook = all_hooks, *prev = NULL;
	while  (hook) {
		if (hook->kind == kind &&
			hook->func == func &&
			hook->callback_data == cb
		) {
			if (prev)
				prev->next = hook->next;
			else
				all_hooks  = hook->next;
			Gif_Delete(hook);
			return;
		}
		prev = hook;
		hook = hook->next;
	}
}


/* 
  Class Deconstructors
*/
void Gif_DeleteStream(Gif_Stream *gfs)
{
	DELETE_HOOK(gfs, GIF_T_STREAM);

	for (int i = 0; i < gfs->nimages; i++)
		Gif_DeleteImage(gfs->images[i]);

	Gif_DeleteArray   (gfs->images);
	Gif_DeleteColormap(gfs->global);
	Gif_DeleteComment (gfs->end_comment);

	while (gfs->end_extension_list)
		Gif_DeleteExtension(gfs->end_extension_list);

	Gif_Delete(gfs);
}

void Gif_DeleteImage(Gif_Image *gfi)
{
	DELETE_HOOK(gfi, GIF_T_IMAGE);

	Gif_DeleteArray(gfi->identifier);
	Gif_DeleteComment(gfi->comment);

	while (gfi->extension_list)
		Gif_DeleteExtension(gfi->extension_list);

	Gif_DeleteColormap(gfi->local);

	if (gfi->image_data && gfi->free_image_data)
		(*gfi->free_image_data)((void *)gfi->image_data);

	Gif_DeleteArray(gfi->img);

	if (gfi->compressed && gfi->free_compressed)
		(*gfi->free_compressed)((void *)gfi->compressed);
	if (gfi->user_data && gfi->free_user_data)
		(*gfi->free_user_data)(gfi->user_data);

	Gif_Delete(gfi);
}

void Gif_DeleteColormap(Gif_Colormap *gfcm)
{
	DELETE_HOOK(gfcm, GIF_T_COLORMAP);
	Gif_DeleteArray(gfcm->col);
	Gif_Delete(gfcm);
}

void Gif_DeleteComment(Gif_Comment *gfcom)
{
	if (!gfcom)
		return;
	for (int i = 0; i < gfcom->count; i++)
		Gif_DeleteArray(gfcom->str[i]);
	Gif_DeleteArray(gfcom->str);
	Gif_DeleteArray(gfcom->len);
	Gif_Delete(gfcom);
}

void Gif_DeleteExtension(Gif_Extension *gfex)
{
	if (!gfex)
		return;
	if (gfex->data && gfex->free_data)
		(*gfex->free_data)(gfex->data);

	Gif_DeleteArray(gfex->appname);

	if (gfex->stream || gfex->image) {
		Gif_Extension** pprev = (
			gfex->image ? &gfex->image->extension_list : &gfex->stream->end_extension_list
		);
		while(*pprev  && *pprev != gfex)
		       pprev = &(*pprev)->next;
		if   (*pprev)
		      *pprev = gfex->next;
	}
	Gif_Delete(gfex);
}

#ifdef GIF_DEBUGGING
void Gif_Debug(char *x, ...) {
	va_list val;
	va_start(val, x);
	vfprintf(stderr, x, val);
	va_end(val);
}
#endif

#ifdef GIFSI_COMPILE_CPP
namespace GifSI {
	Stream::Stream() {
		Gif_InitStream(&this);
	}
	Stream::Stream(const Stream& other) {
		Gif_CopyStream(other, &this);
	}
	Stream::~Stream() {
		Gif_DeleteStream(&this);
	}
	Stream& Stream::operator=(const Stream& other) {
		Gif_CopyStream(other, &this);
		return *this;
	}
} /* namespace GifSI */
#endif //GIFSI_COMPILE_CPP
