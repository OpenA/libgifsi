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
bool Gif_InitStream(Gif_Stream *gfs)
{
	if (!gfs)
		return false;
	gfs->images = NULL;
	gfs->nimages = gfs->imagescap = 0;
	gfs->global = NULL;
	gfs->background = 256;
	gfs->screen_width = gfs->screen_height = 0;
	gfs->loopcount = -1;
	gfs->end_comment = NULL;
	gfs->end_extension_list = NULL;
	gfs->has_local_colors = false;
	gfs->errors = 0;
	gfs->user_flags = 0;
	gfs->refcount = 0;
	gfs->landmark = NULL;
	return true;
}

bool Gif_CopyStream(Gif_Stream *dest, const Gif_Stream *src, char no_copy_flags)
{
	if (!src || !Gif_InitStream(dest))
		return false;

	dest->background    = src->background;
	dest->screen_width  = src->screen_width;
	dest->screen_height = src->screen_height;
	dest->loopcount     = src->loopcount;
	dest->errors        = src->errors;
	dest->user_flags    = src->user_flags;
	dest->refcount      = src->refcount;
	dest->landmark      = Gif_CopyString(src->landmark);

	if (!(no_copy_flags & NO_COPY_GIF_COLORMAP) && src->global) {
		if (!(dest->global = Gif_CopyColormap(src->global)))
			return false;
	}
	if (!(no_copy_flags & NO_COPY_GIF_IMAGES) && src->nimages > 0) {
		Gif_Image *last_gfi;
		if (!(dest->images = Gif_NewArray(Gif_Image *, src->imagescap)))
			return false;
		for (int i = 0; i < src->nimages; i++) {
			last_gfi = Gif_New(Gif_Image);
			if (!Gif_CopyImage(last_gfi, src->images[i], no_copy_flags))
				return false;
			dest->images[i] = last_gfi;
		}
		dest->end_comment        = last_gfi->comment;
		dest->end_extension_list = last_gfi->extension_list;
		dest->has_local_colors   = src->has_local_colors;
		dest->nimages            = src->nimages;
		dest->imagescap          = src->imagescap;
	}
	return true;
}


/*
  Image constructor functions
*/
bool Gif_InitImage(Gif_Image *gfi)
{
	if (!gfi)
		return false;
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
	gfi->compressed_len = 0;
	gfi->compressed_errors = 0;
	gfi->compressed = NULL;
	gfi->free_compressed = NULL;
	gfi->refcount = 0;
	return true;
}

bool Gif_CopyImage(Gif_Image *dest, const Gif_Image *src, char no_copy_flags)
{
	if (!src || !Gif_InitImage(dest))
		return false;

	dest->transparent = src->transparent;
	dest->delay       = src->delay;
	dest->disposal    = src->disposal;
	dest->left        = src->left;
	dest->top         = src->top;
	dest->width       = src->width;
	dest->height      = src->height;
	dest->interlace   = src->interlace;
	dest->user_flags  = src->user_flags;
	dest->refcount    = src->refcount;
	dest->identifier  = Gif_CopyString(src->identifier);

	if (!(no_copy_flags & NO_COPY_GIF_COMMENTS) && src->comment) {
		if (!(dest->comment = Gif_NewComment()))
			return false;
		for (int i = 0; i < src->comment->count; i++) {
			if (!Gif_AddComment(dest->comment, src->comment->str[i], src->comment->len[i]))
				return false;
		}
	}
	if (!(no_copy_flags & NO_COPY_GIF_EXTENSIONS) && src->extension_list) {
		Gif_Extension* gfex = src->extension_list;
		while (gfex) {
			Gif_Extension* dstex = Gif_NewExtensionFrom(gfex);
			if (!(dstex && Gif_AddExtension(NULL, dest, dstex)))
				return false;
			gfex = gfex->next;
		}
	}
	if (!(no_copy_flags & NO_COPY_GIF_COLORMAP) && src->local) {
		if (!(dest->local = Gif_CopyColormap(src->local)))
			return false;
	}
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
		dest->img[dest->height] = 0;
	}
	if (src->compressed) {
		dest->compressed = Gif_NewArray(unsigned char, src->compressed_len);
		if (!dest->compressed)
			return false;
		dest->free_compressed = Gif_Free;
		memcpy(dest->compressed, src->compressed, src->compressed_len);
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
		if (!Gif_InitColormap(gfcm, count, capacity)) {
			Gif_DeleteColormap(gfcm);
			return NULL;
		}
	}
	return gfcm;
}

bool Gif_InitColormap(Gif_Colormap *gfcm, int count, int capacity) {
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
Gif_CopyColormap(const Gif_Colormap *src) {
	if (!src)
		return NULL;
	Gif_Colormap *dest = Gif_NewColormap(src->ncol, src->capacity);
	if (!dest)
		return NULL;
	memcpy(dest->col, src->col, sizeof(src->col[0]) * src->ncol);
	return dest;
}

int Gif_IndexOfColor(Gif_Color *arr, const int len, const Gif_Color col)
{
	for (int i = 0; i < len; i++) {
		if (Gif_ColorEq(arr[i], col))
			return i;
	}
	return -1;
}

int Gif_PutColor(Gif_Colormap *gfcm, int look_from, Gif_Color c)
{
	const int midx = gfcm->ncol;
	if (look_from >= 0 && look_from < midx) {
		while (look_from < midx) {
			if (Gif_ColorEq(gfcm->col[look_from], c))
				return look_from;
			look_from++;
		}
	}
	if (midx >= gfcm->capacity) {
		if (!Gif_ReArray(gfcm->col, Gif_Color, gfcm->capacity *= 2))
			return -1;
	}
	gfcm->col[midx] = c;
	gfcm->ncol++;
	return midx;
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
	gcinfo->flags = gcinfo->lossy = 0;
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
Gif_ImageByName(Gif_Image **arr, const int len, const char *name)
{
	for (int i = 0; i < len; i++) {
		if (!arr[i]->identifier)
			continue;
		if (!strcmp(arr[i]->identifier, name))
			return arr[i];
	}
	return NULL;
}

int Gif_IndexOfImage(Gif_Image **arr, const int len, const Gif_Image *img)
{
	for (int i = 0; i < len; i++) {
		if (arr[i] == img)
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
	for (unsigned j = inum; j < gfs->nimages - 1; j++)
		gfs->images[j] = gfs->images[j + 1];
	gfs->nimages--;
}

void Gif_CalculateScreenSize(Gif_Stream *gfs, bool force)
{
	unsigned screen_width = 0, screen_height = 0;

	for (unsigned i = 0; i < gfs->nimages; i++) {
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
void Gif_ClipImage(Gif_Image *gfi, int left, int top, int width, int height)
{
	int new_width = gfi->width, new_height = gfi->height;

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
	return true;
}

bool Gif_CreateUncompressedImage(Gif_Image *gfi, bool data_interlaced) {
	size_t size = gfi->width && gfi->height ? (size_t)gfi->width * (size_t)gfi->height : 1;
	unsigned char *data = Gif_NewArray(unsigned char, size);
	return Gif_SetUncompressedImage(gfi, data, Gif_Free, data_interlaced);
}

void Gif_ReleaseUncompressedImage(Gif_Image *gfi) {
	Gif_DeleteArray(gfi->img);
	if (gfi->image_data)
		Gif_Delete(gfi->image_data);
	gfi->img = NULL;
	gfi->image_data = NULL;
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
void Gif_FreeStream(Gif_Stream *gfs)
{
	DELETE_HOOK(gfs, GIF_T_STREAM);

	for (unsigned i = 0; i < gfs->nimages; i++)
		Gif_DeleteImage(gfs->images[i]);

	Gif_DeleteArray   (gfs->images);
	Gif_DeleteColormap(gfs->global);
	Gif_DeleteComment (gfs->end_comment);

	while (gfs->end_extension_list)
		Gif_DeleteExtension(gfs->end_extension_list);

	Gif_Delete(gfs);
}

void Gif_FreeImage(Gif_Image *gfi)
{
	DELETE_HOOK(gfi, GIF_T_IMAGE);

	Gif_DeleteArray(gfi->identifier);
	Gif_DeleteComment(gfi->comment);

	while (gfi->extension_list)
		Gif_DeleteExtension(gfi->extension_list);

	Gif_DeleteColormap(gfi->local);

	if (gfi->image_data)
		Gif_Delete(gfi->image_data);

	Gif_DeleteArray(gfi->img);

	if (gfi->compressed && gfi->free_compressed)
		(*gfi->free_compressed)((void *)gfi->compressed);

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
