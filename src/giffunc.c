/* giffunc.c - General functions for the GIF library.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of the LCDF GIF library.

   The LCDF GIF library is free software. It is distributed under the GNU
   General Public License, version 2; you can copy, distribute, or alter it at
   will, as long as this notice is kept intact and this source code is made
   available. There is no warranty, express or implied. */

#include <gifsi.h>

/* Gif Stream functions */
extern void Gif_InitStream(Gif_Stream *gfs);
extern bool Gif_CopyStream(const Gif_Stream *src, Gif_Stream *dest);

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


/* Gif Image functions */
extern void Gif_InitImage(Gif_Image *gfi);
extern bool Gif_CopyImage(const Gif_Image *src, Gif_Image *dest);

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


/* Gif Colormap functions */
extern void Gif_InitColormap(Gif_Colormap *gfcm);
extern bool Gif_FillColormap(Gif_Colormap *gfcm, int count, int capacity);

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


/* Gif Comment functions */
extern void Gif_InitComment(Gif_Comment *gfcom);

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


/* Gif Extension functions */
// void Gif_InitExtension(Gif_Extension *gfex, int kind, const char* appname, int applength);
// bool Gif_CopyExtension(const Gif_Extension *src, Gif_Extension *dest);

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

int
Gif_AddImage(Gif_Stream *gfs, Gif_Image *gfi)
{
  if (gfs->nimages >= gfs->imagescap) {
    if (gfs->imagescap)
      gfs->imagescap *= 2;
    else
      gfs->imagescap = 2;
    Gif_ReArray(gfs->images, Gif_Image *, gfs->imagescap);
    if (!gfs->images)
      return 0;
  }
  gfs->images[gfs->nimages] = gfi;
  gfs->nimages++;
  gfi->refcount++;
  return 1;
}


void
Gif_RemoveImage(Gif_Stream *gfs, int inum)
{
  int j;
  if (inum < 0 || inum >= gfs->nimages)
    return;
  Gif_DeleteImage(gfs->images[inum]);
  for (j = inum; j < gfs->nimages - 1; j++)
    gfs->images[j] = gfs->images[j+1];
  gfs->nimages--;
}


int
Gif_ImageColorBound(const Gif_Image* gfi)
{
    if (gfi->compressed && gfi->compressed[0] > 0 && gfi->compressed[0] < 8)
        return 1 << gfi->compressed[0];
    else
        return 256;
}


int
Gif_AddCommentTake(Gif_Comment *gfcom, char *x, int xlen)
{
  if (gfcom->count >= gfcom->cap) {
    if (gfcom->cap)
      gfcom->cap *= 2;
    else
      gfcom->cap = 2;
    Gif_ReArray(gfcom->str, char *, gfcom->cap);
    Gif_ReArray(gfcom->len, int, gfcom->cap);
    if (!gfcom->str || !gfcom->len)
      return 0;
  }
  if (xlen < 0)
    xlen = strlen(x);
  gfcom->str[ gfcom->count ] = x;
  gfcom->len[ gfcom->count ] = xlen;
  gfcom->count++;
  return 1;
}


int
Gif_AddComment(Gif_Comment *gfcom, const char *x, int xlen)
{
  char *new_x;
  if (xlen < 0)
    xlen = strlen(x);
  new_x = Gif_NewArray(char, xlen);
  if (!new_x)
    return 0;
  memcpy(new_x, x, xlen);
  if (Gif_AddCommentTake(gfcom, new_x, xlen) == 0) {
    Gif_DeleteArray(new_x);
    return 0;
  } else
    return 1;
}


int
Gif_AddExtension(Gif_Stream* gfs, Gif_Image* gfi, Gif_Extension* gfex)
{
    Gif_Extension **pprev;
    if (gfex->stream || gfex->image)
        return 0;
    pprev = gfi ? &gfi->extension_list : &gfs->end_extension_list;
    while (*pprev)
        pprev = &(*pprev)->next;
    *pprev = gfex;
    gfex->stream = gfs;
    gfex->image = gfi;
    gfex->next = 0;
    return 1;
}


int
Gif_ImageNumber(Gif_Stream *gfs, Gif_Image *gfi)
{
    int i;
    if (gfs && gfi)
        for (i = 0; i != gfs->nimages; ++i)
            if (gfs->images[i] == gfi)
                return i;
    return -1;
}


void
Gif_CalculateScreenSize(Gif_Stream *gfs, int force)
{
  int i;
  int screen_width = 0;
  int screen_height = 0;

  for (i = 0; i < gfs->nimages; i++) {
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


Gif_Stream *
Gif_CopyStreamImages(Gif_Stream *gfs)
{
  Gif_Stream *ngfs = Gif_CopyStreamSkeleton(gfs);
  int i;
  if (!ngfs)
    return 0;
  for (i = 0; i < gfs->nimages; i++) {
    Gif_Image *gfi = Gif_NewImageFrom(gfs->images[i]);
    if (!gfi || !Gif_AddImage(ngfs, gfi)) {
      Gif_DeleteStream(ngfs);
      return 0;
    }
  }
  return ngfs;
}


void Gif_MakeImageEmpty(Gif_Image* gfi) {
    Gif_ReleaseUncompressedImage(gfi);
    Gif_ReleaseCompressedImage(gfi);
    gfi->width = gfi->height = 1;
    gfi->transparent = 0;
    Gif_CreateUncompressedImage(gfi, 0);
    gfi->img[0][0] = 0;
}


/** DELETION **/

typedef struct Gif_DeletionHook {
  int kind;
  Gif_DeletionHookFunc func;
  void *callback_data;
  struct Gif_DeletionHook *next;
} Gif_DeletionHook;

static Gif_DeletionHook *all_hooks;

void
Gif_DeleteStream(Gif_Stream *gfs)
{
  Gif_DeletionHook *hook;
  int i;
  if (!gfs || --gfs->refcount > 0)
    return;

  for (i = 0; i < gfs->nimages; i++)
    Gif_DeleteImage(gfs->images[i]);
  Gif_DeleteArray(gfs->images);

  Gif_DeleteColormap(gfs->global);

  Gif_DeleteComment(gfs->end_comment);
  while (gfs->end_extension_list)
      Gif_DeleteExtension(gfs->end_extension_list);

  for (hook = all_hooks; hook; hook = hook->next)
    if (hook->kind == GIF_T_STREAM)
      (*hook->func)(GIF_T_STREAM, gfs, hook->callback_data);
  Gif_Delete(gfs);
}


void
Gif_DeleteImage(Gif_Image *gfi)
{
  Gif_DeletionHook *hook;
  if (!gfi || --gfi->refcount > 0)
    return;

  for (hook = all_hooks; hook; hook = hook->next)
    if (hook->kind == GIF_T_IMAGE)
      (*hook->func)(GIF_T_IMAGE, gfi, hook->callback_data);

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


void
Gif_DeleteColormap(Gif_Colormap *gfcm)
{
  Gif_DeletionHook *hook;
  if (!gfcm || --gfcm->refcount > 0)
    return;

  for (hook = all_hooks; hook; hook = hook->next)
    if (hook->kind == GIF_T_COLORMAP)
      (*hook->func)(GIF_T_COLORMAP, gfcm, hook->callback_data);

  Gif_DeleteArray(gfcm->col);
  Gif_Delete(gfcm);
}


void
Gif_DeleteComment(Gif_Comment *gfcom)
{
  int i;
  if (!gfcom)
    return;
  for (i = 0; i < gfcom->count; i++)
    Gif_DeleteArray(gfcom->str[i]);
  Gif_DeleteArray(gfcom->str);
  Gif_DeleteArray(gfcom->len);
  Gif_Delete(gfcom);
}


void
Gif_DeleteExtension(Gif_Extension *gfex)
{
  if (!gfex)
    return;
  if (gfex->data && gfex->free_data)
    (*gfex->free_data)(gfex->data);
  Gif_DeleteArray(gfex->appname);
  if (gfex->stream || gfex->image) {
      Gif_Extension** pprev;
      if (gfex->image)
          pprev = &gfex->image->extension_list;
      else
          pprev = &gfex->stream->end_extension_list;
      while (*pprev && *pprev != gfex)
          pprev = &(*pprev)->next;
      if (*pprev)
          *pprev = gfex->next;
  }
  Gif_Delete(gfex);
}


/** DELETION HOOKS **/

int
Gif_AddDeletionHook(int kind, void (*func)(int, void *, void *), void *cb)
{
  Gif_DeletionHook *hook = Gif_New(Gif_DeletionHook);
  if (!hook)
    return 0;
  Gif_RemoveDeletionHook(kind, func, cb);
  hook->kind = kind;
  hook->func = func;
  hook->callback_data = cb;
  hook->next = all_hooks;
  all_hooks = hook;
  return 1;
}

void
Gif_RemoveDeletionHook(int kind, void (*func)(int, void *, void *), void *cb)
{
  Gif_DeletionHook *hook = all_hooks, *prev = 0;
  while (hook) {
    if (hook->kind == kind && hook->func == func
        && hook->callback_data == cb) {
      if (prev)
        prev->next = hook->next;
      else
        all_hooks = hook->next;
      Gif_Delete(hook);
      return;
    }
    prev = hook;
    hook = hook->next;
  }
}


int
Gif_ColorEq(Gif_Color *c1, Gif_Color *c2)
{
  return GIF_COLOREQ(c1, c2);
}


int
Gif_FindColor(Gif_Colormap *gfcm, Gif_Color *c)
{
  int i;
  for (i = 0; i < gfcm->ncol; i++)
    if (GIF_COLOREQ(&gfcm->col[i], c))
      return i;
  return -1;
}


int
Gif_AddColor(Gif_Colormap *gfcm, Gif_Color *c, int look_from)
{
  int i;
  if (look_from >= 0)
    for (i = look_from; i < gfcm->ncol; i++)
      if (GIF_COLOREQ(&gfcm->col[i], c))
        return i;
  if (gfcm->ncol >= gfcm->capacity) {
    gfcm->capacity *= 2;
    Gif_ReArray(gfcm->col, Gif_Color, gfcm->capacity);
    if (gfcm->col == 0)
      return -1;
  }
  i = gfcm->ncol;
  gfcm->ncol++;
  gfcm->col[i] = *c;
  return i;
}


Gif_Image *
Gif_GetImage(Gif_Stream *gfs, int imagenumber)
{
  if (imagenumber >= 0 && imagenumber < gfs->nimages)
    return gfs->images[imagenumber];
  else
    return 0;
}


Gif_Image *
Gif_GetNamedImage(Gif_Stream *gfs, const char *name)
{
  int i;

  if (!name)
    return gfs->nimages ? gfs->images[0] : 0;

  for (i = 0; i < gfs->nimages; i++)
    if (gfs->images[i]->identifier &&
        strcmp(gfs->images[i]->identifier, name) == 0)
      return gfs->images[i];

  return 0;
}


void
Gif_ReleaseCompressedImage(Gif_Image *gfi)
{
  if (gfi->compressed && gfi->free_compressed)
    (*gfi->free_compressed)(gfi->compressed);
  gfi->compressed = 0;
  gfi->compressed_len = 0;
  gfi->compressed_errors = 0;
  gfi->free_compressed = 0;
}

void
Gif_ReleaseUncompressedImage(Gif_Image *gfi)
{
  Gif_DeleteArray(gfi->img);
  if (gfi->image_data && gfi->free_image_data)
    (*gfi->free_image_data)(gfi->image_data);
  gfi->img = 0;
  gfi->image_data = 0;
  gfi->free_image_data = 0;
}


int
Gif_ClipImage(Gif_Image *gfi, int left, int top, int width, int height)
{
  int new_width = gfi->width, new_height = gfi->height;
  int y;

  if (!gfi->img)
    return 0;

  if (gfi->left < left) {
    int shift = left - gfi->left;
    for (y = 0; y < gfi->height; y++)
      gfi->img[y] += shift;
    gfi->left += shift;
    new_width -= shift;
  }

  if (gfi->top < top) {
    int shift = top - gfi->top;
    for (y = gfi->height - 1; y >= shift; y++)
      gfi->img[y - shift] = gfi->img[y];
    gfi->top += shift;
    new_height -= shift;
  }

  if (gfi->left + new_width >= width)
    new_width = width - gfi->left;

  if (gfi->top + new_height >= height)
    new_height = height - gfi->top;

  if (new_width < 0)
    new_width = 0;
  if (new_height < 0)
    new_height = 0;
  gfi->width = new_width;
  gfi->height = new_height;
  return 1;
}


int
Gif_InterlaceLine(int line, int height)
{
  height--;
  if (line > height / 2)
    return line * 2 - ( height       | 1);
  else if (line > height / 4)
    return line * 4 - ((height & ~1) | 2);
  else if (line > height / 8)
    return line * 8 - ((height & ~3) | 4);
  else
    return line * 8;
}


int
Gif_SetUncompressedImage(Gif_Image *gfi, uint8_t *image_data,
                         void (*free_data)(void *), int data_interlaced)
{
  /* NB does not affect compressed image (and must not) */
  unsigned i;
  unsigned width = gfi->width;
  unsigned height = gfi->height;
  uint8_t **img;

  Gif_ReleaseUncompressedImage(gfi);
  if (!image_data)
    return 0;

  img = Gif_NewArray(uint8_t *, height + 1);
  if (!img)
    return 0;

  if (data_interlaced)
    for (i = 0; i < height; i++)
      img[ Gif_InterlaceLine(i, height) ] = image_data + width * i;
  else
    for (i = 0; i < height; i++)
      img[i] = image_data + width * i;
  img[height] = 0;

  gfi->img = img;
  gfi->image_data = image_data;
  gfi->free_image_data = free_data;
  return 1;
}

int
Gif_CreateUncompressedImage(Gif_Image *gfi, int data_interlaced)
{
    size_t sz = (size_t) gfi->width * (size_t) gfi->height;
    uint8_t *data = Gif_NewArray(uint8_t, sz ? sz : 1);
    return Gif_SetUncompressedImage(gfi, data, Gif_Free, data_interlaced);
}

void
Gif_InitCompressInfo(Gif_CompressInfo *gcinfo)
{
    gcinfo->flags = 0;
    gcinfo->loss = 0;
}


#ifdef GIF_DEBUGGING
void
Gif_Debug(char *x, ...)
{
    va_list val;
    va_start(val, x);
    vfprintf(stderr, x, val);
    va_end(val);
}
#endif

void Gif_Free(void* p) {
    free(p);
}

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
