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
bool Gif_InitStream(Gif_Stream *gfs, const char *lmark)
{
	if (!gfs)
		return false;
	gfs->images = NULL;
	gfs->nimages = gfs->imgscap = 0;
	gfs->global = NULL;
	gfs->background = 256;
	gfs->screen_width = gfs->screen_height = 0;
	gfs->loopcount = -1;
	gfs->end_comment = NULL;
	gfs->end_extension_list = NULL;
	gfs->has_local_colors = false;
	gfs->errors.lvl = GE_Warning;
	gfs->errors.num = 0;
	gfs->errors.msg = lmark;
	gfs->user_flags = gfs->refcount = 0;
	gfs->handler = NULL;
	return true;
}

bool Gif_CopyStream(Gif_Stream *dest, const Gif_Stream *src, char no_copy_flags)
{
	if (!src || !Gif_InitStream(dest, NULL))
		return false;

	dest->background    = src->background;
	dest->screen_width  = src->screen_width;
	dest->screen_height = src->screen_height;
	dest->loopcount     = src->loopcount;
	dest->errors.num    = src->errors.num;
	dest->user_flags    = src->user_flags;
	dest->refcount      = src->refcount;
	dest->errors.msg    = Gif_CopyString(src->errors.msg);

	if (!(no_copy_flags & NO_COPY_GIF_COLORMAP) && src->global) {
		dest->global = Gif_New(Gif_Colormap);
		if (!Gif_CopyColormap(dest->global, src->global))
			return false;
	}
	if (!(no_copy_flags & NO_COPY_GIF_IMAGES) && src->nimages > 0) {
		/* there is no reason for copy imgscap */
		if (!(dest->images = Gif_NewArray(Gif_Image *, dest->imgscap = src->nimages)))
			return false;
		for (int i = 0; i < src->nimages; i++) {
			Gif_Image *gim = Gif_New(Gif_Image);
			if (!Gif_CopyImage(gim, src->images[i], no_copy_flags))
				return false;
			dest->images[i] = gim;
		}
		dest->has_local_colors = src->has_local_colors;
		dest->nimages = src->nimages;
	}
	if (!(no_copy_flags & NO_COPY_GIF_EXTENSIONS) && src->end_extension_list) {
		Gif_Extension *gfex = Gif_New(Gif_Extension);
		if (Gif_CopyExtension(gfex, src->end_extension_list, no_copy_flags))
			Gif_AddStreamExtension(dest, gfex);
	}
	if (!(no_copy_flags & NO_COPY_GIF_COMMENTS) && src->end_comment) {
		dest->end_comment = Gif_New(Gif_Comment);
		if (!Gif_CopyComment(dest->end_comment, src->end_comment))
			return false;
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
	gfi->identifier = NULL;
	gfi->comment = NULL;
	gfi->extension_list = NULL;
	gfi->compressed_len = 0;
	gfi->compressed_errors = 0;
	gfi->compressed = NULL;
	gfi->user_flags = gfi->refcount = 0;
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
		dest->comment = Gif_New(Gif_Comment);
		if (!Gif_CopyComment(dest->comment, src->comment))
			return false;
	}
	if (!(no_copy_flags & NO_COPY_GIF_EXTENSIONS) && src->extension_list) {
		Gif_Extension *gfex = Gif_New(Gif_Extension);
		if (Gif_CopyExtension(gfex, src->extension_list, no_copy_flags))
			Gif_AddImageExtension(dest, gfex);
	}
	if (!(no_copy_flags & NO_COPY_GIF_COLORMAP) && src->local) {
		dest->local = Gif_New(Gif_Colormap);
		if (!Gif_CopyColormap(dest->local, src->local))
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
		memcpy(dest->compressed, src->compressed, src->compressed_len);
		dest->compressed_len = src->compressed_len;
		dest->compressed_errors = src->compressed_errors;
	}
	return true;
}


/*
  Colormap constructor functions & methods
*/
bool Gif_InitColormap(Gif_Colormap *gfcm, const int ncols, int capacity)
{
	if (ncols < 0 || capacity <= 0 || !gfcm)
		return false;
	if (ncols > capacity)
		capacity    = ncols;
	gfcm->ncol      = ncols;
	gfcm->capacity  = capacity;
	gfcm->refcount  = gfcm->user_flags = 0;
	if (!(gfcm->col = Gif_NewArray(Gif_Color, capacity)))
		return false;
	return true;
}

bool Gif_CopyColormap(Gif_Colormap *dest, const Gif_Colormap *src)
{
	if (!src || !Gif_InitColormap(dest, src->ncol, src->capacity))
		return false;
	memcpy(dest->col, src->col, sizeof(Gif_Color) * src->ncol);
	return true;
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
bool Gif_InitComment(Gif_Comment *gcom, const int icount)
{
	if (!gcom)
		return false;
	gcom->str = NULL;
	gcom->len = NULL;
	gcom->indents = gcom->cap = 0;
	return true;
}

bool Gif_CopyComment(Gif_Comment *dest, const Gif_Comment *src)
{
	if (!src || !Gif_InitComment(dest, src->indents))
		return false;
	for (int i = 0; i < src->indents; i++)
		if (-1 == Gif_CpyIndent(dest, src->str[i], src->len[i]))
			return false;
	return true;
}

int Gif_CatIndent(Gif_Comment *gcom, const char *str, unsigned len)
{
	const int last_ind = gcom->indents;

	if (last_ind >= gcom->cap) {
		gcom->cap = (gcom->cap ?: 1) * 2;
		Gif_ReArray(gcom->str, const char *, gcom->cap);
		Gif_ReArray(gcom->len, unsigned int, gcom->cap);
		if (!gcom->str || !gcom->len)
			return -1;
	}
	gcom->str[ last_ind ] = str;
	gcom->len[ last_ind ] = len;
	gcom->indents++;
	return last_ind;
}

int Gif_CpyIndent(Gif_Comment *gcom, const char *str, unsigned len)
{
	char *comm = Gif_NewArray(char, len);
	if ( !comm )
		return -1;

	memcpy(comm, str, len);

	int new_ind = Gif_CatIndent(gcom, comm, len);
	if (new_ind == -1)
		Gif_DeleteArray(comm);

	return new_ind;
}


/*
  Extension constructor functions
*/
bool Gif_InitExtension(Gif_Extension *gfex, short kind, const char *name, unsigned len)
{
	if (!gfex)
		return false;

	gfex->kind = kind;
	gfex->appname = gfex->data = NULL;
	gfex->applength = gfex->length = 0;
	gfex->packetized = false;

	char *appname = name ? Gif_NewArray(char, (len ?: (len = strlen(name))) + 1) : NULL;
	if (  appname  ) {
		strncpy(appname, name, len);
		appname[len] = '\0';
		gfex->appname = appname;
		gfex->applength = len;
	}
	gfex->prev = gfex->next = NULL;
	return true;
}

bool Gif_CopyExtension(Gif_Extension *dest, const Gif_Extension *src, char no_copy_flags)
{
	if (!src || !dest)
		return false;

	bool done_ok = true;
	bool do_copy = !(no_copy_flags & NO_COPY_EXTENSION_NEXT);
	short s_kind = !(no_copy_flags & NO_COPY_EXTENSION_APP ) ? -1 : 255;

	Gif_Extension *prev = NULL, *curx = dest, *list = (Gif_Extension *)src;
	do {
		if (list->kind != s_kind) {
			if(!curx) // if curx is null, make new obj
				curx = Gif_New(Gif_Extension);
			if (Gif_InitExtension(curx, list->kind, list->appname, list->applength)) {
				curx->packetized = list->packetized;
				if (curx->data) {
					if (!(curx->data = Gif_NewArray(unsigned char, list->length))) {
						done_ok = false;
						goto done;
					}
					memcpy(curx->data, list->data, (curx->length = list->length));
				}
				if((curx->prev = prev)) // 1. set the previous link of current el
					prev->next = curx;  // 2. set the next link of previous el
				prev = curx; // 3. swap previous to current links
				curx = NULL; // 4. clear current link
			}
		}
	} while (do_copy && (list = list->next));

 done:
	if (curx)
		Gif_Delete(curx);
	return done_ok;
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

int Gif_PutExtension(Gif_Extension *dst_list, Gif_Extension *gfex)
{
	Gif_Extension *prev = dst_list;
	int cnt = 0;

	do { cnt++;
		if (gfex == prev)
			return -1;
	} while ((prev = prev->prev));

	for (prev = dst_list; prev->next; cnt++) {
		if (gfex == (prev = prev->next))
			return -1;
	}
	prev->next = gfex;
	gfex->prev = prev;
	gfex->next = NULL;
	return cnt;
}

int Gif_PutImage(Gif_Stream *gfs, Gif_Image *gfi)
{
	const int max_idx = gfs->nimages;
	if (gfs->imgscap <= max_idx) {
		gfs->imgscap += 10;
		if (!Gif_ReArray(gfs->images, Gif_Image *, gfs->imgscap))
			return -1;
	}
	gfs->images[max_idx] = gfi;
	gfs->nimages++, gfi->refcount++;
	return max_idx;
}

void Gif_RemoveImage(Gif_Stream *gfs, int idx)
{
	if (idx < 0)
		idx += gfs->nimages;
	int max_idx = --gfs->nimages;
	Gif_FreeImage(gfs->images[idx]);
	for (; idx < max_idx; idx++)
		gfs->images[idx] = gfs->images[idx + 1];
	gfs->images[max_idx] = NULL;
}

void Gif_CalcScreenSize(Gif_Stream *gfs, bool force)
{
	unsigned short screen_width = 0, screen_height = 0;

	for (int i = 0; i < gfs->nimages; i++) {
		unsigned width = gfs->images[i]->left + gfs->images[i]->width,
		        height = gfs->images[i]->top  + gfs->images[i]->height;

		if (screen_width < width)
			screen_width = width;
		if (screen_height < height)
			screen_height = height;
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

void Gif_SetUncompressedImage(Gif_Image *gfi, bool is_interlaced, unsigned char *data)
{
	/* NB does not affect compressed image (and must not) */
	unsigned int   k;
	unsigned short y, iH = gfi->height,
	                  iW = gfi->width;
	unsigned char  **img = Gif_NewArray(unsigned char *, iH + 1);

	Gif_ReleaseUncompressedImage(gfi);

	if (img) {
		for (y = k = 0; y < iH; k += iW, y++)
			img[(is_interlaced ? Gif_InterlaceLine(y, iH) : y)] = &data[k];
		img[iH]  = 0;
		gfi->img = img;
	}
	gfi->image_data = data;
}

bool Gif_CreateUncompressedImage(Gif_Image *gfi, bool is_interlaced)
{
	unsigned len = 1;
	unsigned char *new_data;

	if (gfi->width && gfi->height)
		len = (unsigned)gfi->width * (unsigned)gfi->height;
	if (!(new_data = Gif_NewArray(unsigned char, len)))
		return false;
	Gif_SetUncompressedImage(gfi, is_interlaced, new_data);
	return true;
}

void Gif_ReleaseUncompressedImage(Gif_Image *gfi) {
	Gif_DeleteArray(gfi->img);
	if (gfi->image_data)
		Gif_Delete(gfi->image_data);
}

void Gif_ReleaseCompressedImage(Gif_Image *gfi) {
	if (gfi->compressed)
		Gif_Delete(gfi->compressed);
	gfi->compressed_len    = 0;
	gfi->compressed_errors = 0;
}

void Gif_MakeImageEmpty(Gif_Image* gfi) {
	Gif_ReleaseUncompressedImage(gfi);
	Gif_ReleaseCompressedImage(gfi);
	gfi->width = gfi->height = 1;
	gfi->transparent = 0;
	Gif_CreateUncompressedImage(gfi, 0);
	gfi->img[0][0] = 0;
}


/* 
  Class Deconstructors
*/
#define CHECK_REF(obj) \
	if (!obj || --obj->refcount > 0) return

void Gif_FreeStream(Gif_Stream *gfs)
{
	CHECK_REF(gfs);

	for (int i = 0; i < gfs->nimages; i++)
		Gif_DeleteImage(gfs->images[i]);

	Gif_DeleteArray   (gfs->images);
	Gif_DeleteColormap(gfs->global);
	Gif_DeleteComment (gfs->end_comment);

	if (gfs->end_extension_list) {
		while (gfs->end_extension_list->next)
			Gif_DeleteExtension(gfs->end_extension_list->next);
		Gif_DeleteExtension(gfs->end_extension_list);
	}
	Gif_Delete(gfs);
}

void Gif_FreeImage(Gif_Image *gfi)
{
	CHECK_REF(gfi);

	Gif_DeleteArray(gfi->identifier);
	Gif_DeleteComment(gfi->comment);

	if (gfi->extension_list) {
		while (gfi->extension_list->next)
			Gif_DeleteExtension(gfi->extension_list->next);
		Gif_DeleteExtension(gfi->extension_list);
	}
	Gif_DeleteColormap(gfi->local);

	if (gfi->image_data)
		Gif_Delete(gfi->image_data);

	Gif_DeleteArray(gfi->img);

	if (gfi->compressed)
		Gif_Delete(gfi->compressed);

	Gif_Delete(gfi);
}

void Gif_FreeColormap(Gif_Colormap *gfcm)
{
	CHECK_REF(gfcm);
	Gif_DeleteArray(gfcm->col);
	Gif_Delete(gfcm);
}

void Gif_FreeComment(Gif_Comment *gfcom)
{
	if (!gfcom)
		return;
	for (int i = 0; i < gfcom->indents; i++)
		Gif_DeleteArray(gfcom->str[i]);
	Gif_DeleteArray(gfcom->str);
	Gif_DeleteArray(gfcom->len);
	Gif_Delete(gfcom);
}

void Gif_FreeExtension(Gif_Extension *gfex)
{
	if (!gfex)
		return;
	if (gfex->data)
		Gif_DeleteArray(gfex->data);
	if (gfex->appname)
		Gif_DeleteArray(gfex->appname);
	if (gfex->prev)
		gfex->prev->next = gfex->next;
	if (gfex->next)
		gfex->next->prev = gfex->prev;
	gfex->next = gfex->prev = NULL;
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
