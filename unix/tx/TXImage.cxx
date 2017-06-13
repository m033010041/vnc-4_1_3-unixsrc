/* Copyright (C) 2002-2005 RealVNC Ltd.  All Rights Reserved.
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
 * USA.
 */
//
// TXImage.cxx
//


#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <list>
#include <rfb/TransImageGetter.h>
#include <rfb/Exception.h>
#include <rfb/LogWriter.h>
#include <stdio.h>
#include <stdlib.h>

#include "TXWindow.h"
#include "TXImage.h"

using namespace rfb;

static rfb::LogWriter vlog("TXImage");
static XImage* xim_tmp;
static XShmSegmentInfo* shminfo_tmp;

static void superscale(XImage * ximg, int width, int height, unsigned int bytesperline, char* __restrict__ newBuf);
static void reduce_pixmap50(XImage* oldimg, XImage* newimg);

TXImage::TXImage(Display* d, int width, int height, Visual* vis_, int depth_)
  : xim(0), dpy(d), vis(vis_), depth(depth_), shminfo(0), tig(0), cube(0)
{
  width_ = width;
  height_ = height;
  for (int i = 0; i < 256; i++)
    colourMap[i].r = colourMap[i].g = colourMap[i].b = 0;

  if (!vis)
    vis = DefaultVisual(dpy,DefaultScreen(dpy));
  if (!depth)
    depth = DefaultDepth(dpy,DefaultScreen(dpy));

  createXImage();
  getNativePixelFormat(vis, depth);
  colourmap = this;
  format.bpp = 0;  // just make it different to any valid format, so that...
  setPF(nativePF); // ...setPF() always works
}

TXImage::~TXImage()
{
  if (data != (rdr::U8*)xim->data) delete [] data;
  destroyXImage();
  delete tig;
  delete cube;
}

void TXImage::resize(int w, int h)
{
  if (w == width() && h == height()) return;

  int oldStrideBytes = getStride() * (format.bpp/8);
  int rowsToCopy = __rfbmin(h, height());
  int bytesPerRow = __rfbmin(w, width()) * (format.bpp/8);
  rdr::U8* oldData = 0;
  bool allocData = false;

  if (data != (rdr::U8*)xim->data) {
    oldData = (rdr::U8*)data;
    allocData = true;
  } else {
    oldData = new rdr::U8[xim->bytes_per_line * height()];
    memcpy(oldData, xim->data, xim->bytes_per_line * height());
  }

  destroyXImage();
  width_ = w;
  height_ = h;
  createXImage();

  if (allocData)
    data = new rdr::U8[width() * height() * (format.bpp/8)];
  else
    data = (rdr::U8*)xim->data;

  int newStrideBytes = getStride() * (format.bpp/8);
  for (int i = 0; i < rowsToCopy; i++)
    memcpy((rdr::U8*)data + newStrideBytes * i, oldData + oldStrideBytes * i,
           bytesPerRow);
  delete [] oldData;
}

void TXImage::setPF(const PixelFormat& newPF)
{
  if (newPF.equal(format)) return;
  format = newPF;

  if (data != (rdr::U8*)xim->data) delete [] data;
  delete tig;
  tig = 0;

  if (format.equal(nativePF) && format.trueColour) {
    data = (rdr::U8*)xim->data;
  } else {
    data = new rdr::U8[width() * height() * (format.bpp/8)];
    tig = new TransImageGetter();
    tig->init(this, nativePF, 0, cube);
  }
}

int TXImage::getStride() const
{
  if (data == (rdr::U8*)xim->data)
    return xim->bytes_per_line / (xim->bits_per_pixel / 8);
  else
    return width();
}

void TXImage::put(Window win, GC gc, const rfb::Rect& r)
{
  if (r.is_empty()) return;
  int x = r.tl.x;
  int y = r.tl.y;
  int w = r.width();
  int h = r.height();
  static int count=0;

  if (data != (rdr::U8*)xim->data) {
    rdr::U8* ximDataStart = ((rdr::U8*)xim->data + y * xim->bytes_per_line
                             + x * (xim->bits_per_pixel / 8));
    tig->getImage(ximDataStart, r,
                  xim->bytes_per_line / (xim->bits_per_pixel / 8));
  }

  //superscale(xim_tmp, 400, 300, xim->bits_per_pixel, xim->data);
  reduce_pixmap50(xim, xim_tmp);


  if (usingShm()) {
    XShmPutImage(dpy, win, gc, xim_tmp, x/2, y/2, x/2, y/2, w/2, h/2, False);
    //XShmPutImage(dpy, win, gc, xim, x, y, x, y, w, h, False);
  } else {
    XPutImage(dpy, win, gc, xim, x, y, x, y, w, h);
  }
}

void TXImage::setColourMapEntries(int firstColour, int nColours, rdr::U16* rgbs)
{
  for (int i = 0; i < nColours; i++) {
    colourMap[firstColour+i].r = rgbs[i*3];
    colourMap[firstColour+i].g = rgbs[i*3+1];
    colourMap[firstColour+i].b = rgbs[i*3+2];
  }
}

void TXImage::updateColourMap()
{
  tig->setColourMapEntries(0, 0, 0);
}

void TXImage::lookup(int index, int* r, int* g, int* b)
{
  *r = colourMap[index].r;
  *g = colourMap[index].g;
  *b = colourMap[index].b;
}


static bool caughtError = false;

static int XShmAttachErrorHandler(Display *dpy, XErrorEvent *error)
{
  caughtError = true;
  return 0;
}

class TXImageCleanup {
public:
  std::list<TXImage*> images;
  ~TXImageCleanup() {
    while (!images.empty())
      delete images.front();
  }
};

static TXImageCleanup imageCleanup;

void TXImage::createXImage()
{
  if (XShmQueryExtension(dpy)) {
    shminfo = new XShmSegmentInfo;
    shminfo_tmp = new XShmSegmentInfo;

    xim = XShmCreateImage(dpy, vis, depth, ZPixmap,
                          0, shminfo, width(), height());
    xim_tmp = XShmCreateImage(dpy, vis, depth, ZPixmap,
                          0, shminfo_tmp, width()/2, height()/2);
    printf("%d, %d\n",width(), height());

    printf("xim->bytes_per_line=%d\n", xim->bytes_per_line);
    if (xim && xim_tmp) {
      shminfo->shmid = shmget(IPC_PRIVATE,
                              xim->bytes_per_line * xim->height,
                              IPC_CREAT|0777);
      shminfo_tmp->shmid = shmget(IPC_PRIVATE,
                              xim_tmp->bytes_per_line * xim_tmp->height,
                              IPC_CREAT|0777);


      if ((shminfo->shmid != -1)&&(shminfo_tmp->shmid != -1)) {
        shminfo->shmaddr = xim->data = (char*)shmat(shminfo->shmid, 0, 0);
        shminfo_tmp->shmaddr = xim_tmp->data = (char*)shmat(shminfo_tmp->shmid, 0, 0);

        if ((shminfo->shmaddr != (char *)-1)&&(shminfo_tmp->shmaddr != (char *)-1)) {

          shminfo_tmp->readOnly = shminfo->readOnly = False;

          XErrorHandler oldHdlr = XSetErrorHandler(XShmAttachErrorHandler);
          XShmAttach(dpy, shminfo);
          XShmAttach(dpy, shminfo_tmp);
          XSync(dpy, False);
          XSetErrorHandler(oldHdlr);

          if (!caughtError) {
            vlog.debug("Using shared memory XImage");
            imageCleanup.images.push_back(this);
            return;
          }

          shmdt(shminfo->shmaddr);
          shmdt(shminfo_tmp->shmaddr);
        } else {
          vlog.error("shmat failed");
          perror("shmat");
        }

        shmctl(shminfo->shmid, IPC_RMID, 0);
        shmctl(shminfo_tmp->shmid, IPC_RMID, 0);
      } else {
        vlog.error("shmget failed");
        perror("shmget");
      }

      XDestroyImage(xim);
      XDestroyImage(xim_tmp);
      xim_tmp = xim = 0;
    } else {
      vlog.error("XShmCreateImage failed");
    }

    delete shminfo;
    delete shminfo_tmp;
    shminfo_tmp = shminfo = 0;
  }

  xim = XCreateImage(dpy, vis, depth, ZPixmap,
                     0, 0, width(), height(), BitmapPad(dpy), 0);

  xim->data = (char*)malloc(xim->bytes_per_line * xim->height);
  if (!xim->data) {
    vlog.error("malloc failed");
    exit(1);
  }
}

void TXImage::destroyXImage()
{
  if (shminfo) {
    vlog.debug("Freeing shared memory XImage");
    shmdt(shminfo->shmaddr);
    shmctl(shminfo->shmid, IPC_RMID, 0);
    delete shminfo;
    shminfo = 0;
    imageCleanup.images.remove(this);
  }
  // XDestroyImage() will free(xim->data) if appropriate
  if (xim) XDestroyImage(xim);
  xim = 0;

 if (shminfo_tmp) {
    vlog.debug("Freeing shared memory XImage");
    shmdt(shminfo_tmp->shmaddr);
    shmctl(shminfo_tmp->shmid, IPC_RMID, 0);
    delete shminfo_tmp;
    shminfo_tmp = 0;
  }
  // XDestroyImage() will free(xim->data) if appropriate
  if (xim_tmp) XDestroyImage(xim_tmp);
  xim_tmp = 0;

}


static bool supportedBPP(int bpp) {
  return (bpp == 8 || bpp == 16 || bpp == 32);
}

static int depth2bpp(Display* dpy, int depth)
{
  int nformats;
  XPixmapFormatValues* format = XListPixmapFormats(dpy, &nformats);

  int i;
  for (i = 0; i < nformats; i++)
    if (format[i].depth == depth) break;

  if (i == nformats || !supportedBPP(format[i].bits_per_pixel))
    throw rfb::Exception("Error: couldn't find suitable pixmap format");

  int bpp = format[i].bits_per_pixel;
  XFree(format);
  return bpp;
}

void TXImage::getNativePixelFormat(Visual* vis, int depth)
{
  cube = 0;
  nativePF.depth = depth;
  nativePF.bpp = depth2bpp(dpy, depth);
  nativePF.bigEndian = (ImageByteOrder(dpy) == MSBFirst);
  nativePF.trueColour = (vis->c_class == TrueColor);

  vlog.info("Using default colormap and visual, %sdepth %d.",
            (vis->c_class == TrueColor) ? "TrueColor, " :
            ((vis->c_class == PseudoColor) ? "PseudoColor, " : ""),
            depth);

  if (nativePF.trueColour) {

    nativePF.redShift   = ffs(vis->red_mask)   - 1;
    nativePF.greenShift = ffs(vis->green_mask) - 1;
    nativePF.blueShift  = ffs(vis->blue_mask)  - 1;
    nativePF.redMax   = vis->red_mask   >> nativePF.redShift;
    nativePF.greenMax = vis->green_mask >> nativePF.greenShift;
    nativePF.blueMax  = vis->blue_mask  >> nativePF.blueShift;

  } else {

    XColor xc[256];
    cube = new rfb::ColourCube(6,6,6);
    int r;
    for (r = 0; r < cube->nRed; r++) {
      for (int g = 0; g < cube->nGreen; g++) {
        for (int b = 0; b < cube->nBlue; b++) {
          int i = (r * cube->nGreen + g) * cube->nBlue + b;
          xc[i].red =   r * 65535 / (cube->nRed-1);
          xc[i].green = g * 65535 / (cube->nGreen-1);
          xc[i].blue =  b * 65535 / (cube->nBlue-1);
        }
      }
    }

    TXWindow::getColours(dpy, xc, cube->size());

    for (r = 0; r < cube->nRed; r++) {
      for (int g = 0; g < cube->nGreen; g++) {
        for (int b = 0; b < cube->nBlue; b++) {
          int i = (r * cube->nGreen + g) * cube->nBlue + b;
          cube->set(r, g, b, xc[i].pixel);
        }
      }
    }
  }
}



/*
 * Super sampling scale. Down only.
 */
static void superscale(XImage* ximg, int width, int height, unsigned int bytesperline, char* __restrict__ newBuf)
{
	unsigned int x, y, i;
	char * __restrict__ ibuf;

	if((ximg->width == width) && (ximg->height == height)) return;

	ibuf = ximg->data;


	unsigned int divx[width];
	unsigned int divy[height];
	memset(divx, 0, sizeof divx);
	memset(divy, 0, sizeof divy);
	for(x = 0; x < ximg->width; x++){
		 divx[x * width / ximg->width]++;
	}
	for(y = 0; y < ximg->height; y++){
		 divy[y * height / ximg->height]++;
	}

	unsigned int tmp[width * 4];

	unsigned int *xoff[ximg->width];
	for(x = 0; x < ximg->width; x++){
		xoff[x] = tmp + (x * width / ximg->width) * 3;
	}

	unsigned int count = 0;
	unsigned int y0;
	unsigned int * __restrict__ dest;
	for(y = 0; y < ximg->height;){
		unsigned int ydiv = divy[y * height / ximg->height];
		char * __restrict__ ydest = &newBuf[bytesperline * (y * height / ximg->height)];
		memset(tmp, 0, sizeof tmp);
		ibuf = &ximg->data[y * ximg->width * 3];
		for(y0 = y + ydiv; y < y0; y++){
			for(x = 0; x < ximg->width; x++){
				dest = xoff[x];
				for(i = 0; i < 3; i++){
					*dest++ += *ibuf++;
				}
			}
		}
		unsigned int * __restrict__ src = tmp;
		for(x = 0; x < width; x++){
			ydest[2] = *src++ / ydiv / divx[x];
			ydest[1] = *src++ / ydiv / divx[x];
			ydest[0] = *src++ / ydiv / divx[x];
			ydest += 4;
		}
	}

}


static void reduce_pixmap50(XImage* oldimg, XImage* newimg)
{
	int i, j;
	unsigned long p1base, p2base, p3base, p4base, pnew;
	unsigned char p1R, p1G, p1B;
	unsigned char p2R, p2G, p2B;
	unsigned char p3R, p3G, p3B;
	unsigned char p4R, p4G, p4B;

	/* I think raw data format is B-G-R-A */

	//printf("oldimg->height=%d, oldimg->width=%d\n", oldimg->height, oldimg->width);
	//printf("newimg->height=%d, newimg->width=%d\n", newimg->height, newimg->width);

	for(i=0; i<oldimg->height; i=i+2) {
		for(j=0; j<oldimg->width; j=j+2) {

			p1base =  (j<<2)    +  i   *(oldimg->bytes_per_line);
			p2base = ((j+1)<<2) +  i   *(oldimg->bytes_per_line);
			p3base =  (j<<2)    + (i+1)*(oldimg->bytes_per_line);
			p4base = ((j+1)<<2)  + (i+1)*(oldimg->bytes_per_line);
			pnew   = ((j/2)<<2) + (i/2)*(newimg->bytes_per_line);


			p1B = oldimg->data[p1base];
			p1G = oldimg->data[p1base+1];
			p1R = oldimg->data[p1base+2];
			//p1A = oldimg->data[p1base+3] & 0x00;

			p2B = oldimg->data[p2base];
			p2G = oldimg->data[p2base+1];
			p2R = oldimg->data[p2base+2];
			//p2A = oldimg->data[p2base+3] & 0x00;

			p3B = oldimg->data[p3base];
			p3G = oldimg->data[p3base+1];
			p3R = oldimg->data[p3base+2];
			//p3A = oldimg->data[p3base+3] & 0x00;

			p4B = oldimg->data[p4base];
			p4G = oldimg->data[p4base+1];
			p4R = oldimg->data[p4base+2];
			//p4A = oldimg->data[p4base+3] & 0x00;

			unsigned char avgR = (p1R+p2R+p3R+p4R)>>2;
			unsigned char avgG = (p1G+p2G+p3G+p4G)>>2;
			unsigned char avgB = (p1B+p2B+p3B+p4B)>>2;

			newimg->data[pnew]   = avgB;
			newimg->data[pnew+1] = avgG;
			newimg->data[pnew+2] = avgR;
			newimg->data[pnew+3] = 0x00;

/* This method is not effiency but it works */
/*
			unsigned long  p1 = XGetPixel(oldimg, j, i);
			unsigned long p1R = p1 & 0x00ff0000; //red_mask
			unsigned long p1G = p1 & 0x0000ff00; //green_mask
			unsigned long p1B = p1 & 0x000000ff; //blue_mask

			unsigned long  p2 = XGetPixel(oldimg, j+1, i);
			unsigned long p2R = p2 & 0x00ff0000; //red_mask
			unsigned long p2G = p2 & 0x0000ff00; //green_mask
			unsigned long p2B = p2 & 0x000000ff; //blue_mask

			unsigned long  p3 = XGetPixel(oldimg, j, i+1);
			unsigned long p3R = p3 & 0x00ff0000; //red_mask
			unsigned long p3G = p3 & 0x0000ff00; //green_mask
			unsigned long p3B = p3 & 0x000000ff; //blue_mask

			unsigned long  p4 = XGetPixel(oldimg, j+1, i+1);
			unsigned long p4R = p4 & 0x00ff0000; //red_mask
			unsigned long p4G = p4 & 0x0000ff00; //green_mask
			unsigned long p4B = p4 & 0x000000ff; //blue_mask

			XPutPixel(newimg, j/2, i/2, avgPixel);
*/
		}
	}
}


/*
static void downscale(XImage* src, XImage* dst, int srcWidth, int srcHeight, int dstWidth, int dstHeight)
{
	float x_ratio = ((float)(srcWidth-1.0)) / dstHeight;
	float y_ratio =
}

*/
