/* I cut out useless pieces of this X.h file and
 * kept what was useful for generating the
 * opengl perl module
 */

/*
 *	$XConsortium: X.h,v 1.69 94/04/17 20:10:48 dpw Exp $
 */

/* Definitions for the X window system likely to be used by applications */

/***********************************************************

Copyright (c) 1987  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.


Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Digital not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
#define X_PROTOCOL	11		/* current protocol version */
#define X_PROTOCOL_REVISION 0		/* current minor version */

/* Resources */


typedef unsigned long XID;
typedef unsigned long Mask;
typedef unsigned long Atom;
typedef unsigned long VisualID;
typedef unsigned long Time;

typedef XID Window;
typedef XID Drawable;
typedef XID Font;
typedef XID Pixmap;
typedef XID Cursor;
typedef XID Colormap;
typedef XID GContext;
typedef XID KeySym;

typedef unsigned char KeyCode;

/*****************************************************************
 * RESERVED RESOURCE AND CONSTANT DEFINITIONS
 *****************************************************************/

   /*** deleted***/


/*****************************************************************
 * EVENT DEFINITIONS
 *****************************************************************/

/* Input Event Masks. Used as event-mask window attribute and as arguments
   to Grab requests.  Not to be confused with event names.  */

/* I removed the "L"'s from the folloing 26 lines to make it parseable */
#define NoEventMask			0
#define KeyPressMask			(1<<0)
#define KeyReleaseMask			(1<<1)
#define ButtonPressMask			(1<<2)
#define ButtonReleaseMask		(1<<3)
#define EnterWindowMask			(1<<4)
#define LeaveWindowMask			(1<<5)
#define PointerMotionMask		(1<<6)
#define PointerMotionHintMask		(1<<7)
#define Button1MotionMask		(1<<8)
#define Button2MotionMask		(1<<9)
#define Button3MotionMask		(1<<10)
#define Button4MotionMask		(1<<11)
#define Button5MotionMask		(1<<12)
#define ButtonMotionMask		(1<<13)
#define KeymapStateMask			(1<<14)
#define ExposureMask			(1<<15)
#define VisibilityChangeMask		(1<<16)
#define StructureNotifyMask		(1<<17)
#define ResizeRedirectMask		(1<<18)
#define SubstructureNotifyMask		(1<<19)
#define SubstructureRedirectMask	(1<<20)
#define FocusChangeMask			(1<<21)
#define PropertyChangeMask		(1<<22)
#define ColormapChangeMask		(1<<23)
#define OwnerGrabButtonMask		(1<<24)

/* Event names.  Used in "type" field in XEvent structures.  Not to be
confused with event masks above.  They start from 2 because 0 and 1
are reserved in the protocol for errors and replies. */

#define KeyPress		2
#define KeyRelease		3
#define ButtonPress		4
#define ButtonRelease		5
#define MotionNotify		6
#define EnterNotify		7
#define LeaveNotify		8
#define FocusIn			9
#define FocusOut		10
#define KeymapNotify		11
#define Expose			12
#define GraphicsExpose		13
#define NoExpose		14
#define VisibilityNotify	15
#define CreateNotify		16
#define DestroyNotify		17
#define UnmapNotify		18
#define MapNotify		19
#define MapRequest		20
#define ReparentNotify		21
#define ConfigureNotify		22
#define ConfigureRequest	23
#define GravityNotify		24
#define ResizeRequest		25
#define CirculateNotify		26
#define CirculateRequest	27
#define PropertyNotify		28
#define SelectionClear		29
#define SelectionRequest	30
#define SelectionNotify		31
#define ColormapNotify		32
#define ClientMessage		33
#define MappingNotify		34
#define LASTEvent		35	/* must be bigger than any event # */


/* Key masks. Used as modifiers to GrabButton and GrabKey, results of QueryPointer,
   state in various key-, mouse-, and button-related events. */

#define ShiftMask		(1<<0)
#define LockMask		(1<<1)
#define ControlMask		(1<<2)
#define Mod1Mask		(1<<3)
#define Mod2Mask		(1<<4)
#define Mod3Mask		(1<<5)
#define Mod4Mask		(1<<6)
#define Mod5Mask		(1<<7)

/* modifier names.  Used to build a SetModifierMapping request or
   to read a GetModifierMapping request.  These correspond to the
   masks defined above. */
#define ShiftMapIndex		0
#define LockMapIndex		1
#define ControlMapIndex		2
#define Mod1MapIndex		3
#define Mod2MapIndex		4
#define Mod3MapIndex		5
#define Mod4MapIndex		6
#define Mod5MapIndex		7


/* button masks.  Used in same manner as Key masks above. Not to be confused
   with button names below. */

#define Button1Mask		(1<<8)
#define Button2Mask		(1<<9)
#define Button3Mask		(1<<10)
#define Button4Mask		(1<<11)
#define Button5Mask		(1<<12)

#define AnyModifier		(1<<15)  /* used in GrabButton, GrabKey */


/* button names. Used as arguments to GrabButton and as detail in ButtonPress
   and ButtonRelease events.  Not to be confused with button masks above.
   Note that 0 is already defined above as "AnyButton".  */

#define Button1			1
#define Button2			2
#define Button3			3
#define Button4			4
#define Button5			5

/* Notify modes */

#define NotifyNormal		0
#define NotifyGrab		1
#define NotifyUngrab		2
#define NotifyWhileGrabbed	3

#define NotifyHint		1	/* for MotionNotify events */

/* Notify detail */

#define NotifyAncestor		0
#define NotifyVirtual		1
#define NotifyInferior		2
#define NotifyNonlinear		3
#define NotifyNonlinearVirtual	4
#define NotifyPointer		5
#define NotifyPointerRoot	6
#define NotifyDetailNone	7

/* Visibility notify */

#define VisibilityUnobscured		0
#define VisibilityPartiallyObscured	1
#define VisibilityFullyObscured		2

/* Circulation request */

#define PlaceOnTop		0
#define PlaceOnBottom		1

/* protocol families */

#define FamilyInternet		0
#define FamilyDECnet		1
#define FamilyChaos		2

/* Property notification */

#define PropertyNewValue	0
#define PropertyDelete		1

/* Color Map notification */

#define ColormapUninstalled	0
#define ColormapInstalled	1

/* GrabPointer, GrabButton, GrabKeyboard, GrabKey Modes */

#define GrabModeSync		0
#define GrabModeAsync		1

/* GrabPointer, GrabKeyboard reply status */

#define GrabSuccess		0
#define AlreadyGrabbed		1
#define GrabInvalidTime		2
#define GrabNotViewable		3
#define GrabFrozen		4

/* AllowEvents modes */

#define AsyncPointer		0
#define SyncPointer		1
#define ReplayPointer		2
#define AsyncKeyboard		3
#define SyncKeyboard		4
#define ReplayKeyboard		5
#define AsyncBoth		6
#define SyncBoth		7

/* Used in SetInputFocus, GetInputFocus */
/*** deleted***/

/*****************************************************************
 * ERROR CODES
 *****************************************************************/
 /*** deleted***/


/*****************************************************************
 * WINDOW DEFINITIONS
*****************************************************************/
 /*** deleted***/

/*****************************************************************
 * GRAPHICS DEFINITIONS
 *****************************************************************/

   /*** deleted***/


/*****************************************************************
 * FONTS
 *****************************************************************/
  /*** deleted***/

/*****************************************************************
 *  IMAGING
 *****************************************************************/
  /*** deleted***/

/*****************************************************************
 *  COLOR MAP STUFF
 *****************************************************************/
  /*** deleted***/

/*****************************************************************
 * CURSOR STUFF
 *****************************************************************/
  /*** deleted***/

/*****************************************************************
 * KEYBOARD/POINTER STUFF
 *****************************************************************/
 /*** deleted***/

/*****************************************************************
 * SCREEN SAVER STUFF
 *****************************************************************/
 /*** deleted***/

/*****************************************************************
 * HOSTS AND CONNECTIONS
 *****************************************************************/
 /*** deleted***/


/* Display classes  used in opening the connection
 * Note that the statically allocated ones are even numbered and the
 * dynamically changeable ones are odd numbered */

#define StaticGray		0
#define GrayScale		1
#define StaticColor		2
#define PseudoColor		3
#define TrueColor		4
#define DirectColor		5


/* Byte order  used in imageByteOrder and bitmapBitOrder */
 /*** deleted***/

