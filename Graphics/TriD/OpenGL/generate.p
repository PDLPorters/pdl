# Regeneration of the PDL::Graphics::OpenGL from the Mesa 2.0 include files.
#
# Based on OpenGL-0.4 but heavily modified by Tjl
#
# Bits based on Tk and ideas in there.
#

sub maybe {map {-e $_ ? $_ : ()} @_}
@types = qw|X.h /usr/include/GL/gl.h /usr/include/GL/glx.h|;

push @types, maybe qw|/usr/include/GL/glxtokens.h|;

@funcs = qw|/usr/include/GL/gl.h /usr/include/GL/glu.h /usr/include/GL/glx.h|;

# These are defined elsewhere or have difficult arg types
@dontfuncs = qw|glGetPointerv glGetPointervEXT gluNurbsCallback
	glXSwapBuffers gluLookAt gluOrtho2D gluPerspective
	gluPickMatrix glPolygonOffsetEXT|;

# IRIX doesn't have these that MESA does. 
# later on, make an ifdeffable solution
push @dontfuncs, qw|glPolygonOffset glPushClientAttrib glPopClientAttrib
	glIndexub glIndexubv glVertexPointer glNormalPointer glColorPointer
	glIndexPointer glTexCoordPointer glEdgeFlagPointer glArrayElement
	glDrawArrays glDrawElements glInterleavedArrays glGenTextures
	glDeleteTextures glBindTexture glPrioritizeTextures glAreTexturesResident
	glIsTexture glTexSubImage1D glTexSubImage2D glCopyTexImage1D
	glCopyTexImage2D glCopyTexSubImage1D glCopyTexSubImage2D
	glWindowPos2iMESA glWindowPos2sMESA glWindowPos2fMESA
	glWindowPos2dMESA glWindowPos2ivMESA glWindowPos2svMESA
	glWindowPos2fvMESA glWindowPos2dvMESA glWindowPos3iMESA
	glWindowPos3sMESA glWindowPos3fMESA glWindowPos3dMESA
	glWindowPos3ivMESA glWindowPos3svMESA glWindowPos3fvMESA
	glWindowPos3dvMESA  glWindowPos4iMESA glWindowPos4sMESA
	glWindowPos4fMESA  glWindowPos4dMESA glWindowPos4ivMESA
	glWindowPos4svMESA glWindowPos4fvMESA glWindowPos4dvMESA
	glResizeBuffersMESA glXCreateGLXPixmapMESA glXReleaseBuffersMESA
	glEnableClientState glDisableClientState
|;

# These have difficult types on IRIX
push @dontfuncs, qw|glXCreateGLXVideoSourceSGIX glXGetContextIDEXT
	glXGetFBConfigAttribSGIX glXCreateGLXPixmapWithConfigSGIX
	glXCreateContextWithConfigSGIX glXGetFBConfigFromVisualSGIX
	glXCreateGLXPbufferSGIX glXGetVideoSyncSGI glXWaitVideoSyncSGI
	glXQueryGLXPbufferSGIX glXSelectEventSGIX glXGetSelectedEventSGIX
	glXDestroyGLXVideoSourceSGIX
	glXAssociateDMPbufferSGIX|;

# And these were generated on the SGI but not usable under MESA for
# some reason or other (?).
push @dontfuncs, qw|gluBeginCurve gluBeginPolygon gluBeginSurface
	gluBeginTrim gluCylinder gluDeleteNurbsRenderer gluDeleteQuadric
	gluDeleteTess gluDisk gluEndCurve gluEndPolygon gluEndSurface
	gluEndTrim gluGetNurbsProperty gluGetTessProperty gluLoadSamplingMatrices
	gluNextContour gluNurbsCurve gluNurbsProperty gluNurbsSurface
	gluPartialDisk  gluPwlCurve gluQuadricDrawStyle gluQuadricNormals
	gluQuadricOrientation gluQuadricTexture 
	gluSphere gluTessBeginContour gluTessBeginPolygon gluTessEndContour
	gluTessEndPolygon gluTessNormal gluTessProperty gluTessVertex
	glXDestroyGLXPbufferSGIX|;

# generating on Alphas dies at these...
push @dontfuncs, qw|glXCreateContext glXCopyContext glBlendEquationEXT
	glBlendColorEXT glXGetCurrentDrawableEXT glXImportContextEXT
	glXFreeContextEXT glXQueryContextInfoEXT|;

# Generating on mesa, used on alphas...
push @dontfuncs, qw|glTexImage3DEXT glTexSubImage3DEXT glCopyTexSubImage3DEXT
	|;

# Generating on irix, used on alphas...
push @dontfuncs, qw|
	glGetListParameterivSGIX glGetMinmaxEXT glGetMinmaxParameterfvEXT
	glGetMinmaxParameterivEXT glGetSeparableFilterEXT 
	glGetSharpenTexFuncSGIS
	glGetTexFilterFuncSGIS glHistogramEXT glInstrumentsBufferSGIX
	glListParameterfSGIX glListParameterfvSGIX glListParameteriSGIX
	glListParameterivSGIX glLoadIdentityDeformationMapSGIX
	glMinmaxEXT glPixelTexGenSGIX glPointParameterfSGIS
	glPointParameterfvSGIS glPollInstrumentsSGIX
	glReadInstrumentsSGIX glReferencePlaneSGIX glResetHistogramEXT
	glResetMinmaxEXT  glSampleMaskSGIS glSamplePatternSGIS
	glSeparableFilter2DEXT glSharpenTexFuncSGIS
	glSpriteParameterfSGIX glSpriteParameterfvSGIX
	glSpriteParameteriSGIX glSpriteParameterivSGIX
	glStartInstrumentsSGIX glStopInstrumentsSGIX
	glTagSampleBufferSGIX glTexFilterFuncSGIS
	glTexImage4DSGIS glTexSubImage1DEXT
 	glTexSubImage2DEXT glTexSubImage4DSGIS
	gluDeleteNurbsTessellatorEXT gluNurbsCallbackDataEXT
	gluTexFilterFuncSGI glXSwapIntervalSGI
	glXMakeCurrentReadSGI glXGetCurrentReadDrawableSGI
	glXBindChannelToWindowSGIX glXQueryChannelDeltasSGIX
	glXChannelRectSGIX glXQueryChannelRectSGIX
	glXChannelRectSyncSGIX glXJoinSwapGroupSGIX
	glXBindSwapBarrierSGIX glXQueryMaxSwapBarriersSGIX
	glColorTableParameterfvSGI glColorTableParameterivSGI
glColorTableSGI glConvolutionFilter1DEXT glConvolutionFilter2DEXT
glConvolutionParameterfEXT glConvolutionParameterfvEXT 
glConvolutionParameteriEXT glConvolutionParameterivEXT
glCopyColorTableSGI glCopyConvolutionFilter1DEXT
glCopyConvolutionFilter2DEXT glCopyTexImage1DEXT
glCopyTexImage2DEXT glCopyTexSubImage1DEXT
glCopyTexSubImage2DEXT glDeformSGIX
glDeformationMap3dSGIX glDeformationMap3fSGIX 
glDetailTexFuncSGIS glFlushRasterSGIX glFogFuncSGIS
glFrameZoomSGIX glGetColorTableParameterfvSG glGetColorTableParameterivSGI
glGetColorTableSGI glGetConvolutionFilterEXT 
glGetConvolutionParameterfvE glGetConvolutionParameterivE
glGetDetailTexFuncSGIS glGetHistogramEXT 
glGetHistogramParameterfvEXT glGetHistogramParameterivEXT
glGetInstrumentsSGIX glGetListParameterfvSGIX
glGetColorTableParameterfvSGI glGetConvolutionParameterfvEXT
glGetConvolutionParameterivEXT
|;

@consts = @types;

for(@dontfuncs) {$dontfuncs{$_} = 1}

# 1. generate typemap

%t=(
        'int'    => 'INT',
        'float'  => 'FLOAT',
        'double' => 'DOUBLE',
        'short'  => 'SHORT',
        'long'   => 'LONG',
        'char'   => 'CHAR',
        'XID'    => 'U_LONG',
);
sub gettypes {
        local($file)=@_;
        open(FILE,$file) || die "cant open $file\n";
        while(<FILE>) {
                if(/typedef/) {
#			print STDERR "$_";
                        foreach $k (keys(%t)) {
                                print "$1\t\tT_$t{$k}\n" if(/typedef\s+$k\s+(\w+)\s*\;/);
                                print "$1\t\tT_$t{$k}\n" if(/typedef\s+signed\s+$k\s+(\w+)\s*\;/);
                                print "$1\t\tT_U_$t{$k}\n" if(/typedef\s+unsigned\s+$k\s+(\w+)\s*\;/);
                        }
                }
        }
        close(FILE);
}

open(TYPEMAP,">typemap") or die "Can't write typemap\n";;
select TYPEMAP;
for(@types) {gettypes($_)}
print "GLXContext	T_PTR\n";
print "Bool	T_INT\n";
close TYPEMAP;

sub getfuncs {
        local($file)=@_;
        open(FILE,$file) || die "cant open $file\n";
	my $str = join '',<FILE>;
        while($str =~ /extern\s+(\w+)\s+(\w+)\s*\(([^\(\)]*)\)\s*\;/gs){
                        #print "$1 $2($3)\n"
                        $rt=$1;$name=$2;$args=$3;
			if($dontfuncs{$name}) { next }
			push @subnames,$name;
#			print "Gen: $rt $name $args\n";
			push @vfuncs,"$1,$2,V_$2,($3)";
                        $exists_pointer = ($args =~ /\*/);
                        @args = split(/\,/,$args);
                        @args=() if (($args =~ /^\s*void\s*$/) || ($args =~ /^\s*$/));
                        print "$rt\n";
                        print "$name(";
                        $i=0;
                        foreach $a (@args) {
                                print "," if($i);
				$a =~ s/const\s+(\w+)\s+(\w+)\s*\[[^]]*\]/$1_star $2/;
				$a =~ s/(\w+)\s+(\w+)\s*\[[^]]*\]/$1_star $2/;
                                $a =~ s/const\s+(.*\S)\s*\*\s*(.*)/$1_star $2/;
                                $a =~ s/(.*\S)\s*\*\s*(.*)/$1_star $2/;
                                $a =~ /(\w+)\s+(\w+)/;
                                print($2);
                                $i++;
                        }
                        print ")\n";
                        foreach $a (@args) {
                                $a =~ /(\w+)\s+(\w+)/;
                                $t=$1;$n=$2;
                                $t =~ s/\w+_star/char \*/;
                                print "\t$t\t$n\n";
                        }
                        if($exists_pointer) {
                                print "\tCODE:\n";
                                print "\t{\n";
                                print "\t   $name(";
                                $i=0;
                                foreach $a (@args) {
                                        print "," if($i);
                                        $a =~ /(\w+)\s+(\w+)/;
                                        $t=$1;$n=$2;
                                        if($t =~ /(\w+)_star/) {
                                                print "($1 *)";
                                        }
                                        print $n;
                                        $i++;
                                }
                                print ");\n";
                                print "\t}\n";
                        }
                        print "\n";
        }
}

open(XS,">OpenGL.xs") or die "Can't write OpenGL.xs";
select XS;
my $start = join '',<DATA>;
print $start;
for(@funcs) {getfuncs($_)}
print <<'END';
BOOT:
 {
	OpenGLVPtr = &vtab;
#define VFUNC(type,name,mem,args) (vtab.mem = name);
#include "OpenGL.vf"
#undef VFUNC
	sv_setiv(perl_get_sv("PDL::Graphics::OpenGLVPtr",1),(IV)OpenGLVPtr);
 }
END

close XS;



open(VTAB,">OpenGL.vf") or die "Can't write OpenGL.vf";
select VTAB;
for(@vfuncs) {
	print "VFUNC($_)\n";
}
close VTAB;


open(MTAB,">OpenGL.m") or die "Can't write OpenGL.m";
select MTAB;

print <<'END';
#ifndef OPENGL_VIRT
#define OPENGL_VIRT
typedef struct OpenGLVTab
	{
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "OpenGL.vf"
#undef VFUNC
#undef VVAR
} OpenGLVTab;
extern OpenGLVTab *OpenGLVPtr;
#define D_OPENGL OpenGLVTab *OpenGLVPtr
#define I_OPENGL OpenGLVPtr = (OpenGLVTab *) SvIV(perl_get_sv("PDL::Graphics::OpenGLVPtr",5));   
#ifndef OPENGL_NOVIRT
END

for(@subnames) {
	print "#define $_ (*OpenGLVPtr->V_$_)\n";
}
print "#endif\n#endif\n";

close MTAB;






sub getconsts {
	local ($file) = @_;
        open(FILE,$file) || die "cant open $file\n";
        while(<FILE>) {
		if(/^\#define\s+(\w+)\s+(\S+)\s*/) {
			$consts{$1} = $2;
		} elsif(/^\s*(\w+)\s*=\s*(\S+)\s*,\s*/) {
			# Assume it's inside an enum.
			$consts{$1} = $2;
		}
	}
}

open PM,">OpenGL.pm" or die "Can't write OpenGL.pm";
select PM;

print <<'END';
	package PDL::Graphics::OpenGL;
	$Version = 0.5;

	require Exporter;
	require DynaLoader;
	@ISA = qw(Exporter DynaLoader);
	@EXPORT = qw(
		glpOpenWindow
		glXSwapBuffers
		XPending
		glpXNextEvent
		glpXQueryPointer
		glpReadTex
		glpClipPlane
		glpGetClipPlane
		glpLoadMatrixf glpLoadMatrixd glpMultMatrixf glpMultMatrixd
		gluOrtho2D
		gluPerspective
		gluLookAt
		glupPickMatrix

		glpRasterFont
		glpPrintString
		glPolygonOffsetEXT

END

for(@consts) {getconsts($_)}

print join '',map {"$_\n"} (sort @subnames),"",(sort keys %consts);

print <<'END';
	);
	bootstrap PDL::Graphics::OpenGL;
END
print join '',map {"sub $_ () {$consts{$_}}\n"} (sort keys %consts);
print <<'END';

%window_defaults=(
                'x'     => 0,
                'y'     => 0,
                'width' => 500,
                'height'=> 500,
                'parent'=> 0,
                'mask'  => StructureNotifyMask,
                'attributes'=> [GLX_RGBA],
        );
sub glpOpenWindow {
        # default values
        my(%a) = @_;
        my(%p) = %window_defaults;
        foreach $k (keys(%a)){
                defined($p{$k}) || warn "Not a valid parameter to glpOpenWindow: `$k'
\n";
                #print "parameter $k now ",$a{$k}," was ",$p{$k},"\n";
                $p{$k} = $a{$k};
        }
        glpcOpenWindow($p{x},$p{y},$p{width},$p{height},
                       $p{parent},$p{'mask'},
                       @{$p{attributes}});
}
1;

END


####
#### Here is some verbatim code for the beginning of OpenGL.xs
####

__DATA__
/*
 *  file OpenGL.xs is generated by OpenGL.xs.gen 
 */
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>
#include <unistd.h>
#include <stdio.h>

#define NUM_ARG 6

Display *dpy;
XVisualInfo *vi;
Colormap cmap;
XSetWindowAttributes swa;
Window win;
GLXContext cx;

#define OPENGL_NOVIRT
#include "OpenGL.m"

static OpenGLVTab vtab;
OpenGLVTab *OpenGLVPtr;

static int default_attributes[] = { GLX_RGBA, /*GLX_DOUBLEBUFFER,*/  None };
static Bool WaitForNotify(Display *d, XEvent *e, char *arg) {
    return (e->type == MapNotify) && (e->xmap.window == (Window)arg);
}


MODULE = PDL::Graphics::OpenGL		PACKAGE = PDL::Graphics::OpenGL


void
glpcOpenWindow(x,y,w,h,pw,event_mask, ...)
	int	x
	int	y
	int	w
	int	h
	int	pw
	long	event_mask
	CODE:
	{
	    XEvent event;
	    Window pwin=(Window)pw;
	    int *attributes = default_attributes;
	    if(items>NUM_ARG){
	        int i;
	        attributes = (int *)malloc((items-NUM_ARG+1)* sizeof(int));
	        for(i=NUM_ARG;i<items;i++) {
	            attributes[i-NUM_ARG]=SvIV(ST(i));
	        }
	        attributes[items-NUM_ARG]=None;
	    }
	    /* get a connection */
	    dpy = XOpenDisplay(0);
	    if (!dpy) { fprintf(stderr, "No display!\n");exit(-1);}
	
	    /* get an appropriate visual */
	    vi = glXChooseVisual(dpy, DefaultScreen(dpy),attributes);
	    if(!vi) { fprintf(stderr, "No visual!\n");exit(-1);}
	
	    /* create a GLX context */
	    cx = glXCreateContext(dpy, vi, 0, GL_FALSE);
	    if(!cx){fprintf(stderr, "No context!\n");exit(-1);}
	
	    /* create a color map */
	    cmap = XCreateColormap(dpy, RootWindow(dpy, vi->screen),
				   vi->visual, AllocNone);
	
	    /* create a window */
	    swa.colormap = cmap;
	    swa.border_pixel = 0;
	    swa.event_mask = event_mask;
	    if(!pwin){pwin=RootWindow(dpy, vi->screen);}
	    if(x>=0) {
		    win = XCreateWindow(dpy, pwin, 
					x, y, w, h,
					0, vi->depth, InputOutput, vi->visual,
					CWBorderPixel|CWColormap|CWEventMask, &swa);
		    if(!win) {
			fprintf(stderr, "No Window\n");
			exit(-1);
		    }
		    XMapWindow(dpy, win);
		    if(event_mask & StructureNotifyMask) {
			XIfEvent(dpy, &event, WaitForNotify, (char*)win);
		    }
	    } else {
#ifdef FOIFJOISEJFOISJEFSF
			/* This doesn't work too well because of
			   trouble */
	    	    win = XCreatePixmap(dpy, pwin, w,h,vi->depth);
		    if(!win) {
			fprintf(stderr, "No Pixmap\n");
			exit(-1);
		    }
		    win = glXCreateGLXPixmap(dpy, vi, win);
		    if(!win) {
			fprintf(stderr, "No GLXPixmap\n");
			exit(-1);
		    }
#endif
#ifdef SOEIFJSOEIFS_____GL_SGIX_pbuffer
GLXFBConfig *glXChooseFBConfigSGIX(Display *dpy, int screen,
                     const int *attrib_list, int *nitems)


		win = glXCreateGLXPbufferSGIX(Display *dpy, 
				GLXFBConfig config,
				  unsigned int *width, unsigned int *height, int attrib_list)
#else
		    die("NO PBUFFER EXTENSION\n");
#endif
	    }

	    /* connect the context to the window */
	    if(!glXMakeCurrent(dpy, win, cx)) {
	        fprintf(stderr, "Non current\n");
	        exit(-1);
	    }
	
	    /* clear the buffer */
	    glClearColor(0,0,0,1);
	}

int
glpXConnectionNumber(d=dpy)
	void *d
	CODE:
	{
		Display *dp = d;
		RETVAL = ConnectionNumber(dp);
	}
	OUTPUT:
	RETVAL

# If glpOpenWindow was used then glXSwapBuffers should be called
# without parameters (i.e. use the default parameters)

void
glXSwapBuffers(d=dpy,w=win)
	void *	d
	GLXDrawable	w
	CODE:
	{
	    glXSwapBuffers(d,w);
	}


int
XPending(d=dpy)
	void *	d
	CODE:
	{
		RETVAL = XPending(d);
	}
	OUTPUT:
	RETVAL

void
glpXNextEvent(d=dpy)
	void *	d
	PPCODE:
	{
		XEvent event;
		char buf[10];
		KeySym ks;
		XNextEvent(d,&event);
		switch(event.type) {
			case ConfigureNotify:
				EXTEND(sp,3);
				PUSHs(sv_2mortal(newSViv(event.type)));
				PUSHs(sv_2mortal(newSViv(event.xconfigure.width)));
				PUSHs(sv_2mortal(newSViv(event.xconfigure.height)));				
				break;
			case KeyPress:
			case KeyRelease:
				EXTEND(sp,2);
				PUSHs(sv_2mortal(newSViv(event.type)));
				XLookupString(&event.xkey,buf,sizeof(buf),&ks,0);
				buf[0]=(char)ks;buf[1]='\0';
				PUSHs(sv_2mortal(newSVpv(buf,1)));
				break;
			case ButtonPress:
			case ButtonRelease:
				EXTEND(sp,7);
				PUSHs(sv_2mortal(newSViv(event.type)));
				PUSHs(sv_2mortal(newSViv(event.xbutton.button)));
				PUSHs(sv_2mortal(newSViv(event.xbutton.x)));
				PUSHs(sv_2mortal(newSViv(event.xbutton.y)));
				PUSHs(sv_2mortal(newSViv(event.xbutton.x_root)));
				PUSHs(sv_2mortal(newSViv(event.xbutton.y_root)));
				PUSHs(sv_2mortal(newSViv(event.xbutton.state)));
				break;
			case MotionNotify:
				EXTEND(sp,4);
				PUSHs(sv_2mortal(newSViv(event.type)));
				PUSHs(sv_2mortal(newSViv(event.xmotion.state)));
				PUSHs(sv_2mortal(newSViv(event.xmotion.x)));
				PUSHs(sv_2mortal(newSViv(event.xmotion.y)));
				break;
			case Expose:
			default:
				EXTEND(sp,1);
				PUSHs(sv_2mortal(newSViv(event.type)));
				break;
		}
	}

void
glpXQueryPointer(d=dpy,w=win)
	void *	d
	GLXDrawable	w
	PPCODE:
	{
		int x,y,rx,ry;
		Window r,c;
		unsigned int m;
		XQueryPointer(d,w,&r,&c,&rx,&ry,&x,&y,&m);
		EXTEND(sp,3);
		PUSHs(sv_2mortal(newSViv(x)));
		PUSHs(sv_2mortal(newSViv(y)));
		PUSHs(sv_2mortal(newSViv(m)));
	}

int
glpRasterFont(name,base,number,d=dpy)
	char *name
	int base
	int number
	void *d
	CODE:
	{
		XFontStruct *fi;
		int lb;
		fi = XLoadQueryFont(d,name);
		if(fi == NULL) {
			die("No font %s found",name);
		}
		lb = glGenLists(number);
		if(lb == 0) {
			die("No display lists left for font %s (need %d)",name,number);
		}
		glXUseXFont(fi->fid, base, number, lb);
		RETVAL=lb;
	}
	OUTPUT:
	RETVAL

void
glpPrintString(base,str)
	int base
	char *str
	CODE:
	{
		glPushAttrib(GL_LIST_BIT);
		glListBase(base);
		glCallLists(strlen(str),GL_UNSIGNED_BYTE,(GLubyte*)str);
		glPopAttrib();
	}

#
# The following XSUBS were done by hand
# These are perl-ized versions of the corresponding opengl function
# The reason is that the API with respect to 
# arguments and/or return value differs from the C equivalent
# These functions are more elegant to call and provide better error checking
# than the equivalent counter-part that needs pointer arguments
#

void
glpClipPlane(p,a,b,c,d)
	GLenum	p
	GLdouble	a
	GLdouble	b
	GLdouble	c
	GLdouble	d
	CODE:
	{
		GLdouble e[4];
		e[0]=a;e[1]=b;e[2]=c;e[3]=d;
		glClipPlane(p,e);
	}

void
glpGetClipPlane(plane)
	GLenum	plane
	PPCODE:
	{
	    GLdouble equation[4];
	    glGetClipPlane(plane,equation);
	    EXTEND(sp,4);
	    PUSHs(sv_2mortal(newSVnv(equation[0])));
	    PUSHs(sv_2mortal(newSVnv(equation[1])));
	    PUSHs(sv_2mortal(newSVnv(equation[2])));
	    PUSHs(sv_2mortal(newSVnv(equation[3])));
	}

void
glpReadTex(file)
	char *	file
	CODE:
	{
	    GLsizei w,h;
	    int d,i;
	    char buf[250];
	    unsigned char *image;
	    FILE *fp;
	    fp=fopen(file,"r");
	    if(!fp) {
	        fprintf(stderr,"couldn't open file %s\n",file);
	        return;
	    }
	    fgets(buf,250,fp);
	    fgets(buf,250,fp);
	    fscanf(fp,"%d%d",&w,&h);
	    fscanf(fp,"%d",&d);
	    if(d != 255 || w<64 || h<64 || w>10000 || h>10000) {
	        fprintf(stderr,"error reading %s\n",file);
	        return;
	    }
	    image=(unsigned char *)malloc(w*h*3);
	    for(i=0;i<w*h*3;i++) {
		int v;
	        fscanf(fp,"%d",&v);
	        image[i]=(unsigned char) v;
	    }
	    fclose(fp);
	    glTexImage2D(GL_TEXTURE_2D, 0, 3, w,h, 
	                 0, GL_RGB, GL_UNSIGNED_BYTE,image);
	}


void
glpLoadMatrixd(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,ma,mb,mc,md,me,mf)
	GLdouble	m0
	GLdouble	m1
	GLdouble	m2
	GLdouble	m3
	GLdouble	m4
	GLdouble	m5
	GLdouble	m6
	GLdouble	m7
	GLdouble	m8
	GLdouble	m9
	GLdouble	ma
	GLdouble	mb
	GLdouble	mc
	GLdouble	md
	GLdouble	me
	GLdouble	mf
	CODE:
	{
		GLdouble m[16];
		m[0]= m0; m[1]= m1; m[2]= m2; m[3]= m3;
		m[4]= m4; m[5]= m5; m[6]= m6; m[7]= m7;
		m[8]= m8; m[9]= m9; m[10]=ma; m[11]=mb;
		m[12]=mc; m[13]=md; m[14]=me; m[15]=mf;
		glLoadMatrixd(m);
	}

void
glpMultMatrixd(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,ma,mb,mc,md,me,mf)
	GLdouble	m0
	GLdouble	m1
	GLdouble	m2
	GLdouble	m3
	GLdouble	m4
	GLdouble	m5
	GLdouble	m6
	GLdouble	m7
	GLdouble	m8
	GLdouble	m9
	GLdouble	ma
	GLdouble	mb
	GLdouble	mc
	GLdouble	md
	GLdouble	me
	GLdouble	mf
	CODE:
	{
		GLdouble m[16];
		m[0]= m0; m[1]= m1; m[2]= m2; m[3]= m3;
		m[4]= m4; m[5]= m5; m[6]= m6; m[7]= m7;
		m[8]= m8; m[9]= m9; m[10]=ma; m[11]=mb;
		m[12]=mc; m[13]=md; m[14]=me; m[15]=mf;
		glMultMatrixd(m);
	}

void
glpLoadMatrixf(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,ma,mb,mc,md,me,mf)
	GLfloat	m0
	GLfloat	m1
	GLfloat	m2
	GLfloat	m3
	GLfloat	m4
	GLfloat	m5
	GLfloat	m6
	GLfloat	m7
	GLfloat	m8
	GLfloat	m9
	GLfloat	ma
	GLfloat	mb
	GLfloat	mc
	GLfloat	md
	GLfloat	me
	GLfloat	mf
	CODE:
	{
		GLfloat m[16];
		m[0]= m0; m[1]= m1; m[2]= m2; m[3]= m3;
		m[4]= m4; m[5]= m5; m[6]= m6; m[7]= m7;
		m[8]= m8; m[9]= m9; m[10]=ma; m[11]=mb;
		m[12]=mc; m[13]=md; m[14]=me; m[15]=mf;
		glLoadMatrixf(m);
	}


void
glpMultMatrixf(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,ma,mb,mc,md,me,mf)
	GLfloat	m0
	GLfloat	m1
	GLfloat	m2
	GLfloat	m3
	GLfloat	m4
	GLfloat	m5
	GLfloat	m6
	GLfloat	m7
	GLfloat	m8
	GLfloat	m9
	GLfloat	ma
	GLfloat	mb
	GLfloat	mc
	GLfloat	md
	GLfloat	me
	GLfloat	mf
	CODE:
	{
		GLfloat m[16];
		m[0]= m0; m[1]= m1; m[2]= m2; m[3]= m3;
		m[4]= m4; m[5]= m5; m[6]= m6; m[7]= m7;
		m[8]= m8; m[9]= m9; m[10]=ma; m[11]=mb;
		m[12]=mc; m[13]=md; m[14]=me; m[15]=mf;
		glMultMatrixf(m);
	}


#
# Here are the glu ones that have been done so far:
#


void
gluOrtho2D(left,right,bottom,top)
	GLdouble	left
	GLdouble	right
	GLdouble	bottom
	GLdouble	top

void
gluPerspective(fovy,aspect,zNear,zFar)
	GLdouble	fovy
	GLdouble	aspect
	GLdouble	zNear
	GLdouble	zFar

void
gluLookAt(eyex,eyey,eyez,centerx,centery,centerz,upx,upy,upz)
	GLdouble	eyex
	GLdouble	eyey
	GLdouble	eyez
	GLdouble	centerx
	GLdouble	centery
	GLdouble	centerz
	GLdouble	upx
	GLdouble	upy
	GLdouble	upz


void
glupPickMatrix(x,y,width,height,vp1,vp2,vp3,vp4)
	GLdouble	x
	GLdouble	y
	GLdouble	width
	GLdouble	height
	GLint vp1
	GLint vp2
	GLint vp3
	GLint vp4
	CODE:
	{
		GLint vp[4];
		vp[0] = vp1; vp[1]=vp2; vp[2]=vp3; vp[3]=vp4;
		gluPickMatrix(x,y,width,height,vp);
	}

void
glPolygonOffsetEXT(factor,bias)
	GLfloat factor
	GLfloat bias
	CODE:
	{
		#ifdef GL_EXT_polygon_offset
			extern void glPolygonOffsetEXT(GLfloat factor, GLfloat units);
			glPolygonOffsetEXT(factor,bias);
		#endif
	}

#
#  Some of the following XSUBS functions have 1 or more pointers as arguments
#  The type was changed to "char *" for each of these so you can
#  use perl's pack() routine to pass the required data to the C function.
#  See examples/texture for an example using glTexImage2D().
#  Be sure to pack your data properly (see the man page for the 
#  function) there is no sanity check here.  Use a glpFunctionName() or
#  a non-vector equivalent wherever possible
#
#  and now the gl XSUBs that were generated: 
#
##################################################



