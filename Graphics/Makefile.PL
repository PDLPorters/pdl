use ExtUtils::MakeMaker;

@subdirs = ("PGPLOT", "LUT", "IIS", "Karma");

# we try and build unless WITH_3d == 0
$t = $PDL_CONFIG{WITH_3D};
if( defined($t) and $t == 0 ) {
	print "\n   Not building TriD or OpenGL. Turn on WITH_3D if this is incorrect.\n\n";
} else {
  print  "\n   Trying OpenGL configuration $PDL_CONFIG{OPENGL_LIBS}\n\n";
  
  $PDL_CONFIG{OPENGL_LIBS} = guessogllibs($PDL_CONFIG{OPENGL_LIBS});

  if(defined $PDL_CONFIG{OPENGL_LIBS}){
    print "\n    Success using $PDL_CONFIG{OPENGL_LIBS}\n\n";
	 unshift @subdirs,"TriD";
	 $PDL_CONFIG{WITH_3D}=1;
  }else{
	 warn "\n     COULD NOT link OpenGL \n".
		"    Not building OpenGL and TriD options!\n\n";
	 $PDL_CONFIG{WITH_3D}=0;
  }
}

#WriteMakefile(
#     'NAME' => 'PDL',
#     VERSION_FROM => '../Basic/Core/Version.pm',
#     DIR => [@subdirs]
#);
WriteMakefile(
        'NAME' => 'PDL',
        VERSION_FROM => '../Basic/Core/Version.pm',
        DIR => [@subdirs],
        LIB => ['-lm'],
);

=head1 NAME

trylink - a perl configure clone

=head1 SYNOPSYS

  if (trylink 'libGL', '', 'char glBegin(); glBegin();', '-lGL') {
    $libs = '-lGLU -lGL';
    $have_GL = 1;
  } else {
    $have_GL = 0;
  }

=head1 DESCRIPTION


=head2 trylink

try to link some C-code making up the body of a function
with a given set of library specifiers

return 1 if successful, 0 otherwise

=for usage

   trylink $infomsg, $include, $progbody, $libs [,$hide];

Takes 4 + 1 optional argument.

=over 5

=item *

an informational message to print (can be empty)

=item *

any commands to be included at the top of the generated C program
(typically something like C<#include "mylib.h">)

=item *

the body of the program (in function main)

=item *

library flags to use for linking. Used as is. Preprocessing
by MakeMaker has to be performed before passing into C<trylink>
at the moment. See example below.

=item *

optional flag to specify if compilation output, etc produced
by trylink should be hidden or not. Hide is on by default.

=back

To ensure that your code will actually work after the link
specs have been processed by MakeMaker it is advisable to
preprocess your link spec (4th arg) using MakeMaker. The
following strategy seems to work:

  require ExtUtils::MakeMaker;
  require ExtUtils::Liblist;
  $libs = '-Lwhereever -lwhatever'; # arbitrary link spec
  my $self = new ExtUtils::MakeMaker {DIR =>  [],'NAME' => 'NONE'};
  my ($processed) = ExtUtils::Liblist::ext($self, $libs, 0);
  # now go on to actually let trylink have a go
  trylink 'libwhatever', $inc, $body, $processed;

=cut


sub trylink {
  my ($txt,$inc,$body,$libs,$hide) = @_;

  require File::Spec;
  my $fs = 'File::Spec';
  sub cdir { return $fs->catdir(@_)}
  sub cfile { return $fs->catfile(@_)}
  use Config;


  print "     Trying $txt...\n     " unless $txt =~ /^\s*$/;
  $hide = 1 unless defined $hide;

  my $HIDE = ($^O =~ /MSWin/) || !$hide ? '' : '>/dev/null 2>&1'; 
  my $td = $^O =~ /MSWin/ ? 'TEMP' : 'tmp';
  my $tempd = defined $ENV{TEMP} ? $ENV{TEMP} :
            defined $ENV{TMP} ? $ENV{TMP} :
                           cdir($fs->rootdir,$td);

  my ($tc,$te) = map {cfile($tempd,"testfile$_")} ('.c','');
  open FILE,">$tc" or die "couldn't open testfile for writing";
  my $prog = <<"EOF";
$inc

int main(void) {
$body

return 0;

}

EOF

  print FILE $prog;
  close FILE;
  # print "test prog:\n$prog\n";
  # print "now doing: $Config{cc} -o $te $tc $libs $HIDE ...\n";
  my $success = (system("$Config{cc} -o $te $tc $libs $HIDE") == 0) && 
    -e $te ? 1 : 0;
#  unlink "$te","$tc";
  print $success ? "\t\tYES\n" : "\t\tNO\n" unless $txt =~ /^\s*$/;
  return $success;
}


sub guessogllibs{
  my $libs=shift;
  use Config;
  my $have_GL;


  if(defined $libs){
	 if (trylink $libs,
		  '', 'char glBegin(); glBegin();',
		  libs($libs)) {
		return $libs;
	 }else{
      undef $libs;
		warn "     User specified OpenGL configuration $libs did not work"
		  ."\n     trying to guess...\n\n";
	 }
  }
  if (trylink 'libGL',
		'', 'char glBegin(); glBegin();',
		libs('-lGL')) {
	 $libs = '-lGLU -lGL';
  } elsif (trylink 'libMesaGL',
			  '', 'char glBegin(); glBegin();',
			  libs('-lMesaGL')) {
	 $libs = '-lMesaGLU -lMesaGL';
  } elsif (trylink 'libMesaGL with pthread',
			  '', 'char glBegin(); glBegin();',
			  libs('-lMesaGL -lpthread')) {
	 $libs = '-lMesaGLU -lMesaGL -lpthread';
  }

  return $libs;
}

sub libs ($) {
  require ExtUtils::MakeMaker;
  require ExtUtils::Liblist;
  my ($libs) = @_;
  my $lpath = '-L/usr/X11R6/lib -L/usr/lib/mesa';
  my $extra = '-lXext -lX11 -lm';
  my $all = "$lpath $libs $extra";

  my $self = new ExtUtils::MakeMaker {DIR =>  [],'NAME' => 'NONE'};
  my @libs = ExtUtils::Liblist::ext($self, $all, 0);
  print "processed EXTRALIBS: $libs[0]\n";
  return $libs[0]; # return what MakeMaker would use
}
