=head1 NAME

PDL::Core::Dev - PDL development module

=head1 DESCRIPTION

This module encapsulates most of the stuff useful for
PDL development and is often used from within Makefile.PL's.

=head1 SYNOPSIS

   use PDL::Core::Dev;
   if ($^O =~ /win32/i) {
    warn "Win32 systems not yet supported. Will not build PDL::IO::Browser";
    write_dummy_make(unsupported('PDL::XXX','win32'));
    return;
   }

=head1 FUNCTIONS

=cut   

# Stuff used in development/install environment of PDL Makefile.PL's
# - not part of PDL itself.

package PDL::Core::Dev;

use English; require Exporter;

@ISA    = qw( Exporter );

@EXPORT = qw( isbigendian genpp %PDL_DATATYPES 
	     PDL_INCLUDE PDL_TYPEMAP
	     PDL_AUTO_INCLUDE PDL_BOOT
		 PDL_INST_INCLUDE PDL_INST_TYPEMAP
		 pdlpp_postamble_int pdlpp_stdargs_int
		 pdlpp_postamble pdlpp_stdargs write_dummy_make
                unsupported getcyglib trylink
		 );

# Installation locations
# beware: whereami_any now appends the /Basic or /PDL directory as appropriate

# The INST are here still just in case we want to change something later.

# print STDERR "executing PDL::Core::Dev from",join(',',caller),"\n";

# Return library locations


sub PDL_INCLUDE { '-I'.whereami_any().'/Core' };
sub PDL_TYPEMAP { whereami_any().'/Core/typemap.pdl' };
# sub PDL_INST_INCLUDE { '-I'.whereami_any().'/Core' };
# sub PDL_INST_TYPEMAP { whereami_any().'/Core/typemap.pdl' };

sub PDL_INST_INCLUDE {&PDL_INCLUDE}
sub PDL_INST_TYPEMAP {&PDL_TYPEMAP}

sub PDL_AUTO_INCLUDE {
  my ($symname) = @_;
  $symname ||= 'PDL';
  return << "EOR";
#include <pdlcore.h>
static Core* $symname; /* Structure holds core C functions */
static SV* CoreSV;       /* Gets pointer to perl var holding core structure */
EOR
}

sub PDL_BOOT {
  my ($symname) = @_;
  $symname ||= 'PDL';
  return << "EOR";

   perl_require_pv ("PDL::Core"); /* make sure PDL::Core is loaded */
   CoreSV = perl_get_sv("PDL::SHARE",FALSE);  /* SV* value */
#ifndef aTHX_
#define aTHX_
#endif
   if (CoreSV==NULL)
     Perl_croak(aTHX_ "We require the PDL::Core module, which was not found");
   $symname = INT2PTR(Core*,SvIV( CoreSV ));  /* Core* value */
   if ($symname->Version != PDL_CORE_VERSION)
     Perl_croak(aTHX_ "The code needs to be recompiled against the newly installed PDL");

EOR
}

# whereami_any returns appended 'Basic' or 'PDL' dir as appropriate
use Cwd qw/abs_path/;
sub whereami_any {
	my $dir = (&whereami(1) or &whereami_inst(1) or
          die "Unable to determine ANY directory path to PDL::Core::Dev module\n");
	return abs_path($dir);
}

sub whereami {
   for $dir (@INC,qw|. .. ../.. ../../.. ../../../..|) {
      return ($_[0] ? $dir . '/Basic' : $dir)
	if -e "$dir/Basic/Core/Dev.pm";
   }
   die "Unable to determine UNINSTALLED directory path to PDL::Core::Dev module\n"
    if !$_[0];
    return undef;
}

sub whereami_inst {
   for $dir (@INC,map {$_."/blib"} qw|. .. ../.. ../../.. ../../../..|) {
      return ($_[0] ? $dir . '/PDL' : $dir)
	if -e "$dir/PDL/Core/Dev.pm";
   }
   die "Unable to determine INSTALLED directory path to PDL::Core::Dev module\n"
    if !$_[0];
   return undef;
}

# Data types to C types mapping
# get the map from Types.pm
{
# load PDL::Types only if it has not been previously loaded
  my $loaded_types = grep (m%(PDL|Core)/Types[.]pm$%, keys %INC);
  $@ = ''; # reset
  eval('require "'.whereami_any().'/Core/Types.pm"') # lets dist Types.pm win
    unless $loaded_types; # only when PDL::Types not yet loaded
  if($@) {  # if PDL::Types doesn't work try with full path (during build)
    my $foo = $@;
    $@="";
    eval('require PDL::Types');
    if($@) {
      die "can't find PDL::Types: $foo and $@" unless $@ eq "";
    }
  }
}
PDL::Types->import();

my $inc = defined $::PDL_CONFIG{MALLOCDBG}->{include} ?
  "$::PDL_CONFIG{MALLOCDBG}->{include}" : '';
my $libs = defined $::PDL_CONFIG{MALLOCDBG}->{libs} ?
  "$::PDL_CONFIG{MALLOCDBG}->{libs}" : '';

%PDL_DATATYPES = ();
foreach $key (keys %PDL::Types::typehash) {
    $PDL_DATATYPES{$PDL::Types::typehash{$key}->{'sym'}} =
	$PDL::Types::typehash{$key}->{'ctype'};
}

# non-blocking IO configuration

$O_NONBLOCK = defined $Config{'o_nonblock'} ? $Config{'o_nonblock'}
                : 'O_NONBLOCK';

=head2 isbigendian

=for ref

Is the machine big or little endian?

=for example

  print "Your machins is big endian.\n" if isbigendian();

returns 1 if the machine is big endian, 0 if little endian,
or dies if neither.  It uses the C<byteorder> element of
perl's C<%Config> array.

=for usage

   my $retval = isbigendian();

=cut

# ' emacs parsing dummy

# big/little endian?
sub isbigendian {
    use Config;
    my $byteorder = $Config{byteorder} || 
	die "ERROR: Unable to find 'byteorder' in perl's Config\n";
    return 1 if $byteorder eq "4321";
    return 1 if $byteorder eq "87654321";
    return 0 if $byteorder eq "1234";
    return 0 if $byteorder eq "12345678";
    die "ERROR: PDL does not understand your machine's byteorder ($byteorder)\n"; 
}

#################### PDL Generic PreProcessor ####################
#
# Preprocesses *.g files to *.c files allowing 'generic'
# type code which is converted to code for each type.
#
# e.g. the code:
#
#    pdl x;
#    GENERICLOOP(x.datatype)
#       generic *xx = x.data;
#       for(i=0; i<nvals; i++)
#          xx[i] = i/nvals;
#    ENDGENERICLOOP
#
# is converted into a giant switch statement:
#
#     pdl x;
#     switch (x.datatype) {
#
#     case PDL_L:
#        {
#           long *xx = x.data;
#           for(i=0; i<nvals; i++)
#              xx[i] = i/nvals;
#        }break;
#
#     case PDL_F:
#        {
#           float *xx = x.data;
#
#       .... etc. .....
#
# 'generic' is globally substituted for each relevant data type.
#
# This is used in PDL to write generic functions (taking pdl or void
# objects) which is still efficient with perl writing the actual C
# code for each type.
#
#     1st version - Karl Glazebrook 4/Aug/1996.
#
# Also makes the followings substitutions:
#
# (i) O_NONBLOCK - open flag for non-blocking I/O (5/Aug/96)
#


sub genpp {

   $gotstart = 0; @gencode = ();

   while (<>) { # Process files in @ARGV list - result to STDOUT

      # Do the miscellaneous substitutions first

      s/O_NONBLOCK/$O_NONBLOCK/go;   # I/O

      if ( /(\s*)?\bGENERICLOOP\s*\(([^\)]*)\)(\s*;)?/ ){  # Start of generic code
         #print $MATCH, "=$1=\n";

         die "Found GENERICLOOP while searching for ENDGENERICLOOP\n" if $gotstart;
         $loopvar = $2;
         $indent = $1;
         print $PREMATCH;

         @gencode = ();  # Start saving code
         push @gencode, $POSTMATCH;
         $gotstart = 1;
         next;
      }

      if ( /\bENDGENERICLOOP(\s*;)?/ ) {

         die "Found ENDGENERICLOOP while searching for GENERICLOOP\n" unless $gotstart;

         push @gencode, $PREMATCH;

         flushgeneric();  # Output the generic code

         print $POSTMATCH;  # End of genric code
         $gotstart = 0;
         next;
      }

      if ($gotstart) {
         push @gencode, $_;
      }
      else {
         print;
      }

   } # End while
}

sub flushgeneric {  # Construct the generic code switch

   print $indent,"switch ($loopvar) {\n\n";

   for $case (keys %PDL_DATATYPES) {

     $type = $PDL_DATATYPES{$case};

     print $indent,"case $case:\n"; # Start of this case
     print $indent,"   {";

     # Now output actual code with substutions

     for  (@gencode) {
        $line = $_;

        $line =~ s/\bgeneric\b/$type/g;

        print "   ",$line;
     }

     print "}break;\n\n";  # End of this case
   }
   print $indent,"default:\n";
   print $indent,'   croak ("Not a known data type code=%d",'.$loopvar.");\n";
   print $indent,"}";

}


# Standard PDL postamble

sub postamble {

  if ($^O =~ /win32/i) {
    open FI,'>./getdev.pl' or die "couldn't open getdev.pl";
    my $location = whereami_any();
    print FI << "EOD";

  require \"$location/Core/Dev.pm\";
  PDL::Core::Dev->import();				\
  genpp();
  1;
EOD
     close FI;
     return q~

# Rules for the generic preprocessor

.SUFFIXES: .g
.g.c:
	$(PERL) -e "require './getdev.pl'" $<  > $@

.c$(OBJ_EXT):
	$(CCCMD) $(CCCDLFLAGS) -I$(PERL_INC) $(DEFINE) $*.c

     ~;
} else {

q~

# Rules for the generic preprocessor

.SUFFIXES: .g
.g.c:
	$(PERL) -e 'require "~.whereami_any().q~/Core/Dev.pm"; \
		PDL::Core::Dev->import();				\
		genpp()' $<  > $@

.g$(OBJ_EXT):
	$(PERL) -e 'require "~.whereami_any().q~/Core/Dev.pm"; \
		PDL::Core::Dev->import();				\
		genpp()' $<  > $*.c
	$(CCCMD) $(CCCDLFLAGS) -I$(PERL_INC) $(DEFINE) $*.c

~;}

}

# Expects list in format:
# [gtest.pd, GTest, PDL::GTest], [...]
# source,    prefix,module/package
# The idea is to support in future several packages in same dir.

# This is the function internal for PDL.
# Later on, we shall provide another for use outside PDL.
#
# added badsupport.p as a requisite
sub pdlpp_postamble_int {
	join '',map { my($src,$pref,$mod) = @$_;
	my $w = whereami_any();
	$w =~ s%/((PDL)|(Basic))$%%;  # remove the trailing subdir
	my $core = "$w/Basic/Core";
	my $gen = "$w/Basic/Gen";
qq|

$pref.pm: $src $gen/pm_to_blib $core/badsupport.p $core/Types.pm
	\$(PERL) -I$w/blib/lib -I$w/blib/arch \"-MPDL::PP qw/$mod $mod $pref/\" $src

$pref.xs: $pref.pm
	\$(TOUCH) \$@

$pref.c: $pref.xs

$pref\$(OBJ_EXT): $pref.c
|
	} (@_)
}


# This is the function to be used outside the PDL tree.
sub pdlpp_postamble {
	join '',map { my($src,$pref,$mod) = @$_;
	my $w = whereami_any();
	$w =~ s%/((PDL)|(Basic))$%%;  # remove the trailing subdir
qq|

$pref.pm: $src
	\$(PERL) -I$w \"-MPDL::PP qw/$mod $mod $pref/\" $src

$pref.xs: $pref.pm
	\$(TOUCH) \$@

$pref.c: $pref.xs

$pref\$(OBJ_EXT): $pref.c
|
	} (@_)
}

sub pdlpp_stdargs_int {
 my($rec) = @_;
 my($src,$pref,$mod) = @$rec;
 my $w = whereami();
 my $malloclib = exists $PDL_CONFIG{MALLOCDBG}->{libs} ?
   $PDL_CONFIG{MALLOCDBG}->{libs} : '';
 my $mallocinc = exists $PDL_CONFIG{MALLOCDBG}->{include} ?
   $PDL_CONFIG{MALLOCDBG}->{include} : '';
 return (
 	%::PDL_OPTIONS,
	 'NAME'  	=> $mod,
	 'VERSION_FROM' => "$w/Basic/Core/Version.pm",
	 'TYPEMAPS'     => [&PDL_TYPEMAP()],
	 'OBJECT'       => "$pref\$(OBJ_EXT)",
	 PM 	=> {"$pref.pm" => "\$(INST_LIBDIR)/$pref.pm"},
	 MAN3PODS => {"$pref.pm" => "\$(INST_MAN3DIR)/$mod.\$(MAN3EXT)"},
	 'INC'          => &PDL_INCLUDE()." $inc $mallocinc",
	 'LIBS'         => ["$libs $malloclib "],
	 'clean'        => {'FILES'  => "$pref.xs $pref.pm $pref\$(OBJ_EXT) $pref.c"},
 );
}

sub pdlpp_stdargs {
 my($rec) = @_;
 my($src,$pref,$mod) = @$rec;
 return (
 	%::PDL_OPTIONS,
	 'NAME'  	=> $mod,
	 'TYPEMAPS'     => [&PDL_INST_TYPEMAP()],
	 'OBJECT'       => "$pref\$(OBJ_EXT)",
	 PM 	=> {"$pref.pm" => "\$(INST_LIBDIR)/$pref.pm"},
	 MAN3PODS => {"$pref.pm" => "\$(INST_MAN3DIR)/$mod.\$(MAN3EXT)"},
	 'INC'          => &PDL_INST_INCLUDE()." $inc",
	 'LIBS'         => ["$libs "],
	 'clean'        => {'FILES'  => "$pref.xs $pref.pm $pref\$(OBJ_EXT) $pref.c"},
 );
}


sub unsupported {
  my ($package,$os) = @_;
  "No support for $package on $os platform yet. Will skip build process";
}

sub write_dummy_make {
  require IO::File;
    my ($msg) = @_;
    print STDERR "writing dummy Makefile\n";
    my $fh = new IO::File "> Makefile" or die "can't open Makefile";
    print $fh <<"EOT";
fred:
	\@echo \"****\"
	\@echo \"$msg\"
	\@echo \"****\"

all: fred

test: fred

clean ::
	-mv Makefile Makefile.old

realclean ::
	rm -rf Makefile Makefile.old

EOT
    close($fh);
}

sub getcyglib {
my ($lib) = @_;
my $lp = `gcc -print-file-name=lib$lib.a`;
$lp =~ s|/[^/]+$||;
$lp =~ s|^([a-z,A-Z]):|//$1|g;
return "-L$lp -l$lib";
}

=head2 trylink

=for ref

a perl configure clone

=for example

  if (trylink 'libGL', '', 'char glBegin(); glBegin();', '-lGL') {
    $libs = '-lGLU -lGL';
    $have_GL = 1;
  } else {
    $have_GL = 0;
  }
  $maybe = 
    trylink 'libwhatever', $inc, $body, $libs, $cflags,
        {MakeMaker=>1, Hide=>0, Clean=>1};

Try to link some C-code making up the body of a function
with a given set of library specifiers

return 1 if successful, 0 otherwise

=for usage

   trylink $infomsg, $include, $progbody, $libs [,$cflags,{OPTIONS}];

Takes 4 + 2 optional arguments.

=over 5

=item *

an informational message to print (can be empty)

=item *

any commands to be included at the top of the generated C program
(typically something like C<#include "mylib.h">)

=item *

the body of the program (in function main)

=item *

library flags to use for linking. Preprocessing
by MakeMaker should be performed as needed (see options and example).

=item *

compilation flags. For example, something like C<-I/usr/local/lib>.
Optional argument. Empty if omitted.

=item OPTIONS

=over

=item MakeMaker

Preprocess library strings in the way MakeMaker does things. This is
advisable to ensure that your code will actually work after the link
specs have been processed by MakeMaker.

=item Hide

Controls if linking output etc is hidden from the user or not.
On by default except within the build of the PDL distribution
where the config value set in F<perldl.conf> prevails.

=item Clean

Remove temporary files. Enabled by default. You might want to switch
it off during debugging.

=back

=back

=cut


sub trylink {
  my $opt = ref $_[$#_] eq 'HASH' ? pop : {};
  my ($txt,$inc,$body,$libs,$cflags) = @_;
  $cflags ||= '';
  require File::Spec;
  my $fs = 'File::Spec';
  my $cdir = sub { return $fs->catdir(@_)};
  my $cfile = sub { return $fs->catfile(@_)};
  use Config;

  # check if MakeMaker should be used to preprocess the libs
  for my $key(keys %$opt) {$opt->{lc $key} = $opt->{$key}}
  my $mmprocess = exists $opt->{makemaker} && $opt->{makemaker};
  my $hide = exists $opt->{hide} ? $opt->{hide} : 
    exists $::PDL_CONFIG{HIDE_TRYLINK} ? $::PDL_CONFIG{HIDE_TRYLINK} : 1;
  my $clean = exists $opt->{clean} ? $opt->{clean} : 1;
  if ($mmprocess) {
      require ExtUtils::MakeMaker;
      require ExtUtils::Liblist;
      my $self = new ExtUtils::MakeMaker {DIR =>  [],'NAME' => 'NONE'};

      my @libs = $self->ext($libs, 0);

      print "processed LIBS: $libs[0]\n" unless $hide;
      $libs = $libs[0]; # replace by preprocessed libs
  }

  print "     Trying $txt...\n     " unless $txt =~ /^\s*$/;

  my $HIDE = ($^O =~ /MSWin/) || !$hide ? '' : '>/dev/null 2>&1'; 
  my $td = $^O =~ /MSWin/ ? 'TEMP' : 'tmp';
  my $tempd = defined $ENV{TEMP} ? $ENV{TEMP} :
            defined $ENV{TMP} ? $ENV{TMP} :
                           &$cdir($fs->rootdir,$td);

  my ($tc,$te) = map {&$cfile($tempd,"testfile$_")} ('.c','');
  open FILE,">$tc" or die "couldn't open testfile `$tc' for writing";
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
  # make sure we can overwrite the executable. shouldn't need this,
  # but if it fails and HIDE is on, the user will never see the error.	 
  open(T, ">$te") or die( "unable to write to test executable `$te'");
  close T;
  print "$Config{cc} $cflags -o $te $tc $libs $HIDE ...\n" unless $hide;
  my $success = (system("$Config{cc} $cflags -o $te $tc $libs $HIDE") == 0) && 
    -e $te ? 1 : 0;
  unlink "$te","$tc" if $clean;
  print $success ? "\t\tYES\n" : "\t\tNO\n" unless $txt =~ /^\s*$/;
  print $success ? "\t\tSUCCESS\n" : "\t\tFAILED\n"
    if $txt =~ /^\s*$/ && !$hide;
  return $success;
}

1; # Return OK

