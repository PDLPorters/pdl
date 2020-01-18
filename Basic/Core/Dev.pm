=head1 NAME

PDL::Core::Dev - PDL development module

=head1 DESCRIPTION

This module encapsulates most of the stuff useful for
PDL development and is often used from within Makefile.PL's.

=head1 SYNOPSIS

   use PDL::Core::Dev;

=head1 FUNCTIONS

=cut

# Stuff used in development/install environment of PDL Makefile.PL's
# - not part of PDL itself.

package PDL::Core::Dev;

use File::Path;
use File::Basename;
use ExtUtils::Manifest;
use English; require Exporter;

@ISA    = qw( Exporter );

@EXPORT = qw( isbigendian genpp %PDL_DATATYPES
	     PDL_INCLUDE PDL_TYPEMAP
	     PDL_AUTO_INCLUDE PDL_BOOT
		 PDL_INST_INCLUDE PDL_INST_TYPEMAP
		 pdlpp_postamble_int pdlpp_stdargs_int
		 pdlpp_postamble pdlpp_stdargs write_dummy_make
                unsupported getcyglib trylink
                pdlpp_mkgen
		 );

# Installation locations
# beware: whereami_any now appends the /Basic or /PDL directory as appropriate

# The INST are here still just in case we want to change something later.

# print STDERR "executing PDL::Core::Dev from",join(',',caller),"\n";

# Return library locations

sub PDL_INCLUDE { '"-I'.whereami_any().'/Core"' };
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
  my ($symname, $module) = @_;
  $symname ||= 'PDL';
  $module ||= 'The code';
  return << "EOR";

   perl_require_pv ("PDL/Core.pm"); /* make sure PDL::Core is loaded */
#ifndef aTHX_
#define aTHX_
#endif
   if (SvTRUE (ERRSV)) Perl_croak(aTHX_ "%s",SvPV_nolen (ERRSV));
   CoreSV = perl_get_sv("PDL::SHARE",FALSE);  /* SV* value */
   if (CoreSV==NULL)
     Perl_croak(aTHX_ "We require the PDL::Core module, which was not found");
   $symname = INT2PTR(Core*,SvIV( CoreSV ));  /* Core* value */
   if ($symname->Version != PDL_CORE_VERSION)
     Perl_croak(aTHX_ "[$symname->Version: \%d PDL_CORE_VERSION: \%d XS_VERSION: \%s] $module needs to be recompiled against the newly installed PDL", $symname->Version, PDL_CORE_VERSION, XS_VERSION);

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

#
# To access PDL's configuration use %PDL::Config. Makefile.PL has been set up
# to create this variable so it is available during 'perl Makefile.PL' and
# it can be eval-ed during 'make'

unless ( %PDL::Config ) {

    # look for the distribution and then the installed version
    # (a manual version of whereami_any)
    #
    my $dir;
    $dir = whereami(1);
    if ( defined $dir ) {
	$dir = abs_path($dir . "/Core");
    } else {
	# as no argument given whereami_inst will die if it fails
        # (and it also returns a slightly different path than whereami(1)
        #  does, since it does not include "/PDL")
	#
	$dir = whereami_inst;
	$dir = abs_path($dir . "/PDL");
    }

    my $dir2 = $dir;
    $dir2 =~ s/\}/\\\}/g;
    eval sprintf('require q{%s/Config.pm};', $dir2);

    die "Unable to find PDL's configuration info\n [$@]"
	if $@;
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

my $inc = defined $PDL::Config{MALLOCDBG}->{include} ?
  "$PDL::Config{MALLOCDBG}->{include}" : '';
my $libs = defined $PDL::Config{MALLOCDBG}->{libs} ?
  "$PDL::Config{MALLOCDBG}->{libs}" : '';

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
#           PDL_Long *xx = x.data;
#           for(i=0; i<nvals; i++)
#              xx[i] = i/nvals;
#        }break;
#
#     case PDL_F:
#        {
#           PDL_Float *xx = x.data;
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

      if ( m/ (\s*)? \b GENERICLOOP \s* \( ( [^\)]* ) \) ( \s*; )? /x ){  # Start of generic code
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

      if ( m/ \b ENDGENERICLOOP ( \s*; )? /x ) {

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

   for $case (PDL::Types::typesrtkeys()){

     $type = $PDL_DATATYPES{$case};

     my $ppsym = $PDL::Types::typehash{$case}->{ppsym};
     print $indent,"case $case:\n"; # Start of this case
     print $indent,"   {";

     # Now output actual code with substutions

     for  (@gencode) {
        $line = $_;

        $line =~ s/\bgeneric\b/$type/g;
        $line =~ s/\bgeneric_ppsym\b/$ppsym/g;

        print "   ",$line;
     }

     print "}break;\n\n";  # End of this case
   }
   print $indent,"default:\n";
   print $indent,'   croak ("Not a known data type code=%d",'.$loopvar.");\n";
   print $indent,"}";

}

sub _oneliner {
  my ($cmd) = @_;
  require ExtUtils::MM;
  my $MM = bless { NAME => 'Fake' }, 'MM';
  $MM->oneliner($cmd);
}

sub genpp_cmdline {
  my ($in, $out) = @_;
  my $devpm = whereami_any()."/Core/Dev.pm";
  sprintf(_oneliner(<<'EOF'), $devpm) . qq{ "$in" > "$out"};
require "%s"; PDL::Core::Dev->import(); genpp();
EOF
}


# Standard PDL postamble
#
# This is called via .../Gen/Inline/Pdlpp.pm, in the case that the INTERNAL
# flag for the compilation is off (grep "ILSM" in that file to find the reference).
# If it's ON, then postamble_int gets called instead.


sub postamble {
  my ($self) = @_;
  sprintf <<'EOF', genpp_cmdline(qw($< $@));

# Rules for the generic preprocessor

.SUFFIXES: .g
.g.c :
	%s

EOF
}

# Expects list in format:
# [gtest.pd, GTest, PDL::GTest,     ['../GIS/Proj', ...] ], [...]
# source,    prefix,module/package, optional deps
# The idea is to support in future several packages in same dir - EUMM
#   7.06 supports
# each optional dep is a relative dir that a "make" will chdir to and
# "make" first - so the *.pd file can then "use" what it makes

# This is the function internal for PDL.

sub pdlpp_postamble_int {
	join '',map { my($src,$pref,$mod, $deps) = @$_;
        die "If give dependencies, must be array-ref" if $deps and !ref $deps;
	my $w = whereami_any();
	$w =~ s%/((PDL)|(Basic))$%%;  # remove the trailing subdir
	my $top = File::Spec->abs2rel($w);
	my $basic = File::Spec->catdir($top, 'Basic');
	my $core = File::Spec->catdir($basic, 'Core');
	my $gen = File::Spec->catdir($basic, 'Gen');
        my $depbuild = '';
        for my $dep (@{$deps || []}) {
            my $target = '';
            if ($dep eq 'core') {
                $dep = $top;
                $target = ' core';
            }
            $dep =~ s#([\(\)])#\\$1#g; # in case of unbalanced (
            $depbuild .= _oneliner("exit(!(chdir q($dep) && !system(q(\$(MAKE)$target))))");
            $depbuild .= "\n\t";
        }
qq|

$pref.pm: $src $core/Types.pm
	$depbuild\$(PERLRUNINST) \"-MPDL::PP qw[$mod $mod $pref]\" $src

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
	my $oneliner = _oneliner(q{exit if \$ENV{DESTDIR}; use PDL::Doc; eval { PDL::Doc::add_module(q{$mod}); }});
qq|

$pref.pm: $src
	\$(PERL) "-I$w" \"-MPDL::PP qw[$mod $mod $pref]\" $src

$pref.xs: $pref.pm
	\$(TOUCH) \$@

$pref.c: $pref.xs

$pref\$(OBJ_EXT): $pref.c

install ::
	\@echo "Updating PDL documentation database...";
	$oneliner
|
	} (@_)
}

sub pdlpp_stdargs_int {
 my($rec) = @_;
 my($src,$pref,$mod) = @$rec;
 my $w = whereami();
 my $malloclib = exists $PDL::Config{MALLOCDBG}->{libs} ?
   $PDL::Config{MALLOCDBG}->{libs} : '';
 my $mallocinc = exists $PDL::Config{MALLOCDBG}->{include} ?
   $PDL::Config{MALLOCDBG}->{include} : '';
my $libsarg = $libs || $malloclib ? "$libs $malloclib " : ''; # for Win32
 return (
 	%::PDL_OPTIONS,
	 'NAME'  	=> $mod,
	 'VERSION_FROM' => "$w/Basic/Core/Version.pm",
	 'TYPEMAPS'     => [&PDL_TYPEMAP()],
	 'OBJECT'       => "$pref\$(OBJ_EXT)",
	 PM 	=> {"$pref.pm" => "\$(INST_LIBDIR)/$pref.pm"},
	 MAN3PODS => {"$pref.pm" => "\$(INST_MAN3DIR)/$mod.\$(MAN3EXT)"},
	 'INC'          => &PDL_INCLUDE()." $inc $mallocinc",
	 'LIBS'         => $libsarg ? [$libsarg] : [],
	 'clean'        => {'FILES'  => "$pref.xs $pref.pm $pref\$(OBJ_EXT) $pref.c"},
     (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
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
	 'LIBS'         => $libs ? ["$libs "] : [],
	 'clean'        => {'FILES'  => "$pref.xs $pref.pm $pref\$(OBJ_EXT) $pref.c"},
	 'dist'         => {'PREOP'  => '$(PERL) "-I$(INST_ARCHLIB)" "-I$(INST_LIB)" -MPDL::Core::Dev -e pdlpp_mkgen $(DISTVNAME)' },
     (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
 );
}

# pdlpp_mkgen($dir)
# - scans $dir/MANIFEST for all *.pd files and creates corresponding *.pm files
#   in $dir/GENERATED/ subdir; needed for proper doc rendering at metacpan.org
# - it is used in Makefile.PL like:
#     dist => { PREOP=>'$(PERL) -MPDL::Core::Dev -e pdlpp_mkgen $(DISTVNAME)' }
#   so all the magic *.pm generation happens during "make dist"
# - it is intended to be called as a one-liner:
#     perl -MPDL::Core::Dev -e pdlpp_mkgen DirName
#
sub pdlpp_mkgen {
  my $dir = @_ > 0 ? $_[0] : $ARGV[0];
  die "pdlpp_mkgen: unspecified directory" unless defined $dir && -d $dir;
  my $file = "$dir/MANIFEST";
  die "pdlpp_mkgen: non-existing '$dir/MANIFEST'" unless -f $file;

  my @pairs = ();
  my $manifest = ExtUtils::Manifest::maniread($file);
  for (keys %$manifest) {
    next if $_ !~ m/\.pd$/;     # skip non-pd files
    next if $_ =~ m/^(t|xt)\//; # skip *.pd files in test subdirs
    next unless -f $_;
    my $content = do { local $/; open my $in, '<', $_; <$in> };
    if ($content =~ /=head1\s+NAME\s+(\S+)\s+/sg) {
      push @pairs, [$_, $1];
    }
    else {
      warn "pdlpp_mkgen: unknown module name for '$_' (use proper '=head1 NAME' section)\n";
    }
  }

  my %added = ();
  for (@pairs) {
    my ($pd, $mod) = @$_;
    (my $prefix = $mod) =~ s|::|/|g;
    my $manifestpm = "GENERATED/$prefix.pm";
    $prefix = "$dir/GENERATED/$prefix";
    File::Path::mkpath(dirname($prefix));
    #there is no way to use PDL::PP from perl code, thus calling via system()
    my @in = map { "-I$_" } @INC, 'inc';
    my $rv = system($^X, @in, "-MPDL::PP qw[$mod $mod $prefix]", $pd);
    if ($rv == 0 && -f "$prefix.pm") {
      $added{$manifestpm} = "mod=$mod pd=$pd (added by pdlpp_mkgen)";
      unlink "$prefix.xs"; #we need only .pm
    }
    else {
      warn "pdlpp_mkgen: cannot convert '$pd'\n";
    }
  }

  if (scalar(keys %added) > 0) {
    #maniadd works only with this global variable
    local $ExtUtils::Manifest::MANIFEST = $file;
    ExtUtils::Manifest::maniadd(\%added);
  }
}

sub unsupported {
  my ($package,$os) = @_;
  "No support for $package on $os platform yet. Will skip build process";
}

sub write_dummy_make {
  my ($msg) = @_;
  $msg =~ s#\n*\z#\n#;
  $msg =~ s#^\s*#\n#gm;
  print $msg;
  require ExtUtils::MakeMaker;
  ExtUtils::MakeMaker::WriteEmptyMakefile(NAME => 'Dummy', DIR => []);
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

=item *

OPTIONS

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
  require File::Temp;
  my $cdir = sub { return File::Spec->catdir(@_)};
  my $cfile = sub { return File::Spec->catfile(@_)};
  use Config;

  # check if MakeMaker should be used to preprocess the libs
  for my $key(keys %$opt) {$opt->{lc $key} = $opt->{$key}}
  my $mmprocess = exists $opt->{makemaker} && $opt->{makemaker};
  my $hide = exists $opt->{hide} ? $opt->{hide} :
    exists $PDL::Config{HIDE_TRYLINK} ? $PDL::Config{HIDE_TRYLINK} : 1;
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

  my $HIDE = !$hide ? '' : '>/dev/null 2>&1';
  if($^O =~ /mswin32/i) {$HIDE = '>NUL 2>&1'}

  my $tempd;

  $tempd = File::Temp::tempdir(CLEANUP=>1) || die "trylink: could not make TEMPDIR";
  ### if($^O =~ /MSWin32/i) {$tempd = File::Spec->tmpdir()}
  ### else {
  ###    $tempd = $PDL::Config{TEMPDIR} ||
  ### }

  my ($tc,$te) = map {&$cfile($tempd,"testfile$_")} ('.c','');
  open FILE,">$tc" or die "trylink: couldn't open testfile `$tc' for writing, $!";
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

=head2 datatypes_switch

=for ref

prints on C<STDOUT> XS text for F<Core.xs>.

=cut

sub datatypes_switch {
  my $ntypes = $#PDL::Types::names;
  my @m;
  foreach my $i ( 0 .. $ntypes ) {
    my $type = PDL::Type->new( $i );
    my $typesym = $type->symbol;
    my $typeppsym = $type->ppsym;
    my $cname = $type->ctype;
    $cname =~ s/^PDL_//;
    push @m, "\tcase $typesym: retval.type = $typesym; retval.value.$typeppsym = PDL.bvals.$cname; break;";
  }
  print map "$_\n", @m;
}

=head2 generate_core_flags

=for ref

prints on C<STDOUT> XS text with core flags, for F<Core.xs>.

=cut

my %flags = (
    hdrcpy => { set => 1 },
    fflows => { FLAG => "DATAFLOW_F" },
    bflows => { FLAG => "DATAFLOW_B" },
    is_inplace => { FLAG => "INPLACE", postset => 1 },
    donttouch => { FLAG => "DONTTOUCHDATA" },
    allocated => { },
    vaffine => { FLAG => "OPT_VAFFTRANSOK" },
    anychgd => { FLAG => "ANYCHANGED" },
    dimschgd => { FLAG => "PARENTDIMSCHANGED" },
    tracedebug => { FLAG => "TRACEDEBUG", set => 1},
);
#if ( $bvalflag ) { $flags{baddata} = { set => 1, FLAG => "BADVAL" }; }

sub generate_core_flags {
    # access (read, if set is true then write as well; if postset true then
    #         read first and write new value after that)
    # to piddle's state
    foreach my $name ( sort keys %flags ) {
        my $flag = "PDL_" . ($flags{$name}{FLAG} || uc($name));
        if ( $flags{$name}{set} ) {
            print <<"!WITH!SUBS!";
int
$name(x,mode=0)
        pdl *x
        int mode
        CODE:
        if (items>1)
           { setflag(x->state,$flag,mode); }
        RETVAL = ((x->state & $flag) > 0);
        OUTPUT:
        RETVAL

!WITH!SUBS!
        } elsif ($flags{$name}{postset}) {
            print <<"!WITH!SUBS!";
int
$name(x,mode=0)
        pdl *x
        int mode
        CODE:
        RETVAL = ((x->state & $flag) > 0);
        if (items>1)
           { setflag(x->state,$flag,mode); }
        OUTPUT:
        RETVAL

!WITH!SUBS!
        } else {
            print <<"!WITH!SUBS!";
int
$name(self)
        pdl *self
        CODE:
        RETVAL = ((self->state & $flag) > 0);
        OUTPUT:
        RETVAL

!WITH!SUBS!
        }
    } # foreach: keys %flags
}

=head2 generate_badval_init

=for ref

prints on C<STDOUT> XS text with badval initialisation, for F<Core.xs>.

=cut

sub generate_badval_init {
  for my $type (PDL::Types::types()) {
    my $typename = $type->ctype;
    $typename =~ s/^PDL_//;
    my $bval = $type->defbval;
    if ($PDL::Config{BADVAL_USENAN} && $type->usenan) {
      # note: no defaults if usenan
      print "\tPDL.bvals.$typename = PDL.NaN_$type;\n"; #Core NaN value
    } else {
      print "\tPDL.bvals.$typename = PDL.bvals.default_$typename = $bval;\n";
    }
  }
#    PDL.bvals.Byte   = PDL.bvals.default_Byte   = UCHAR_MAX;
#    PDL.bvals.Short  = PDL.bvals.default_Short  = SHRT_MIN;
#    PDL.bvals.Ushort = PDL.bvals.default_Ushort = USHRT_MAX;
#    PDL.bvals.Long   = PDL.bvals.default_Long   = INT_MIN;

}

1;
