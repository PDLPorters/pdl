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

use strict;
use warnings;
use File::Path;
use File::Basename;
use ExtUtils::Manifest;
require Exporter;
use Config;
eval { require Devel::CheckLib };

our @ISA    = qw( Exporter );

our @EXPORT = qw( isbigendian
	     PDL_INCLUDE PDL_TYPEMAP
	     PDL_AUTO_INCLUDE PDL_BOOT
		 PDL_INST_INCLUDE PDL_INST_TYPEMAP
		 pdlpp_postamble_int pdlpp_stdargs_int
		 pdlpp_postamble pdlpp_stdargs write_dummy_make
                unsupported getcyglib trylink
                pdlpp_mkgen
                got_complex_version
		 );

# Installation locations
# beware: whereami_any now appends the /Basic or /PDL directory as appropriate

# Return library locations
sub PDL_INCLUDE { '"-I'.whereami_any().'/Core"' };
sub PDL_TYPEMAP { whereami_any().'/Core/typemap' };

# The INST are here still just in case we want to change something later.
*PDL_INST_INCLUDE = \&PDL_INCLUDE;
*PDL_INST_TYPEMAP = \&PDL_TYPEMAP;

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

use Cwd qw/abs_path/;
my $MY_FILE = abs_path(__FILE__); # capture at load-time because EUMM chdirs
my $MY_DIR2 = dirname(dirname($MY_FILE));
my $IS_INST = $MY_DIR2 =~ /PDL\W*$/i;
sub whereami_any { $MY_DIR2 } # something containing "Core/Dev.pm"

# true arg = something containing "Core/Dev.pm"
# no good if want "Config.pm" as varies between installed and dev tree
# hence extra logic in PDL::Config-finding below
sub whereami {
  return undef if $IS_INST;
  return $MY_DIR2 if $_[0];
  dirname($MY_DIR2);
}

sub whereami_inst {
  return undef if !$IS_INST;
  return $MY_DIR2 if $_[0];
  dirname($MY_DIR2);
}

# To access PDL's configuration use %PDL::Config. Makefile.PL has been set up
# to create this variable so it is available during 'perl Makefile.PL' and
# it can be eval-ed during 'make'
unless ( %PDL::Config ) {
    # look for the distribution and then the installed version
    # (a manual version of whereami_any)
    require File::Spec::Functions;
    my $dir = whereami(1);
    if ( defined $dir ) {
	$dir = File::Spec::Functions::catdir($dir, qw(Core));
    } else {
	# as no argument given whereami_inst will die if it fails
        # (and it also returns a slightly different path than whereami(1)
        #  does, since it does not include "/PDL")
	$dir = File::Spec::Functions::catdir(whereami_inst, qw(PDL));
    }
    eval { require "$dir/Config.pm" };
    die "Unable to find PDL's configuration info\n [$@]" if $@;
}

my $inc = $PDL::Config{MALLOCDBG}{include} || '';
my $libs = $PDL::Config{MALLOCDBG}{libs} || '';

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

sub _oneliner {
  my ($cmd) = @_;
  require ExtUtils::MM;
  my $MM = bless { NAME => 'Fake' }, 'MM';
  $MM->oneliner($cmd);
}

# Expects list in format:
# [gtest.pd, GTest, PDL::GTest[, PDL::XSPkg] ], [...]
# source,    prefix,module/package, optional pp_addxs destination
# The idea is to support in future several packages in same dir - EUMM
#   7.06 supports

# This is the function internal for PDL.

sub _pp_call_arg {
  "-MPDL::PP=".join ',', @_
}
sub _postamble {
  my ($w, $internal, $src, $pref, $mod, $callpack, $multi_c) = @_;
  $callpack //= '';
  $w =~ s%/((PDL)|(Basic))$%%;  # remove the trailing subdir
  my ($perlrun, $pmdep) = ($internal ? '$(PERLRUNINST)' : "\$(PERL) \"-I$w\"", $src);
  if ($internal) {
    require File::Spec::Functions;
    my $top = File::Spec::Functions::abs2rel($w);
    my $core = File::Spec::Functions::catdir($top, qw(Basic Core));
    my $coredeps = join ' ', map File::Spec::Functions::catfile($core, $_),
        qw(pdl.h pdlcore.h pdlthread.h pdlmagic.h Types.pm);
    my $gendep = File::Spec::Functions::catfile($top, qw(Basic Gen pm_to_blib));
    $pmdep .= " $coredeps $gendep";
  }
  my $pp_call_arg = _pp_call_arg($mod, $mod, $pref, $callpack, $multi_c);
  my $install = '';
  if (!$internal) {
    my $oneliner = _oneliner(qq{exit if \$ENV{DESTDIR}; use PDL::Doc; eval { PDL::Doc::add_module(q{$mod}); }});
    $install = qq|\n\ninstall ::\n\t\@echo "Updating PDL documentation database...";\n\t$oneliner\n|;
  }
  my @generanda = "$pref.xs";
  push @generanda, map "pp-$_.c", _pp_list_functions($src) if $multi_c;
qq|

$pref.pm : $pmdep
	$perlrun \"$pp_call_arg\" $src

@generanda : $pref.pm
	\$(NOECHO) \$(TOUCH) \$@
$install|
}

sub pdlpp_postamble_int {
  my $w = whereami_any();
  join '', map _postamble($w, 1, @$_[0..3], 1), @_;
}

# This is the function to be used outside the PDL tree.
# same format as pdlpp_postamble_int
sub pdlpp_postamble {
  my $w = whereami_any();
  join '', map _postamble($w, 0, @$_[0..3], 0), @_;
}

my %flist_cache;
sub _pp_list_functions {
  require File::Spec::Functions;
  my ($src) = @_;
  my $abs_src = File::Spec::Functions::rel2abs($src);
  if (!$flist_cache{$abs_src}) {
    my $w = whereami_any();
    my $typespm = File::Spec::Functions::catfile($w, qw(Core Types.pm));
    system $^X, "$typespm.PL", $typespm if !-f $typespm;
    require $typespm;
    local $INC{'PDL/Types.pm'} = 1;
    require ''.File::Spec::Functions::catfile($w, qw(Gen PP.pm));
    $flist_cache{$abs_src} = [ PDL::PP::list_functions($src) ];
  }
  @{ $flist_cache{$abs_src} };
}

sub _stdargs {
  my ($w, $internal, $src, $pref, $mod, $callpack, $multi_c) = @_;
  my @cbase = $pref;
  push @cbase, map "pp-$_", _pp_list_functions($src) if $multi_c;
  my @cfiles = map "$_.c", @cbase;
  my @objs = map "$_\$(OBJ_EXT)", @cbase;
  (
    NAME  	=> $mod,
    VERSION_FROM => ($internal ? "$w/Basic/PDL.pm" : $src),
    TYPEMAPS     => [PDL_TYPEMAP()],
    OBJECT       => join(' ', @objs),
    PM 	=> {"$pref.pm" => "\$(INST_LIBDIR)/$pref.pm"},
    MAN3PODS => {"$pref.pm" => "\$(INST_MAN3DIR)/$mod.\$(MAN3EXT)"},
    INC          => PDL_INCLUDE()." $inc",
    LIBS         => [$libs],
    clean        => {FILES => "$pref.xs $pref.pm @cfiles"},
    ($internal
      ? (NO_MYMETA => 1)
      : (dist => {PREOP => '$(PERLRUNINST) -MPDL::Core::Dev -e pdlpp_mkgen $(DISTVNAME)' })
    ),
  );
}

sub pdlpp_stdargs_int {
  my $w = whereami();
  _stdargs($w, 1, @{$_[0]}[0..3], 1);
}

sub pdlpp_stdargs {
  _stdargs(undef, 0, @{$_[0]}[0..3], 0);
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
    my $pp_call_arg = _pp_call_arg($mod, $mod, $prefix, '');
    my $rv = system($^X, @in, $pp_call_arg, $pd);
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

  my $tempd = File::Temp::tempdir(CLEANUP=>1) || die "trylink: could not make temp dir";

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

sub generate_core_flags {
    # access (read, if set is true then write as well; if postset true then
    #         read first and write new value after that)
    # to ndarray's state
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

=head2 got_complex_version

=for ref

  PDL::Core::Dev::got_complex_version($func_name, $num_params)

For a given function appearing in C99's C<complex.h>, will return a
boolean of whether the system being compiled on has the complex version
of that. E.g. for C<sin>, will test whether C<csin> exists.

=cut

my %got_complex_cache;
sub got_complex_version {
    my ($name, $params) = @_;
    return $got_complex_cache{$name} if defined $got_complex_cache{$name};
    my $args = join ',', ('(double complex)1') x $params;
    $got_complex_cache{$name} = Devel::CheckLib::check_lib(
        ($Config{gccversion} ? (ccflags => '-O0') : ()), # stop GCC optimising test code away
        lib => 'm',
        header => 'complex.h',
        function => sprintf('double num; num = creal(c%s(%s)); return 0;', $name, $args),
    );
}

1;
