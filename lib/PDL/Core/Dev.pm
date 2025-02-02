=head1 NAME

PDL::Core::Dev - PDL development module

=head1 SYNOPSIS

   use PDL::Core::Dev;

=head1 DESCRIPTION

This module encapsulates most of the stuff useful for
PDL development and is often used from within Makefile.PL's.

=head1 BUILD MODES

=head2 Old Skool

The original scheme, which still works, was one PDL module per
L<ExtUtils::MakeMaker> (EUMM) "module", i.e. directory. There has
been work with Module::Build, but as that is now deprecated, it
will not be further discussed.  That module would generate a C<.xs>
and C<.pm> file in its directory, according to its F<Makefile.PL>,
such as linking extra objects, or other libraries.

To have several distinct PDL modules in a CPAN
distribution, you made several directories, each with a F<Makefile.PL>.
Parallel building was not normally possible between modules, though
see PDL's top-level F<Makefile.PL> as of about 2015, which gained a
C<coretest> target that did build in parallel despite subdirectories
and "recursive make". Any change to any C code caused a rebuild of
the whole C<.xs> file (for Slices this was upwards of 40,000 lines),
and being one compilation unit, it could not be parallelised.

=head2 Multi-C files

As of 2.058, a new "multi-C" mode was added. For all internal PDL
modules, and external ones that opted in (by adding a 5th, true,
element to the array-ref describing the package). This creates one
C file per operation, so parallel building is possible. This makes
for quicker builds, both for those installing the package, and those
developing it.

It can also avoid unnecessary rebuilds if only one operation got
changed - it only recompiles that C file.
This is possible due to some trickiness by L<PDL::PP>,
which detects if each C file that it I<would> output is the same
as the one already on disk, first removing any C<#line> directives so
that line renumbering will not force a rebuild, and not writing
anything unless a real change happened.

It is opt-in because if the module adds C functions with C<pp_addhdr>
without scoping them appropriately, they get incorporated in each
generated C file, which causes linking problems. Moving those to
separate C files solves that.

But parallel building (without the "cleverness" of the C<coretest>
work) is only possible within each module.

=head2 Deep mode

EUMM pure-Perl distributions in the modern era have
a F<lib> directory, whose structure matches the hierarchy of modules.
PDL now uses this in its C<Basic> subdirectory, so there is e.g.
F<lib/PDL/Core.pm> under that. As of EUMM 7.12 (shipped with Perl
5.26), it's also possible
to put C<.xs> files next to their respective C<.pm> files, by giving
a true value for C<XSMULTI>.

As of 2.096, another new mode was added, which automatically engages
"multi-C" mode. This allows you to place your C<.pd> file under a
F<lib> directory, whose location tells the build system its package
name, which means the previous schemes' need to communicate that
in each F<Makefile.PL> is no more. Now, the configuration is
communicated by each C<.pd> file by setting values in a hash, e.g.:

  { no warnings 'once'; # pass info back to Makefile.PL
  $PDL::Core::Dev::EXTRAS{$::PDLMOD}{OBJECT} .= join '', map " $::PDLBASE/$_\$(OBJ_EXT)", qw(fftn);
  $PDL::Core::Dev::EXTRAS{$::PDLMOD}{DEFINE} .= qq{ -DFFT_FLOAT -DFFT_DOUBLE -DFFT_LDOUBLE};
  $PDL::Core::Dev::EXTRAS{$::PDLMOD}{INC} .= qq{ "-I$::PDLBASE"};
  }

This works because PDL needs to make an entry in the Makefile for
each operation defined, which it does by loading the C<.pd> file,
and making its version of C<pp_def> just record the operation name.
As a side effect, the setting of the C<EXTRAS> value can be seen
by the build process.

To have the only F<Makefile.PL> work in this new scheme, converting
it from the previous one(s), you need to add this key to the
C<WriteMakefile> call:

  VERSION_FROM => 'lib/PDL/GSL/CDF.pd',

Note that the ones supplied by C<pdlpp_stdargs> are added for you. You I<do>
need to provide overrides for C<postamble> as before, and also C<init_PM>:

  {
  my @pd_srcs;
  package MY; # so that "SUPER" works right
  sub init_PM {
    my ($self) = @_;
    $self->SUPER::init_PM;
    @pd_srcs = ::pdlpp_eumm_update_deep($self);
  }
  sub postamble { ::pdlpp_postamble(@pd_srcs) }
  }

=head1 FUNCTIONS

=cut

# Stuff used in development/install environment of PDL Makefile.PL's
# - not part of PDL itself.

package PDL::Core::Dev;

use strict;
use warnings;
use File::Path;
use File::Basename;
require Exporter;
use Config;
use File::Spec::Functions;
eval { require Devel::CheckLib };

our @ISA    = qw( Exporter );

our @EXPORT = qw( isbigendian
  PDL_INCLUDE PDL_TYPEMAP
  PDL_AUTO_INCLUDE PDL_BOOT
  PDL_INST_INCLUDE PDL_INST_TYPEMAP
  pdlpp_eumm_update_deep
  pdlpp_postamble_int pdlpp_stdargs_int
  pdlpp_postamble pdlpp_stdargs write_dummy_make
  unsupported trylink get_maths_libs
  pdlpp_mkgen
  got_complex_version
);

# Installation locations
sub PDL_INCLUDE { '"-I'.catdir(whereami_any(), 'Core').'"' }
sub PDL_TYPEMAP { catfile(whereami_any(), qw(Core typemap)) }

# The INST are here still just in case we want to change something later.
*PDL_INST_INCLUDE = \&PDL_INCLUDE;
*PDL_INST_TYPEMAP = \&PDL_TYPEMAP;

sub PDL_AUTO_INCLUDE {
  my ($symname) = @_;
  $symname ||= 'PDL';
  return << "EOR";
#include <pdlcore.h>
static Core* $symname; /* Structure holds core C functions */
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
   SV* CoreSV = perl_get_sv("PDL::SHARE",FALSE); /* var with core structure */
   if (!CoreSV)
     Perl_croak(aTHX_ "We require the PDL::Core module, which was not found");
   if (!($symname = INT2PTR(Core*,SvIV( CoreSV )))) /* Core* value */
     Perl_croak(aTHX_ "Got NULL pointer for $symname");
   if ($symname->Version != PDL_CORE_VERSION)
     Perl_croak(aTHX_ "[$symname->Version: \%ld PDL_CORE_VERSION: \%ld XS_VERSION: \%s] $module needs to be recompiled against the newly installed PDL", (long int)$symname->Version, (long int)PDL_CORE_VERSION, XS_VERSION);
EOR
}

use Cwd qw/abs_path/;
my $MY_FILE = abs_path(__FILE__); # capture at load-time because EUMM chdirs
my $MY_DIR2 = dirname(dirname($MY_FILE));
sub whereami_any { $MY_DIR2 } # something containing "Core/Dev.pm"

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
    my $byteorder = $Config{byteorder} ||
	die "ERROR: Unable to find 'byteorder' in perl's Config\n";
    return 1 if $byteorder eq "4321";
    return 1 if $byteorder eq "87654321";
    return 0 if $byteorder eq "1234";
    return 0 if $byteorder eq "12345678";
    die "ERROR: PDL does not understand your machine's byteorder ($byteorder)\n";
}

sub _oneliner {
  my ($cmd, @flags) = @_;
  require ExtUtils::MM;
  my $MM = bless { NAME => 'Fake' }, 'MM';
  $MM->oneliner($cmd, \@flags);
}

# Expects list in format:
# [gtest.pd, GTest, PDL::GTest[, PDL::XSPkg] ], [...]
# source,    prefix,module/package, optional pp_addxs destination
# The idea is to support in future several packages in same dir - EUMM
#   7.06 supports

my %flist_cache;
sub _pp_call_arg {
  "-MPDL::PP=".join ',', @_
}
sub _postamble {
  my ($w, $internal, $src, $base, $mod, $callpack, $multi_c, $deep) = @_;
  $callpack //= '';
  $w = dirname($w);
  my $perlrun = "\$(PERLRUN) \"-I$w\"";
  my ($pmdep, $install, $cdep) = ($src, '', '');
  my ($ppc, $ppo) = ($multi_c && $flist_cache{File::Spec::Functions::rel2abs($src)})
    ? map "\$($_)", pdlpp_mod_vars($mod)
    : pdlpp_mod_values($internal, $src, $base, $multi_c);
  if ($internal) {
    my $ppdir = File::Spec::Functions::abs2rel(catdir($w, qw(PDL)));
    $pmdep .= join ' ', '', catfile($ppdir, 'PP.pm'), glob(catfile($ppdir, 'PP/*'));
    $cdep .= join ' ', $ppo, ':', map catfile($ppdir, qw(Core), $_),
      qw(pdl.h pdlcore.h pdlbroadcast.h pdlmagic.h);
  } else {
    my $oneliner = _oneliner(qq{exit if \$ENV{DESTDIR}; use PDL::Doc; eval { PDL::Doc::add_module(q{$mod}); }});
    $install = qq|\ninstall ::\n\t\@echo "Updating PDL documentation database...";\n\t$oneliner\n|;
  }
  my $pp_call_arg = _pp_call_arg($mod, $mod, $base, $callpack, $multi_c||'',$deep||'');
qq|

$base.pm : $pmdep
	$perlrun \"$pp_call_arg\" $src
	\$(TOUCH) $base.pm

$ppc : $base.pm
	\$(NOECHO) \$(NOOP)

$cdep
$install|
}

sub pdlpp_postamble_int {
  my $w = whereami_any();
  join '', map _postamble($w, 1, @$_[0..3], 1, @$_[5..$#$_]), @_;
}

# This is the function to be used outside the PDL tree.
# same format as pdlpp_postamble_int
sub pdlpp_postamble {
  my $w = whereami_any();
  join '', map _postamble($w, 0, @$_), @_;
}

our %EXTRAS;
sub pdlpp_eumm_update_deep {
  my ($eumm) = @_;
  my $pm = $eumm->{PM};
  delete @$pm{grep /\Q$Config{obj_ext}\E$/, keys %$pm};
  my $macro = $eumm->{macro} ||= {};
  my $xsb = $eumm->{XSBUILD}{xs} ||= {};
  $eumm->{clean}{FILES} ||= '';
  $eumm->{OBJECT} ||= '';
  $eumm->{INC} ||= '';
  my $pdl_inc = PDL_INCLUDE();
  $eumm->{INC} .= ' '.$pdl_inc if index($eumm->{INC}, $pdl_inc) == -1;
  my $tms = $eumm->{TYPEMAPS} ||= [];
  my $pdl_tm = PDL_TYPEMAP();
  push @$tms, $pdl_tm if !grep $_ eq $pdl_tm, @$tms;
  $eumm->{XSMULTI} ||= 1;
  $eumm->{dist}{PREOP} ||= '$(PERLRUNINST) -MPDL::Core::Dev -e pdlpp_mkgen $(DISTVNAME)';
  my $xs = $eumm->{XS} ||= {};
  my $global_version = $eumm->parse_version($eumm->{VERSION_FROM});
  my @pd_srcs;
  for my $f (grep /\.pd$/, keys %$pm) {
    delete $pm->{$f};
    my $nolib = (my $base = $f =~ s/\.pd$//r) =~ s#^lib/##r;
    $xs->{ "$base.xs" } = "$base.c";
    my $pmfile = "$base.pm";
    $pm->{$pmfile} = "\$(INST_LIB)/$nolib.pm";
    my @macro_vars = pdlpp_mod_vars(my $mod = join '::', split /\//, $nolib);
    @$macro{@macro_vars} = pdlpp_mod_values(1, $f, $base, 1, 1);
    $eumm->{OBJECT} .= " $base\$(OBJ_EXT)";
    $xsb->{$base}{OBJECT} = "\$($macro_vars[1])";
    $xsb->{$base}{OBJECT} .= $EXTRAS{$f}{OBJECT} if $EXTRAS{$f}{OBJECT};
    $eumm->{DEFINE} .= $EXTRAS{$f}{DEFINE} if $EXTRAS{$f}{DEFINE}; # global
    $eumm->{INC} .= " $EXTRAS{$f}{INC}" if $EXTRAS{$f}{INC}; # global
    my $mtime = (stat $f)[9] // die "$f: $!";
    open my $fh, ">", $pmfile or die "$pmfile: $!"; # XSMULTI needs this
    print $fh "package $mod;\nour \$VER"."SION = '$global_version';\n1;\n"; # break is so cpanm doesn't try to parse as version
    close $fh;
    utime $mtime - 120, $mtime - 120, $pmfile; # so is out of date
    push @pd_srcs, [$f, $base, $mod, '', 1, 1];
    my $clean_extra = join ' ', '', $pmfile, map "\$($_)", @macro_vars;
    $clean_extra .= $EXTRAS{$f}{OBJECT} if $EXTRAS{$f}{OBJECT};
    if (ref $eumm->{clean}{FILES}) {
      push @{$eumm->{clean}{FILES}}, $clean_extra;
    } else {
      $eumm->{clean}{FILES} .= $clean_extra;
    }
  }
  delete @$pm{grep /\.c$/, keys %$pm};
  @pd_srcs;
}

sub pdlpp_list_functions {
  my ($src, $internal, $base) = @_;
  my $abs_src = File::Spec::Functions::rel2abs($src);
  if (!$flist_cache{$abs_src}) {
    my $w = whereami_any();
    if (!$INC{'PDL/Types.pm'}) {
      my $typespm = catfile($w, 'Types.pm');
      require $typespm;
      $INC{'PDL/Types.pm'} = 1;
    }
    require ''.catfile($w, qw(PP.pm));
    $::PDLBASE = $base;
    $flist_cache{$abs_src} = [ PDL::PP::list_functions($src) ];
  }
  @{ $flist_cache{$abs_src} };
}

sub pdlpp_mod_vars {
  my @parts = split /::/, $_[0];
  shift @parts if $parts[0] eq 'PDL';
  my $mangled = join '_', @parts;
  map "PDL_MULTIC_${mangled}_$_", qw(C O);
}
sub pdlpp_mod_values {
  my ($internal, $src, $base, $multi_c, $deep) = @_;
  return ("$base.xs", "$base\$(OBJ_EXT)") if !$multi_c;
  my $cfileprefix = $deep ? "$base-" : '';
  my @cbase = map $cfileprefix."pp-$_", pdlpp_list_functions($src, $internal, $base);
  (join(' ', "$base.xs", map "$_.c", @cbase),
    join(' ', map "$_\$(OBJ_EXT)", $base, @cbase));
}
sub _stdargs {
  my ($w, $internal, $src, $base, $mod, $callpack, $multi_c) = @_;
  my ($clean, %hash) = '';
  if ($multi_c) {
    my ($mangled_c, $mangled_o) = pdlpp_mod_vars($mod);
    my ($mangled_c_val, $mangled_o_val) = pdlpp_mod_values($internal, $src, $base, $multi_c);
    %hash = (%hash,
      macro        => {
        $mangled_c => $mangled_c_val, $mangled_o => $mangled_o_val,
      },
      OBJECT       => "\$($mangled_o)",
    );
    $clean .= " \$($mangled_c)";
  } else {
    %hash = (%hash, OBJECT => "$base\$(OBJ_EXT)");
    $clean .= " $base.xs";
  }
  if ($internal) {
    $hash{depend} = {
      "$base\$(OBJ_EXT)" => File::Spec::Functions::abs2rel(catfile($w, qw(PDL Core pdlperl.h))),
    };
  }
  (
    NAME  	=> $mod,
    VERSION_FROM => ($internal ? catfile($w, qw(PDL Core.pm)) : $src),
    TYPEMAPS     => [PDL_TYPEMAP()],
    PM 	=> {"$base.pm" => "\$(INST_LIBDIR)/$base.pm"},
    MAN3PODS => {"$base.pm" => "\$(INST_MAN3DIR)/$mod.\$(MAN3EXT)"},
    INC          => PDL_INCLUDE(),
    LIBS         => [''],
    clean        => {FILES => "$base.pm $base.c$clean"},
    %hash,
    ($internal
      ? (NO_MYMETA => 1)
      : (dist => {PREOP => '$(PERLRUNINST) -MPDL::Core::Dev -e pdlpp_mkgen $(DISTVNAME)' })
    ),
  );
}

sub pdlpp_stdargs_int {
  _stdargs(dirname($MY_DIR2), 1, @{$_[0]}[0..3], 1);
}

sub pdlpp_stdargs {
  _stdargs(undef, 0, @{$_[0]});
}

# pdlpp_mkgen($dir)
# - scans $dir/MANIFEST for all *.pd files and creates corresponding *.pm files
#   in $dir/GENERATED/ subdir; needed for proper doc rendering at metacpan.org
# - it is used in Makefile.PL like:
#     dist => { PREOP=>'$(PERL) -MPDL::Core::Dev -e pdlpp_mkgen $(DISTVNAME)' }
#   so all the magic *.pm generation happens during "make dist"
# - it is intended to be called as a one-liner:
#     perl -MPDL::Core::Dev -e pdlpp_mkgen DirName
# - it relies on finding "=head1 NAME" and the module name in *.pd, though can be in comment
#
sub pdlpp_mkgen {
  require File::Copy;
  require ExtUtils::Manifest;
  my $dir = @_ > 0 ? $_[0] : $ARGV[0];
  die "pdlpp_mkgen: unspecified directory" unless defined $dir && -d $dir;
  my $file = "$dir/MANIFEST";
  die "pdlpp_mkgen: non-existing '$file\'" unless -f $file;
  my @pairs = ();
  my $manifest = ExtUtils::Manifest::maniread($file);
  for (grep !/^(t|xt)\// && /\.pd$/ && -f, sort keys %$manifest) {
    my $content = do { local $/; open my $in, '<', $_; <$in> };
    warn("pdlpp_mkgen: unknown module name for '$_' (use proper '=head1 NAME' section)\n"), next
      if !(my ($name) = $content =~ /=head1\s+NAME\s+(\S+)\s+/sg);
    push @pairs, [$_, $name];
  }
  my %added = ();
  my @in = map "-I".File::Spec::Functions::rel2abs($_), @INC;
  for (@pairs) {
    my ($pd, $mod) = @$_;
    (my $prefix = $mod) =~ s|::|/|g;
    my $outfile = File::Spec::Functions::rel2abs("$dir/GENERATED/$prefix.pm");
    File::Path::mkpath(dirname($outfile));
    my $old_cwd = Cwd::cwd();
    my $maybe_lib_base = "lib/$prefix";
    my $maybe_lib_path = "$maybe_lib_base.pd";
    my $is_lib_path = substr($pd, -length $maybe_lib_path) eq $maybe_lib_path;
    my $todir = $is_lib_path ? substr($pd, 0, -length($maybe_lib_path)-1) : dirname($pd);
    chdir $todir if $todir;
    my $basename = $is_lib_path ? $maybe_lib_base : (split '/', $prefix)[-1];
    my $pp_call_arg = _pp_call_arg($mod, $mod, $basename, '', 0); # 0 so guarantee not create pp-*.c
    #there is no way to use PDL::PP from perl code, thus calling via system()
    my $rv = system $^X, @in, $pp_call_arg, $is_lib_path ? "$basename.pd" : basename($pd);
    my $basefile = "$basename.pm";
    die "pdlpp_mkgen: cannot convert '$pd'\n" unless $rv == 0 && -f $basefile;
    File::Copy::copy($basefile, $outfile) or die "$outfile: $!";
    unlink $basefile; # Transform::Proj4.pm is wrong without GIS::Proj built
    unlink "$basename.xs"; # since may have been recreated wrong
    chdir $old_cwd or die "chdir $old_cwd: $!";
    $added{"GENERATED/$prefix.pm"} = "mod=$mod pd=$pd (added by pdlpp_mkgen)";
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
    trylink 'libwhatever', '', $body, $libs, $cflags,
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
On by default but overridable with environment variable C<HIDE_TRYLINK> if set.

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
  require File::Temp;
  # check if MakeMaker should be used to preprocess the libs
  for my $key(keys %$opt) {$opt->{lc $key} = $opt->{$key}}
  my $mmprocess = exists $opt->{makemaker} && $opt->{makemaker};
  my $hide = $opt->{hide} // $ENV{HIDE_TRYLINK} // 1;
  my $clean = exists $opt->{clean} ? $opt->{clean} : 1;
  if ($mmprocess) {
      require ExtUtils::MakeMaker;
      require ExtUtils::Liblist;
      my $self = ExtUtils::MakeMaker->new({DIR =>  [],'NAME' => 'NONE'});
      my @libs = $self->ext($libs, 0);
      print "processed LIBS: $libs[0]\n" unless $hide;
      $libs = $libs[0]; # replace by preprocessed libs
  }
  print "     Trying $txt...\n     " if $txt =~ /\S/;
  my $HIDE = !$hide ? '' : '>/dev/null 2>&1';
  if($^O =~ /mswin32/i) {$HIDE = '>NUL 2>&1'}
  my $tempd = File::Temp::tempdir(CLEANUP=>1) || die "trylink: could not make temp dir";
  my ($tc,$te) = map catfile($tempd,"testfile$_"), ('.c','');
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
  my $cmd = "$Config{cc} $cflags -o $te $tc $libs $HIDE";
  print "$cmd ...\n" unless $hide;
  my $success = (system($cmd) == 0) && -e $te ? 1 : 0;
  unlink $te, $tc if $clean;
  print $success ? "\t\tYES\n" : "\t\tNO\n" unless $txt =~ /^\s*$/;
  print $success ? "\t\tSUCCESS\n" : "\t\tFAILED\n"
    if $txt =~ /^\s*$/ && !$hide;
  return $success;
}

sub get_maths_libs {
  return '' if $^O =~ /MSWin/;
  return getcyglib('m') if $^O =~ /cygwin/;
  return '-lm' if !($^O eq 'solaris' or $^O eq 'sunos');
  my $libs = '-lm';
  # try to guess where sunmath is
  my @d = split /:+/, $ENV{LD_LIBRARY_PATH};
  my $ok = 0;
  for my $d (@d) {
    if (-e "$d/libsunmath.so" or -e "$d/libsunmath.a" ) {
      $libs = "-lsunmath $libs";
      $ok = 1;
      last;
    }
  }
  return $libs if $ok;
  print "libsunmath not found in LD_LIBRARY_PATH: looking elsewhere\n";
  # get root directory of compiler; may be off of there
  require File::Which;
  my @dirs = map dirname($_).'/lib', grep defined, scalar File::Which::which($Config{cc});
  push @dirs, '/opt/SUNWspro/lib'; # default location if all else fails
  for my $d ( @dirs ) {
    if (-e "$d/libsunmath.so") {
      $libs = "-R$d -L$d -lsunmath $libs";
      $ok = 1;
      last;
    }
    if (-e "$d/libsunmath.a") {
      $libs = "-L$d -lsunmath $libs";
      $ok = 1;
      last;
    }
  }
  print <<'EOF' if !$ok;
Couldn't find sunmath library in standard places
If you can find libsunmath.a or libsunmath.so
please let us know at pdl-devel@lists.sourceforge.net
EOF
  $libs;
}

=head2 generate_core_flags

=for ref

prints on C<STDOUT> XS text with core flags, for F<Core.xs>.

=cut

my %flags = (
    hdrcpy => { set => 1 },
    fflows => { FLAG => "DATAFLOW_F" },
    is_readonly => { FLAG => "READONLY" },
    is_inplace => { FLAG => "INPLACE", postset => 1 },
    set_inplace => { FLAG => "INPLACE", noret => 1 },
    donttouch => { FLAG => "DONTTOUCHDATA" },
    allocated => { },
    vaffine => { FLAG => "OPT_VAFFTRANSOK" },
    anychgd => { FLAG => "ANYCHANGED" },
    datachgd => { FLAG => "PARENTDATACHANGED" },
    dimschgd => { FLAG => "PARENTDIMSCHANGED" },
);

sub generate_core_flags {
    # access (read, if set is true then write as well; if postset true then
    #         read first and write new value after that)
    # to ndarray's state
    foreach my $name ( sort keys %flags ) {
        my $flag = "PDL_" . ($flags{$name}{FLAG} || uc($name));
        my $ref = $flags{$name};
        my $with_mode = grep $ref->{$_}, qw(set postset noret);
        my $mode_dflt = (grep $ref->{$_}, qw(set postset)) ? "=0" : "";
        my @mode = $with_mode ? (",mode$mode_dflt", "\n        int mode") : ('', '');
        printf <<'EOF', $ref->{noret} ? 'void' : 'int', $name, @mode;
%s
%s(x%s)
        pdl *x%s
        CODE:
EOF
        my $cond = $ref->{noret} ? "" : "if (items>1) ";
        my $set = "        ${cond}setflag(x->state,$flag,mode);\n";
        my $ret = "        RETVAL = ((x->state & $flag) > 0);\n";
        print $set if $ref->{set} || $ref->{noret};
        print $ret if !$ref->{noret};
        print $set if $ref->{postset};
        print "        OUTPUT:\n        RETVAL\n" if !$ref->{noret};
        print "\n";
    } # foreach: keys %flags
}

=head2 got_complex_version

=for ref

  PDL::Core::Dev::got_complex_version($func_name, $num_params)

For a given function appearing in C99's C<complex.h>, will return a
boolean of whether the system being compiled on has the complex version
of that. E.g. for C<sin>, will test whether C<csinl> exists (before 2.069,
would only check for C<csin>, causing build failures on non-C99 compliant
C<libc> which mandates long-double versions).

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
        function => sprintf('double num; num = creal(c%sl(%s)); return 0;', $name, $args),
    );
}

1;
