use strict;
use warnings;

# using absolute path to make it valid also for Makefile.PL files in subdirectories
use Cwd;
use lib Cwd::abs_path('inc');

use Devel::CheckLib;
use 5.010_000;
use ExtUtils::MakeMaker 6.56;  # for CONFIGURE_REQUIRES support
use Config;
use File::Spec;

$::PP_VERBOSE = $::PP_VERBOSE = 0; # =1 makes PP waffle a lot

# file globals(!)
my ($pdl_f77conf, $forcebuild);
my ($seen_pdlconf, $seen_f77conf);
my $pdl_conf_file = ''; # apparently for users to change

# build-system globals
our ($usenan, $bvalflag, $bvalPerPdl);

sub checkbuggysetup {
    # detect buggy Perl setups
    if (!$forcebuild &&
	$Config{osname} eq 'solaris' &&
	($Config{cc} =~ /gcc/ || $Config{gccversion} ne '') &&
	$Config{usemymalloc} eq 'y'
	) {
	die <<'EOM';

FATAL BUG IN YOUR PERL SETUP DETECTED. BUILD TERMINATED.

On this platform the combination of gcc and the Perl malloc
are buggy. The optimizations lead to random coredumps
and make PDL essentially unusable.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WORKAROUND: YOU MUST RECOMPILE PERL WITH 'usemymalloc=n' !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

You may override and force the build by including the
'FORCE' switch:

  perl Makefile.PL FORCE

However, you will most likely end up with an unusable
PDL build unless *all* optimizations are disabled!
YOU HAVE BEEN WARNED!!

EOM
  }

    my($dot);
    for(@INC) {$dot += $_ eq '.'}
    $dot-- if($INC[-1] eq '.');
    if($dot) {
	$INC = join(":",@INC);
	warn << "EOM"

**********************************************************************
Your Perl \@INC path is:

$INC

That seems to include the directory '.' before the last entry in the
path.  Configurations like that might be broken, because they may
prevent strict division of hierarchy in the module name space.
In particular, adding a module Foo::Bar::Baz can cause trouble
if Baz exists in the top level elsewhere, and if you run from the
Foo/Bar directory.

This happens with the PDL::Config and some other modules.  You may
not be able to build PDL with this \@INC setup.

If you strike trouble, you may need to fix your \@INC.
**********************************************************************


EOM
unless $ENV{PDL_INC_OK};
}

}

# The user specifies configuration options using the PDL_CONFIG
# array in perldl.conf - or ~/.perldl.conf or via the PDLCONF command-line
# argument.
#
# We need to use this information during the 'perl Makefile.PL' process,
# and historically this has been done by directly accessing %PDL_CONFIG
# (or %::PDL_CONFIG) variable. However, there are times that this information
# is also needed during the actual build (ie 'make' cycle), at which point
# the variable is not available. However Basic/Core/Config.pm is available,
# but this uses the %PDL::Config variable to access the data.
#
# So, we try to avoid this dichotomy by creating a %PDL::Config during
# the 'perl Makefile.PL' stage and then making sure that PDL_CONFIG
# is destroyed (so it can't be used by accident).
#
# Do we need to worry about Makefile.PL's hacking PDL_CONFIG?
# Probably...
#
%PDL::Config = ();

sub getpdl_config {
  my ($pdl_conf_file) = @_;

  # First read in distribution config file
  #
  our %PDL_CONFIG;
  require './perldl.conf';

  # Add BAD_VAL tests if CPAN testers
  #
  if ($ENV{AUTOMATED_TESTING}) {
     # warn "Makefile.PL: Automated testing environment: enabling USE_POGL config option\n";
     # $PDL_CONFIG{USE_POGL} = 1;
  }

  # Save standard values
  #
  my %PDL_CONFIG_DIST = %PDL_CONFIG;

  # Now read in the users config file if specified
  # and convert y/n to 1/0
  #
  if (-f $pdl_conf_file) {
    warn "\nINFORMATION: using file $pdl_conf_file to set configuration defaults\n\n";
    require $pdl_conf_file;
  }

  # Sanity checking of user supplied keys (look for ones not defined in dist)

  for(keys %PDL_CONFIG) {
    if(!exists($PDL_CONFIG_DIST{$_})) {
      die << "EOD";
Invalid key $_ found in user supplied $pdl_conf_file
  - this key appears to be no longer in use.
  Please review configuration options and check the comments in
  the file perldl.conf that came with this distribution
EOD
    }
  }

  # Merge in default options where not supplied in users file
  #
  for(keys %PDL_CONFIG_DIST) {
    $PDL_CONFIG{$_} = $PDL_CONFIG_DIST{$_} unless exists $PDL_CONFIG{$_};
  }

  # Set up the default TEMPDIR if it has not been set by the user
  #
  # set up the default directory we use for temporary files throughout
  # PDL. These are mainly for files created during the build of PDL itself,
  # but it can also invovle files created whilst building an external
  # module that uses PDL (e.g. the trylink() function of PDL::Dev needs
  # a temporary file) or when a module is actually being used (e.g.
  # PDL::IO::Dumper may need a temporary file).
  #
  # We have no policy about what directory to use - the following is
  # an amalgam of the different choices that have been used within PDL
  # up to (and including) v2.4.1
  #

  $PDL_CONFIG{PDL_BUILD_DIR} = File::Spec->rel2abs(File::Spec->curdir());

  $PDL_CONFIG{TEMPDIR} = $^O =~ /MSWin32/i ? 'File::Spec->tmpdir()' : File::Spec->tmpdir()
    unless exists $PDL_CONFIG{TEMPDIR} and defined $PDL_CONFIG{TEMPDIR};

  # set up the default MALLOCDBG information (if not specified by the user)
  #
  if ( exists $PDL_CONFIG{MALLOCDBG} and defined $PDL_CONFIG{MALLOCDBG} ) {
      my $val = $PDL_CONFIG{MALLOCDBG};
      my $rval = ref($val);
      die "ERROR: MALLOCDBG must be set to an associative array, not to a " .
	( $rval ? $rval : "SCALAR" ) unless $rval eq "HASH";
  } else {
      $PDL_CONFIG{MALLOCDBG} = undef; # exists but undef
  }
  foreach my $item ( qw( libs include ) ) {
      $PDL_CONFIG{MALLOCDBG}->{$item} = ""
	unless exists $PDL_CONFIG{MALLOCDBG}->{$item};
  }

  # filter out incompatible options for the 'bad' code.
  # At present we can not have the experimental per-piddle
  # code turned on (the BADVAL_PER_PDL option) and use
  # NaN's for floating-point bad values (the BADVAL_USENAN
  # option).
  #
  my $flag_bad     = $PDL_CONFIG{WITH_BADVAL} || 0;
  my $flag_per_pdl = $PDL_CONFIG{BADVAL_PER_PDL} || 0;
  my $flag_use_nan = $PDL_CONFIG{BADVAL_USENAN} || 0;
  if ( $flag_bad and $flag_per_pdl and $flag_use_nan ) {
      print "WARNING: Setting BADVAL_USENAN=0 as BADVAL_PER_PDL option is set!\n\n";
      $PDL_CONFIG{BADVAL_USENAN} = 0;
  }

  # create a PDL::Config variable matching the PDL_CONFIG structure
  # and convert yes/no answers to 1/0
  #
  for my $key ( keys %PDL_CONFIG ) {
      my $val = $PDL_CONFIG{$key};
      $val =~ s/^y.*/1/i if defined $val;
      $val =~ s/^n.*/0/i if defined $val;
      $PDL::Config{$key} = $val;
  }

  # destroy PDL_CONFIG/PDL_CONFIG_DIST so that we can catch any accesses
  # to them in other Makefile.PL's
  #
  %PDL_CONFIG = ();
}

sub check_f77conf {
  my ($seenyet) = @_;
  return 0 unless $seenyet;

  my $pdl_f77conf = File::Spec->rel2abs($pdl_f77conf)
    unless File::Spec->file_name_is_absolute($pdl_f77conf);
  $PDL::Config{F77CONF} = $pdl_f77conf;
  return 1;
}

sub make_Version_pm {
  # Get Version from Basic/PDL.pm and generated Basic/Core/Version.pm from it
  require 'Basic/PDL.pm';
  # keep version info in $PDL::Config
  $PDL::Config{PDL_BUILD_VERSION} = "$PDL::VERSION";

  my $versionFile = 'Basic/Core/Version.pm';
  open my $fh, ">", $versionFile or
    die("Can't Open '$versionFile' for Writing!\n");
  print $fh <<"EOVF";
package PDL::Version;
#  This File was autogenerated by MakeFile.PL from the version
#  number in PDL.pm. It is used by PDL::Lite and others as
#  a single, consistent place to get the current PDL version.
our \$VERSION='$PDL::VERSION';
\$VERSION = eval \$VERSION;
1;
EOVF
  $fh->close();
}

sub make_Types_pm {
  # make sure we have Types.pm ready for prime time
  die "Types.pm.PL not found in Basic/Core"
    unless -f 'Basic/Core/Types.pm.PL';
  my $usebvals = $PDL::Config{WITH_BADVAL} ? "BADVALS=1" : "";
  system( $Config{perlpath}, 'Basic/Core/Types.pm.PL', $usebvals);
  unless ( -f 'Basic/Core/Types.pm' ) {
     warn "system( $Config{perlpath}, 'Basic/Core/Types.pm.PL', $usebvals) was run...\n";
     die "error creating Basic/Core/Types.pm";
  }
}

# very simple formatter, assumes structures are *not* nested
# used by make_PDL_Config_pm
sub myformat {
  my $entry = shift;
  if (ref $entry eq 'ARRAY') {
    my $list = join ',', (map {('"'.quotemeta($_).'"')} @$entry);
    return "[$list]";
  } elsif (ref $entry eq 'HASH') {
    my $list = join ",\n", (map {('"'.quotemeta($_).'" => "'.
			       quotemeta($entry->{$_}).'"')} keys %$entry);
    $list = "\n$list\n\t\t" unless $list =~ /^\s*$/;
    return "{$list}";
  } else {
     return join '', '"',quotemeta($PDL::Config{$_}),'"';
  }
}

sub make_PDL_Config_pm {
  print "Writing Basic/Core/Config.pm\n";

  my $versionFile = "Basic/Core/Config.pm";
  open my $fh, ">", $versionFile
    or die "Couldn't open $versionFile for writing";
  print $fh "
# AUTOMATICALLY GENERATED BY THE PDL TOPLEVEL Makefile.PL.
# DO NOT HAND-EDIT - CHANGES WILL BE LOST UPON YOUR NEXT
#  'perl Makefile.PL'!!!
package PDL;
use File::Spec;
\%PDL::Config = (\n";
  for (sort keys %PDL::Config) {
    $fh->print( "\t$_\t=>\t" );
    if(defined $PDL::Config{$_}) {
      if($PDL::Config{$_} eq 'File::Spec->tmpdir()') {$fh->print( $PDL::Config{$_} )}
      else {$fh->print( myformat($PDL::Config{$_}) )}
    } else {
      $fh->print( "undef" );
    }
    $fh->print(",\n");
  }
  $fh->print( ");\n1;" );
  $fh->close();
}

# test for critical modules
my @hasnt = ();
my @test = (
    ['Filter::Util::Call','Filter'],
    ['Filter::Simple','Filter::Simple'],
    ## ['Module::Compile','Module::Compile'], # don't do this, fails (why?)
    ['Text::Balanced','Text::Balanced'],
);
## $DB::single = 1; # uncomment to have debugger stop here
for my $mod (@test) {
    eval "use $mod->[0]";
    push @hasnt, $mod->[1] if $@;
}

# Scan ARGV for config file argument
@ARGV = map {
	if(/^PDLCONF=(.*)$/) {
		$seen_pdlconf=1;
		$pdl_conf_file = $1; ();
	} elsif (/^F77CONF=(.*)$/) {
		$seen_f77conf=1;
		$pdl_f77conf=$1; ();
	} elsif (/^FORCE$/i) {
		$forcebuild=1;
		();
        } else {
		$_
	}
} @ARGV;

warn "WARNING: forcing build...\n" if $forcebuild;

checkbuggysetup(); # check for buggy Perl setups

unless ( $seen_pdlconf ) {
    my $defname = "$ENV{HOME}/.perldl.conf";
    $pdl_conf_file = $defname if -f $defname;
}

# needs to be called before any of the make_XX routines
getpdl_config($pdl_conf_file);
$seen_f77conf = check_f77conf($seen_f77conf);

# Check environment for SKIP_KNOWN_PROBLEMS
if (!defined($PDL::Config{SKIP_KNOWN_PROBLEMS}) and defined($ENV{SKIP_KNOWN_PROBLEMS})) {
   $PDL::Config{SKIP_KNOWN_PROBLEMS} = $ENV{SKIP_KNOWN_PROBLEMS};
   warn "Makefile.PL: setting SKIP_KNOWN_PROBLEMS Config from environment value: $ENV{SKIP_KNOWN_PROBLEMS}";
}
# Add check for POGL if USE_POGL is enabled
if (!defined($PDL::Config{USE_POGL}) or +$PDL::Config{USE_POGL}) {
   eval "use OpenGL $PDL::Config{POGL_VERSION} qw();";
   if ($@) {
      if (defined($PDL::Config{USE_POGL})) {
         warn "Makefile.PL: DEPENDENCY ERROR: USE_POGL requires at least OpenGL version $PDL::Config{POGL_VERSION}!\n";
         exit 0;
      } else {
         warn "Makefile.PL: OpenGL-$PDL::Config{POGL_VERSION} not found, setting \$PDL::Config{USE_POGL} => 0\n";
         $PDL::Config{USE_POGL} = 0;
      }
   } else {
      # Have OpenGL so set USE_POGL option if needed
      warn "Makefile.PL: Found required OpenGL version, setting USE_POGL => 1\n" unless defined($PDL::Config{USE_POGL});
      $PDL::Config{USE_POGL} ||= 1;
   }
} else {
   warn "Makefile.PL: Have \$PDL::Config{USE_POGL} => 0 so skipping TriD build with POGL\n";
}

make_Version_pm();

make_Types_pm();

# this will make PDL::Core::Dev's functions available
# in Makefile.PL files in all subdirectories
# IMPORTANT: it has to be after make_Types_pm()
require "Basic/Core/Dev.pm"; PDL::Core::Dev->import();

# only perform one test if required modules are missing
# the test will print an informational message and fail
my %notestsifmodulesmissing = @hasnt ? # are any required modules missing ?
  (test => {TESTS => 't/requiredmods.t'}) : ();

my @podpms = map { $_.".pod", '$(INST_LIBDIR)/PDL/' . $_ .".pod"}
  qw/Bugs perldl pdldoc/;

my @prereq = (
	   'Astro::FITS::Header' => 0,
           'Carp'                => 0,         # Need to run
           'Config'              => 0,         # 
           'Convert::UU'         => 0,         # for PDL::IO::Dumper
           'Data::Dumper'        => 2.121,     # for PDL::IO::Dumper
           'English'             => 0,
           'ExtUtils::MakeMaker' => 6.56,      # for CONFIGURE_REQUIRES support
           'ExtUtils::Manifest'  => 0,
           'Fcntl'               => 0,
           'File::Basename'      => 0,
           'File::Find'          => 0,
	   'File::Map'           => 0.57,      # test new mmap implementation
           'File::Path'          => 0,
	   'File::Spec'          => 0.6,
	   'File::Temp'          => 0,
	   'Filter::Util::Call'  => 0,         # for PDL::NiceSlice
	   'Filter::Simple'      => 0.88,      # for new PDL::NiceSlice
	   'Inline'              => 0.68,
	   'Inline::C'           => 0.62,
	   'Module::Compile'     => 0.23,      # for new PDL::NiceSlice
	   'Pod::Parser'         => 0,         # version TBD for PDL::Doc
	   'Pod::Select'         => 0,         # version TBD for PDL::Doc
           'Scalar::Util'        => 0,
           'Storable'            => 1.03,      # for PDL::IO::Storable
	   'Text::Balanced'      => 1.89,      # for PDL::NiceSlice
	  );

# add OpenGL version dependency for CPAN to follow
push @prereq, ('OpenGL' => $PDL::Config{POGL_VERSION}) if $PDL::Config{USE_POGL};

# push @prereq, ('ExtUtils::F77' => 1.10) unless $seen_f77conf;

my @exe_files = qw(perldl pdldoc);
push @exe_files, "pdl$Config{exe_ext}" if $^O !~ /MSWin/;
my @cleanup = ("pdl$Config{exe_ext}", qw(
    pdldoc.db pdldoc.pod perldl.pod Changes.new
    t/callext$(OBJ_EXT) t/callext.$(DLEXT)
));

##############################
# Hack to include fPIC on x86_64 systems -
# use similar mods to affect CCFLAGS on other systems as needed...

my $ccflags =  $Config{ccflags};
if($Config{archname}=~m/x86_64/) {
    $ccflags .= " -fPIC";
}

# create GENERATED subdir with *.pm files during 'make dist' (to make metacpan.org happy)
my $preop = '$(PERL) -I$(INST_ARCHLIB) -I$(INST_LIB) -MPDL::Core::Dev -e pdlpp_mkgen $(DISTVNAME)';

my %makefile_hash = (
	      'PREREQ_PM' => { @prereq },
              'CONFIGURE_REQUIRES' => {
                'Devel::CheckLib' => 1.01,
                'Carp'            => 1.20,    # EU::MM seems to need this not to crash
                'ExtUtils::MakeMaker' => 6.56,
                'File::Path'          => 0,
              },
              TEST_REQUIRES => {
                'CPAN::Meta' => '2.120900',
                'Test::Exception' => 0,
                'Test::Warn' => 0,  # for t/pptest.t
                'Test::Deep' => 0,
              },
              BUILD_REQUIRES => {
                'ExtUtils::MakeMaker' => 0,
                'ExtUtils::ParseXS'   => 3.01, # avoids 2.21, known broken
                'File::Path'          => 0,
              },
              'MIN_PERL_VERSION' => '5.010',
	      'NAME' => 'PDL',
	      'VERSION_FROM' => 'Basic/Core/Version.pm',
	      'EXE_FILES' => \@exe_files,
	      'PM' => { @podpms }, #so that the script docs are picked up
              'META_MERGE' => {
                 resources => {
                    homepage => 'http://pdl.perl.org/',
                    bugtracker  => 'http://sourceforge.net/p/pdl/bugs/',
                    repository  => {
                       url => 'git://git.code.sf.net/p/pdl/code',
                       type => 'git',
                       web => 'http://sourceforge.net/p/pdl/code/ci/master/tree/',
                    },
                 },
              },
              'MAN1PODS' => { 'Bugs.pod' => '$(INST_MAN1DIR)/PDL::Bugs.$(MAN1EXT)',
                              'perldl' => '$(INST_MAN1DIR)/perldl.$(MAN1EXT)',
                              'pdldoc' => '$(INST_MAN1DIR)/pdldoc.$(MAN1EXT)' },
	      'MAN3PODS' => {}, # don't pick up the script pods again
	      'OPTIMIZE'  => $PDL::Config{OPTIMIZE} || $Config{optimize},
	      'CCFLAGS' => $ccflags,
	      'dist'     => { COMPRESS => 'gzip',
                              SUFFIX   => 'gz',
                              PREOP    => $preop },
	      'clean' => {
		'FILES' => join ' ', @cleanup, qw(
                  tbyte.tif tmp0 tmp0.hdr tushort.tif
                  MANIFEST.bak tmp1* tmpraw* t/tmpraw* t/tmp1*
                  _Inline/ .inlinepdlpp/ .inlinewith/ *.xfig
                ),
              },
	      'realclean' => {'FILES' => 'Basic/Core/Config.pm'},
              'AUTHOR' => 'PerlDL Developers (pdl-general@lists.sourceforge.net)',
              'ABSTRACT' => 'Perl Data Language',
              'BINARY_LOCATION' => 'PDL.tar.gz',

	      %notestsifmodulesmissing,
    );

=begin comment

print "makefile hash is:\n";
for $k(sort keys %makefile_hash) {
    print "\t$k\t";
    $v = $makefile_hash{$k};
    unless(ref $v) {
	print $v,"\n";
    } elsif(ref $v eq 'HASH') {
	print "HASH:\n";
	for $vk(sort keys %$v) {
	    print "\t\t$vk\t$v->{$vk}\n";
	}
    } elsif(ref $v eq 'ARRAY') {
	print "ARRAY:\n";
	for $vv(@$v) {
	    print "\t\t$vv\n";
	}
    } else {print "$v\n";}

}

=end comment

=cut

WriteMakefile(%makefile_hash);

# do *after* WriteMakefile since some options
# are set by the recursively called Makefile.PLs
make_PDL_Config_pm(); # write out config to PDL::Config

# Extra build target to build the doc database
sub MY::postamble {
  my ($self) = @_;
  package MY;
  my  $text =
'
doctest ::
	$(NOECHO) $(ECHO) "doctest: Building PDL documentation database in blib ..."
	$(NOECHO) $(PERLRUNINST) Doc$(DFSEP)scantree.pl
%HTML%	$(NOECHO) $(ECHO) "doctest: Building PDL documentation web pages in blib ..."
%HTML%	$(NOECHO) $(PERLRUNINST) Doc$(DFSEP)mkhtmldoc.pl

doc_site_install ::
	$(NOECHO) $(ECHO) "doc_site_install: Building PDL documentation database ..."
	$(NOECHO) $(PERLRUNINST) Doc$(DFSEP)scantree.pl "$(INSTALLSITEARCH)"
%HTML%	$(NOECHO) $(ECHO) "doc_site_install: Building PDL documentation web pages ..."
%HTML%	$(NOECHO) $(PERL) Doc$(DFSEP)mkhtmldoc.pl "$(INSTALLSITEARCH)$(DFSEP)PDL"

doc_vendor_install ::
	$(NOECHO) $(ECHO) "doc_vendor_install: Building PDL documentation database ..."
	$(NOECHO) $(PERLRUNINST) Doc$(DFSEP)scantree.pl "$(INSTALLVENDORARCH)"
%HTML%	$(NOECHO) $(ECHO) "doc_vendor_install: Building PDL documentation web pages ..."
%HTML%	$(NOECHO) $(PERL) Doc$(DFSEP)mkhtmldoc.pl "$(INSTALLVENDORARCH)$(DFSEP)PDL"

doc_perl_install ::
	$(NOECHO) $(ECHO) "doc_perl_install: Building PDL documentation database ..."
	$(NOECHO) $(PERLRUNINST) Doc$(DFSEP)scantree.pl "$(INSTALLARCHLIB)"
%HTML%	$(NOECHO) $(ECHO) "doc_perl_install: Building PDL documentation web pages ..."
%HTML%	$(NOECHO) $(PERL) Doc$(DFSEP)mkhtmldoc.pl "$(INSTALLARCHLIB)$(DFSEP)PDL"
';

  if(defined $PDL::Config{HTML_DOCS} && !$PDL::Config{HTML_DOCS}){
    $text=~ s/\%HTML\%[^\n]*\n//og; # Remove %HTML% lines
  } else {
    $text=~ s/\%HTML\%//og; # Remove just %HTML% markers
  }

$text .= << "EOPS" ;

perldl.pod : perldl
\t\$(PERLRUN) -MPod::Select -e "podselect('perldl');" > perldl.pod

pdldoc.pod : pdldoc
\t\$(PERLRUN) -MPod::Select -e "podselect('pdldoc');" > pdldoc.pod

EOPS

# Here, `pdl.c` is used directly because some makes (e.g., on Solaris) do not
# support the `$<` variable in explicit rules
$text .= <<EOT if $^O !~ /MSWin/;
pdl$Config::Config{exe_ext} : pdl.c
	\$(CC) pdl.c -o \$\@
EOT

$text .= << 'EOT' if $^O =~ /MSWin/;

DISTWIN32NAME=$(DISTVNAME)-win32

ppm : doctest ppd
	$(MV) blib$(DFSEP)lib$(DFSEP)PDL$(DFSEP)HtmlDocs$(DFSEP)PDL blib$(DFSEP)html$(DFSEP)lib$(DFSEP)PDL
	$(COMPRESS) -dc win32$(DFSEP)pbmwin32.tar.gz | $(TAR) xf -
	$(MKPATH) $(DISTWIN32NAME)
	$(CP) win32$(DFSEP)Readme $(DISTWIN32NAME)
	$(CP) win32$(DFSEP)install.ppm .
	$(PERL) -pe "s|<\x{2f}IMPLEMENTATION>|<INSTALL EXEC=\"perl\">install.ppm<\x{2f}INSTALL><\x{2f}IMPLEMENTATION>|" PDL.ppd > PDL.ppd.new
	$(RM) PDL.ppd
	$(MV) PDL.ppd.new PDL.ppd
	$(CP) PDL.ppd $(DISTWIN32NAME)
	$(TAR) cf $(DISTWIN32NAME)$(DFSEP)PDL.tar blib install.ppm
	cd $(DISTWIN32NAME)
	$(COMPRESS) PDL.tar
	$(ZIP) $(DISTWIN32NAME).zip *
	$(MV) $(DISTWIN32NAME).zip ..
	cd ..
	$(RM_RF) $(DISTWIN32NAME)
EOT

my $basic_pmblib = $self->cd('Basic', "\$(MAKE) pm_to_blib");
my $niceslice_pmblib = $self->cd(File::Spec->catdir(qw(Basic SourceFilter)), "\$(MAKE) pm_to_blib");
# the modules in PDL::LiteF, used in t/core.t
my @buildchunks = $self->cd(File::Spec->catdir(qw(Basic Gen)), "\$(MAKE)");
# this contortion is due to an intermittent failure of "cd;make" to
# actually build the SO in blib with gmake -j4, which doesn't happen if
# give actual path. E.g. in Core it often doesn't build pdlcore.o and
# sometimes not Core.o, then just stops and indicates success
my $up_blib = File::Spec->catdir((File::Spec->updir) x 2, qw(blib arch auto PDL));
push @buildchunks,
  map $self->cd(
    File::Spec->catdir(qw(Basic), $_),
    "\$(MAKE) pm_to_blib " . File::Spec->catfile($up_blib, $_, "$_.\$(DLEXT)")
  ),
  qw(Core Ops Primitive Ufunc Slices Bad Math MatrixOps);
my $basicbuild = join "\n\t", @buildchunks;
my $coretest = join ' ', map File::Spec->catfile('t', $_), qw(core.t ops.t);
# looking forward to EUMM better supporting parallel builds in subdirs:
# Core deps on Gen Ops Primitive Ufunc Slices Bad
# Primitive deps on Math
# Math deps on MatrixOps
$text .= <<EOF;

core :
	$basic_pmblib
	$niceslice_pmblib
	$basicbuild

coretest : core
	prove -b $coretest
EOF

return $text

}

# remove pdl.c from making EUMM think this dir has XS in it
sub MY::init_dirscan {
    package MY;
    my ($self) = @_;
    $self->SUPER::init_dirscan;
    $self->{C} = [];
    $self->{O_FILES} = [];
}

##############################
# processPL: generate Makefile lines for top-level components that are created by just perling a .PL file.
#EU::MM's processPL() is continually broken on Win32 ... hence:
### This fix seems necessary with current versions of MM - otherwise it creates circular
### dependencies to "pm_to_blib" for .PL files (!) -- CED 9-July-2008
sub MY::processPL {
    package MY;
    my ($self) = @_;
    return $self->SUPER::processPL if 0; ## || $^O =~ /MSWin32/i && $Config{make} =~ /\b[dn]make/i
    return "" unless $self->{PL_FILES};
    my @m;
    my $alltarget = $ExtUtils::MakeMaker::VERSION >= 7.05_06 ? 'pure_nolink' : 'all';
    my $colon = $Config::Config{make} =~ /\bdmake/i ? ':' : '::';
    foreach my $plfile (sort keys %{$self->{PL_FILES}}) {
        my $list = ref($self->{PL_FILES}->{$plfile})
                     ?  $self->{PL_FILES}->{$plfile}
                     : [$self->{PL_FILES}->{$plfile}];
        foreach my $target (@$list) {
            push @m,
                "\n$alltarget :: $target\n\t\$(NOECHO) \$(NOOP)\n",
                "\n$target $colon\n\t\$(PERLRUNINST) $plfile $target\n";
        }
    }
    return join "", @m;
}

# warn if vital modules are missing
END {
    if (@hasnt) {
      print << 'EOP';

********************************************************
* IMPORTANT: Your installation will not work since it  *
* lacks critical modules.                              *
* ALL TESTS WILL FAIL UNLESS YOU IMMEDIATELY           *
* INSTALL THE FOLLOWING MODULES [available from CPAN]: *
*
EOP

    for (@hasnt) { print "*\t$_\n" }


    print << 'EOP';
*                                                      *
* Please install the missing module(s) and start the   *
* PDL build process again (perl Makefile.PL; ....)     *
*                                                      *
********************************************************

EOP

  }

} # end of END block
