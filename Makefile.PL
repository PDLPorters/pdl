# Build the whole PDL distribtuion

use 5.006_002;      # explicitly require 5.6.2 or above

$::PP_VERBOSE = 0; # =1 makes PP waffle a lot

use Config;
use IO::File;

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

    # check for red hat 5.8.0 problem described at
    # http://bugzilla.redhat.com/bugzilla/show_bug.cgi?id=87682
    if ($^V eq v5.8.0 &&
	$Config{config_args} =~ m/-Dcf_by=Red Hat/) {
	$redhat580problem = 1;
    }

    my($dot);
    for(@INC) {$dot += $_ eq '.'}
    $dot-- if(@INC[-1] eq '.');
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
  require './perldl.conf';

  # Add BAD_VAL tests if CPAN testers
  #
  if ($ENV{AUTOMATED_TESTING} == 1) {
     # warn "Makefile.PL: Automated testing environment: enabling USE_POGL config option\n";
     # $PDL_CONFIG{USE_POGL} = 1;
  }

  # Save standard values
  #
  %PDL_CONFIG_DIST = %PDL_CONFIG;

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

  require File::Spec;
  
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
      $PDL_CONFIG{MALLOCDBG} = ();
  }
  foreach my $item ( qw( libs include ) ) {
      $PDL_Config{MALLOCDBG}->{$item} = ""
	unless exists $PDLConfig{MALLOCDBG}->{$item};
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
      $val =~ s/^y.*/1/i;
      $val =~ s/^n.*/0/i;
      $PDL::Config{$key} = $val;
  }

  # destroy PDL_CONFIG/PDL_CONFIG_DIST so that we can catch any accesses
  # to them in other Makefile.PL's
  #
  %PDL_CONFIG = undef;
  %PDL_CONFIG_DIST = undef;
}

sub check_f77conf {
  my ($seen_f77conf) = @_;
  return 0 unless $seen_f77conf;

  eval 'require File::Spec';
  unless ($@ eq "") {
      print STDERR "can't load File::Spec, skipping f77conf\n";
      # skip if we don't have File::Spec
      return 0;
  }
  $pdl_f77conf = File::Spec->rel2abs($pdl_f77conf)
    unless File::Spec->file_name_is_absolute($pdl_f77conf);
  $PDL::Config{F77CONF} = $pdl_f77conf;
  return 1;
}

sub make_Version_pm {
  # Get Version from Basic/PDL.pm and generated Basic/Core/Version.pm from it
  require 'Basic/PDL.pm';

  my $versionFile = 'Basic/Core/Version.pm';
  my $fh = IO::File->new( ">$versionFile" ) or 
    die("Can't Open '$versionFile' for Writing!\n");
  print $fh <<"EOVF";

package PDL::Version;

#  This File was autogenerated by MakeFile.PL from the version
#  number in PDL.pm. It is used by PDL::Lite and others as
#  a single, consistent place to get the current PDL version.


\$VERSION='$PDL::VERSION';

1;

EOVF
  $fh->close();
}

sub make_badval_dependencies {
  # Are we using bad values or not? Are we using NaN or not?
  # NOTE:
  #  only create if there's been a change (or the file doesn't exist)
  #  since *.pd uses this as a dependency
  #
  my $badFile = "Basic/Core/badsupport.p";
  my $create_badFile = 1;

  # The file '$badFile' sets up bvalflag, usenan, and bvalPerPdl
  # variables, which is why we can use them once the file has been
  # loaded via require.
  #
  if ( -e $badFile ) {
    require $badFile;
    $create_badFile = 0
      if $bvalflag == $PDL::Config{WITH_BADVAL} and
	 $usenan   == $PDL::Config{BADVAL_USENAN} and
	 $bvalPerPdl == $PDL::Config{BADVAL_PER_PDL};
  }

  if ( $create_badFile ) {
    my $fh = IO::File->new( ">$badFile" )
      or die "Can't open '$badFile' for writing.!\n";
    print $fh "# Autogenerated by top-level Makefile.PL ".(localtime)."\n";
    print $fh "\$bvalflag = $PDL::Config{WITH_BADVAL};\n";
    print $fh "\$usenan   = $PDL::Config{BADVAL_USENAN};\n";
    print $fh "\$bvalPerPdl   = $PDL::Config{BADVAL_PER_PDL};\n";
    print $fh "1;  # return true\n";
    $fh->close;
  }
}

sub make_Types_pm {
  # make sure we have Types.pm ready for prime time
  die "Types.pm.PL not found in Basic/Core"
    unless -f 'Basic/Core/Types.pm.PL';
  my $usebvals = $PDL::Config{WITH_BADVAL} ? "BADVALS=1" : "";
  system "$Config{perlpath} Basic/Core/Types.pm.PL $usebvals";
  die "error creating Basic/Core/Types.pm" unless -f 'Basic/Core/Types.pm';
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
  print STDERR "Writing Basic/Core/Config.pm\n";

  my $fh = IO::File->new( ">Basic/Core/Config.pm" )
    or die "Couldn't open Config.pm for writing";
  print $fh "
# AUTOMATICALLY GENERATED BY THE PDL TOPLEVEL Makefile.PL.
# DO NOT HAND-EDIT - CHANGES WILL BE LOST UPON YOUR NEXT
#  'perl Makefile.PL'!!!
package PDL;
use File::Spec;
\%PDL::Config = (\n";
  for(keys %PDL::Config) {
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

##############################
##############################
#
# START: 
# the actual script begins here
#
##############################
##############################

BEGIN{
  # Version test.
  # (See also warning note in the END block)

  eval "use 5.6.0";
  die "\nPDL requires Perl v5.6.0 or later\n\n" if $@ ne "";
  
  # test for critical modules
  @hasnt = ();
  my @test = (['Filter::Util::Call','Filter'],
	      ['Text::Balanced','Text::Balanced'],
	     );
  for my $mod (@test) {
    eval "use $mod->[0]";
    push @hasnt, $mod->[1] if $@;
  }
} # end BEGIN

$seen_pdlconf = 0;

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

# Add check for POGL if USE_POGL is enabled
if (!defined($PDL::Config{USE_POGL}) or +$PDL::Config{USE_POGL}) {
   eval "use OpenGL $PDL::Config{POGL_VERSION} qw();";
   if ($@) {
      if (defined($PDL::Config{USE_POGL})) {
         warn "Makefile.PL: DEPENDENCY ERROR: USE_POGL requires at least OpenGL version $PDL::Config{POGL_VERSION}!\n";
         exit 0;
      } else {
         warn "Makefile.PL: acceptable OpenGL not found, setting \$PDL::Config{USE_POGL} => 0\n";
         $PDL::Config{USE_POGL} = 0;
      }
   } else {
      # Have OpenGL so set USE_POGL option if needed
      warn "Makefile.PL: Found required OpenGL version, setting USE_POGL => 1\n" unless defined($PDL::Config{USE_POGL});
      $PDL::Config{USE_POGL} ||= 1;
   }
} else {
   warn "Makefile.PL: Have \$PDL::Config{USE_POGL} => 0 so skipping TriD build with POGL\n";
   warn "Makefile.PL: Legacy TriD build still possible if WITH_3D is set or available...\n";
}

make_Version_pm();

make_badval_dependencies();

make_Types_pm();

use ExtUtils::MakeMaker;

# only perform one test if required modules are missing
# the test will print an informational message and fail
my %notestsifmodulesmissing = @hasnt ? # are any required modules missing ?
  (test => {TESTS => 't/requiredmods.t'}) : ();

my @podpms = map { $_.".pod", '$(INST_LIBDIR)/PDL/' . $_ .".pod"} 
  qw/perldl pdldoc/;

@prereq = (
	   'Astro::FITS::Header' => 0,
           'Data::Dumper'        => 2.121,     # for PDL::IO::Dumper
	   'File::Spec'          => 0.6,
	   'Filter::Util::Call'  => 0,         # for PDL::NiceSlice
	   'Inline'              => 0.43,
           # 'OpenGL'              => 0.58004,   # for PDL::Graphics::TriD
           'OpenGL'              => $PDL::Config{POGL_VERSION},
           'Storable'            => 1.03,      # for PDL::IO::Storable
	   'Text::Balanced'      => 0,         # for PDL::NiceSlice
	  );
# push @prereq, ('ExtUtils::F77' => 1.10) unless $seen_f77conf;

my @exe_files = ('perldl', 'pdldoc');
my $cleanup = 'perldl pdldoc pdldoc.db pdldoc.pod perldl.pod ';

if($^O !~ /mswin32/i) {
      $cleanup = 'pdl.c ' . $cleanup;
}


##############################
# Hack to include fPIC on x86_64 systems - 
# use similar mods to affect CCFLAGS on other systems as needed...
# 

my $ccflags =  $Config{ccflags};
if($Config{archname}=~m/x86_64/) {
    $ccflags .= " -fPIC";
}

%makefile_hash = (
	      'PREREQ_PM' => { @prereq },
	      'NAME' => 'PDL',
	      'VERSION_FROM' => 'Basic/Core/Version.pm',
	      'EXE_FILES' => \@exe_files,
	      'PM' => { @podpms }, #so that the script docs are picked up
              'MAN1PODS' => { 'perldl' => '$(INST_MAN1DIR)/perldl.$(MAN1EXT)', 
                              'pdldoc' => '$(INST_MAN1DIR)/pdldoc.$(MAN1EXT)'},
	      'MAN3PODS' => {}, # don't pick up the script pods again
	      'OPTIMIZE'  => $PDL::Config{OPTIMIZE} || $Config{optimize},
	      'CCFLAGS' => $ccflags,
	      'linkext'  => {LINKTYPE => ''},  # No linking required
                                               # in this directory
	      'dist'     => { COMPRESS => 'gzip', SUFFIX => 'gz'},
	      'clean' => {
		  'FILES' => $cleanup .
		      'tbyte.tif tmp0 tmp0.hdr tushort.tif ' .
		      'MANIFEST.bak tmp1* tmpraw* t/tmpraw* t/tmp1* ' .
                      '_Inline/'
		      },
	      'realclean' => {'FILES' => 'Basic/Core/Config.pm'},
	      ($] ge '5.005') ? (
				 'AUTHOR' => 'PerlDL Developers (perldl@jach.hawaii.edu)',
				 'ABSTRACT' => 'Perl Data Language',
				 'BINARY_LOCATION' => 'PDL.tar.gz',
				 ) : (),

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
  my  $text =               
'
doctest ::
	@echo "doctest: Building PDL documentation database in blib ..."
	@$(PERL) -I$(INST_ARCHLIB) -I$(INST_LIB) \
		Doc/scantree.pl 
%HTML%	@echo "doctest: Building PDL documentation web pages in blib ..."
%HTML%	@$(PERL)  -I$(INST_ARCHLIB) -I$(INST_LIB) \
%HTML%		Doc/mkhtmldoc.pl 

doc_site_install ::
	@echo "doc_site_install: Building PDL documentation database ..."
	@$(PERL) -Mblib Doc/scantree.pl $(INSTALLSITEARCH)
%HTML%	@echo "doc_site_install: Building PDL documentation web pages ..."
%HTML%	@$(PERL) Doc/mkhtmldoc.pl $(INSTALLSITEARCH)/PDL

doc_perl_install ::
	@echo "doc_perl_install: Building PDL documentation database ..."
	@$(PERL) -Mblib Doc/scantree.pl $(INSTALLARCHLIB)
%HTML%	@echo "doc_perl_install: Building PDL documentation web pages ..."
%HTML%	@$(PERL) Doc/mkhtmldoc.pl $(INSTALLARCHLIB)/PDL

';
  
  if(defined $PDL::Config{HTML_DOCS} && !$PDL::Config{HTML_DOCS}){
    $text=~ s/\%HTML\%[^\n]*\n//og; # Remove %HTML% lines
  } else {
    $text=~ s/\%HTML\%//og; # Remove just %HTML% markers
  }
    

$text .= $^O !~ /MSWin32/i ?
"
perldl.pod : perldl subdirs
\tPERL5OPT='' podselect perldl > perldl.pod

pdldoc.pod : pdldoc subdirs
\tPERL5OPT='' podselect pdldoc > pdldoc.pod

"
:
"
perldl.pod : perldl subdirs
\tpodselect perldl > perldl.pod

pdldoc.pod : pdldoc subdirs
\tpodselect pdldoc > pdldoc.pod

";

$text .= << 'EOT' if $^O =~ /cygwin/;

pdl.exe: pdl

EOT

$text .= << 'EOT' if $^O =~ /MSWin/;

DISTWIN32NAME=$(DISTVNAME)-win32

ppm: doctest ppd
	$(MV) blib/lib/PDL/HtmlDocs/PDL blib/html/lib/PDL
	$(COMPRESS) -dc win32/pbmwin32.tar.gz | $(TAR) xf -
	$(MKPATH) $(DISTWIN32NAME)
	$(CP) win32/Readme $(DISTWIN32NAME)
	$(CP) win32/install.ppm .
	$(PERL) -pe "s|</IMPLEMENTATION>|<INSTALL EXEC=\"perl\">install.ppm</INSTALL></IMPLEMENTATION>|" PDL.ppd > PDL.ppd.new
	$(RM) PDL.ppd
	$(MV) PDL.ppd.new PDL.ppd
	$(CP) PDL.ppd $(DISTWIN32NAME)
	$(TAR) cf $(DISTWIN32NAME)/PDL.tar blib install.ppm
	cd $(DISTWIN32NAME)
	$(COMPRESS) PDL.tar
	$(ZIP) $(DISTWIN32NAME).zip *
	$(MV) $(DISTWIN32NAME).zip ..
	cd ..
	$(RM_RF) $(DISTWIN32NAME)
EOT

return $text

}

##############################
# processPL: generate Makefile lines for top-level components that are created by just perling a .PL file.
# This currently (28-October) includes pdl, pdldoc, and perldl.
# 
# Nomininally MakeMaker is supposed to just handle this, but seems to create circular dependencies when 
# left to its own devices. (CED 28-Oct-2008)

#EU::MM's processPL() is continually broken on Win32 ... hence:
sub MY::processPL {
    ### This fix seems necessary with current versions of MM - otherwise it creates circular
    ### dependencies to "pm_to_blib" for .PL files (!) -- CED 9-July-2008
    if(1) { ##  || $^O =~ /MSWin32/i && ($Config{make} =~ /\bdmake/i || $Config{make} =~ /\bnmake/i)) {
	my($self) = shift;
	return "" unless $self->{PL_FILES};
	my(@m, $plfile);
	foreach $plfile (sort keys %{$self->{PL_FILES}}) {
	    my $list = ref($self->{PL_FILES}->{$plfile})
                         ?  $self->{PL_FILES}->{$plfile}
	                 : [$self->{PL_FILES}->{$plfile}];
	    my $target;
	    if($Config{make} =~ /\bdmake/i) {
		foreach $target (@$list) {
		    push @m, "
all :: $target
	\$(NOECHO) \$(NOOP)

$target :
	\$(PERLRUNINST) $plfile $target
";
		} # close foreach
	    }
	    else {
		foreach $target (@$list) {
		    push @m, "
all :: $target
	\$(NOECHO) \$(NOOP)

$target ::
	\$(PERLRUNINST) $plfile $target
";
		} # close foreach
	    }  
	}
	return join "", @m;
    }
    else {
	package MY;
	my $self = shift;
	return $self->SUPER::processPL;
    }
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

  if ($redhat580problem) {
    print << "EOP";

************************************************************
* IMPORTANT: You seem to be on a redhat system with        *
* a Perl 5.8.0 installation. Your Perl installation may be *
* broken and generate broken makefiles                     *
* see                                                      *
*                                                          *
* http://bugzilla.redhat.com/bugzilla/show_bug.cgi?id=87682
*                                                          *
* for details and workarounds.                             *
* In particular check the setting of the LANG environment  *
* variable:                                                *
*             current setting LANG=$ENV{LANG}              *
************************************************************

EOP
  }

    eval "use 5.8.0";
    if($@ ne "") {
      $vstring = sprintf("%vd",$^V);
    print <<"DEPRECATED-EOM"
******************************
* 
* You are using a deprecated version of perl (v$vstring); as of PDL 2.4.2, 
* perl version 5.8 or greater is recommended.  Most stuff will
* probably still work, but perl versions earlier than 5.8 are
* deprecated and may stop working in future.
* 
******************************
DEPRECATED-EOM
   } # end of deprecation case
} # end of END block
