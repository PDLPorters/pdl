
# Makefile.PL for PDL::Examples module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

BEGIN {

  sub decimate { # version string -> number
     $_[0] =~ /(\d+)\.(\d+)\.(\d+)/;
     return int($1*1000 + $2 * 100 + $3);
  }

  $karmabase = "/usr/local/karma";
  $karmabase = $ENV{KARMABASE} if -d $ENV{KARMABASE};
  $karmabase = $PDL_CONFIG{WHERE_KARMA} if defined $PDL_CONFIG{WHERE_KARMA};

  # Check Karma version

  my $version_ok=0;
  if (-e "$karmabase/bin/tx") {
     my $test = join("",`$karmabase/bin/tx -show_version 2>&1`);
     #print "T=",$test;

     $test=~ /running with Karma library version:\s+(\S+)\s+/;
     my $runvers = $1;
     $test=~ /compiled with library version:\s+(\S+)\s+/;
     my $compvers = $1;

     if (decimate($compvers) != decimate($runvers)) {

     	warn "Karma runtime version ($runvers) and compile time library versions ($compvers) do not match! - Sick Karma installation?";

     }
     elsif ( decimate($runvers)<1625 ) {

     	warn "Karma version is $runvers - must be at least v1.6.25 for this module\n";

     }
     else {

     	print "Congratulations you appear to have an up to date version ($runvers) of Karma!\n:";
     	$version_ok=1;

     }
  }

  if (defined ($PDL_CONFIG{WITH_KARMA})) {
     $withkarma = $PDL_CONFIG{WITH_KARMA};
  }
  else { # Build Karma if can find it
     if (-d $karmabase && $version_ok) {
        $withkarma = 1; print "\n         Seen $karmabase\n";
     }
  }

  if ($withkarma) {

     print "\n         Building Karma extension.  Turn off WITH_KARMA if there are problem\n\n";
     $PDL_CONFIG{WITH_KARMA} = 1;
  }
  else{
      my $msg = "\n      Not building PDL::Graphics::Karma, turn on WITH_KARMA if this is incorrect\n";
      warn $msg . "\n";
      $msg =~ s/\n//g;
      write_dummy_make( $msg );
      $PDL_CONFIG{WITH_KARMA} = 0;
  }


}

return if not $withkarma;


use ExtUtils::MakeMaker;
#use PDL::Core::Dev;
PDL::Core::Dev->import();

my $malloclib = exists $PDL_CONFIG{MALLOCDBG}->{libs} ?
  $PDL_CONFIG{MALLOCDBG}->{libs} : '';
my $mallocinc = exists $PDL_CONFIG{MALLOCDBG}->{include} ?
  $PDL_CONFIG{MALLOCDBG}->{include} : '';

@pack = (["karma.pd",Karma,PDL::Graphics::Karma]);

%hash = pdlpp_stdargs(@::pack);
$hash{INC}         .= " -I$karmabase/include $mallocinc";
$hash{LIBS}[0]     .=' -L'. $karmabase."/lib" .' -lkarma -lkarmagraphics -L/usr/X11R6/lib -lkarmaX11 -lX11 -lXext -lm ' . $malloclib;
# change here approriately for other platforms !
if ($^O =~ 'irix' or $^O eq 'dec_osf') { $rflag = '-rpath ' } else { $rflag = '-R' }
$hash{dynamic_lib} = { OTHERLDFLAGS => $rflag.$karmabase.'/lib'};
$hash{'dist'}      = { COMPRESS => 'gzip', SUFFIX => 'gz'};
$hash{'clean'}->{'FILES'} .= ' .tx.defaults';
WriteMakefile(%hash);

# Add genpp rule
sub MY::postamble { pdlpp_postamble_int(@::pack); }

