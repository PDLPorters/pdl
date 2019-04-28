use strict;
use warnings;
use ExtUtils::MakeMaker;
use Config;
use version;

# This mess sorts out the Fortran availability - KGB.
# Depends on ExtUtils::F77

my $donot = 0;
my $msg = "";
my $forcebuild=0;
my ($f77);
our ($f2cbased, $g2cbased);

if (defined $PDL::Config{WITH_SLATEC} && $PDL::Config{WITH_SLATEC}==0) {
   $msg = "Will skip build of PDL::Slatec on this system";
   goto skip;
}

if (defined $PDL::Config{WITH_SLATEC} && $PDL::Config{WITH_SLATEC}==1) {
   print "Will forcibly try and build PDL::Slatec on this system\n";
   $forcebuild=1;
}

if (exists $PDL::Config{F77CONF} && -f $PDL::Config{F77CONF}) {
  print "Loading F77 configuration from $PDL::Config{F77CONF}...\n";
  eval { require $PDL::Config{F77CONF} };
  if ($@ ne "") {
    $msg = "F77CONF file not loaded: $@\nOught not build PDL::Slatec\n";
    goto skip unless $forcebuild;
  }
  $f77 = 'F77Conf';
} else {
  eval { require ExtUtils::F77; ExtUtils::F77->import; };  # Might want "ExtUtils::F77->import(qw(generic f2c))"
  if ($@ ne "") {
    $msg = "ExtUtils::F77 module not found. Ought not build PDL::Slatec" ;
    goto skip unless $forcebuild;
  } else {
    $f77 = 'ExtUtils::F77';
    print "(ExtUtils Version $ExtUtils::F77::VERSION)\n";
    if ($ExtUtils::F77::VERSION < 1.03 ) {
      $msg = "Need a version of ExtUtils::F77 >= 1.03. Ought not build PDL::Slatec" ;
      goto skip unless $forcebuild;
    }
  }  # end if ($@ ne "")
} # if (exists $PDL::Config{F77CONF}...

my $compiler_available = $f77->testcompiler;

if (!$compiler_available) {
   $msg = "No f77 compiler found. Ought to skip PDL::Slatec on this system";
   $PDL::Config{WITH_SLATEC} = 0;
} else {
   $PDL::Config{WITH_SLATEC} = 1;
}

skip:

if ($msg ne "" && $forcebuild==0) {
    write_dummy_make( $msg );
    $PDL::Config{WITH_SLATEC} = 0;
    $donot = 1;
} else {
   print "\n   Building PDL::Slatec. Turn off WITH_SLATEC if there are any problems\n\n";
   $PDL::Config{WITH_SLATEC} = 1;
}

return if $donot;

my @pack = (["slatec.pd", qw(Slatec PDL::Slatec)]);
my @slatecfiles = map {s/^slatec\///; s/\.f$//; $_} glob("slatec/*.f");
my %hash = pdlpp_stdargs_int(@pack);

$hash{OBJECT} .= join '', map {" slatec/${_}$Config{obj_ext} "} @slatecfiles;

if($Config{cc} eq 'cl') {
# Link to MinGW's libg2c.a and libgcc.a, if appropriate
# First check that ExtUtils::F77 is available
  eval{require ExtUtils::F77};
  unless($@) {
    my @f = ();

    my $drive = (split /:/, `gcc -v 2>&1`)[0];
    $drive = substr($drive, -1, 1);

    for(split ' ', ExtUtils::F77->runtime) {
       if($_ =~ /^\-L/) {
         $_ =~ s#^\-L##;
         unless($_ =~ /:/) {$_ = $drive . ':' . $_}
         if(-e $_ . '/libg2c.a') {push @f, $_ . '/libg2c.a'}
         if(-e $_ . '/libgcc.a') {push @f, $_ . '/libgcc.a'}
       }
    }
    $hash{LDFROM} = $hash{OBJECT};
    for(@f) {$hash{LDFROM} .= ' ' . $_}
  }
}

$hash{LIBS}[0] .= $f77->runtime ;
$hash{clean}{FILES} .= " SlatecProtos.h f77_underscore" .
  join '', map {" slatec/$_\$(OBJ_EXT) "} @slatecfiles;

# Handle multiple compilers

$f2cbased = ($f77->runtime =~ /-lf2c/);
$g2cbased = ($f77->runtime =~ /-lg2c/) unless $f2cbased;
my $trail = $f77->trail_;

# no longer create the prototypes here - this is now handled
# by slatec.pd. In fact, with the current method, we no
# longer need the .P files
#

# Create flag file according to whether or not to use
# underscores (pretty hacky)

unlink("f77_underscore") if -e "f77_underscore";
if ($trail) {
   open OUT, ">f77_underscore" or die "unable to write scratch file";
   close OUT;
}

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
        my $mycompiler     = $f77->compiler();
        my $mycflags       = $f77->cflags();
	my $orig = pdlpp_postamble_int(@pack);
	my $hack_64bit = ($Config{archname}=~m/x86_64/ ?" -fPIC " : "");
	$orig =~ s/:\s*slatec\.pd/: slatec.pd/;
	$orig .join "\n",map {
("

slatec/$_\$(OBJ_EXT): slatec/$_.f 
	$mycompiler -c $hack_64bit -o slatec/$_\$(OBJ_EXT) $mycflags slatec/$_.f
" )} @slatecfiles;
 
};

# Remove i386 option for OS X recent versions for better build, dual arch does not work anyway
my %items;
if ($Config{osname} =~ /darwin/ && version->parse($Config{osvers}) >= version->parse("14")) { # OS X Mavericks+
       print "Forcing single arch build for SLATEC\n";
       $items{LDDLFLAGS} = $Config{lddlflags};
       $items{LDDLFLAGS} =~ s/-arch i386/ /g;
}


WriteMakefile(
 %hash,
 VERSION => "0.12",   # This is overridden by VERSION_FROM in %hash 
 %items
);
