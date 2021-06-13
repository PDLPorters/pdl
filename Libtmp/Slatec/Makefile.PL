use strict;
use warnings;
use ExtUtils::MakeMaker;
use Config;
use version;
use Text::ParseWords qw(shellwords);

my $msg = "";
my ($f77);
our ($f2cbased, $g2cbased);

if (exists $PDL::Config{F77CONF} && -f $PDL::Config{F77CONF}) {
  print "Loading F77 configuration from $PDL::Config{F77CONF}...\n";
  eval { require $PDL::Config{F77CONF} };
  if ($@ ne "") {
    $msg = "F77CONF file not loaded: $@\nOught not build PDL::Slatec\n";
    goto skip;
  }
  $f77 = 'F77Conf';
} else {
  eval { require ExtUtils::F77; ExtUtils::F77->import; };  # Might want "ExtUtils::F77->import(qw(generic f2c))"
  if ($@ ne "") {
    $msg = "ExtUtils::F77 module not found. Ought not build PDL::Slatec\n" ;
    goto skip;
  } else {
    $f77 = 'ExtUtils::F77';
    if ($ExtUtils::F77::VERSION < 1.03 ) {
      $msg = "Need a version of ExtUtils::F77 >= 1.03. Ought not build PDL::Slatec\n" ;
      goto skip;
    }
  }  # end if ($@ ne "")
} # if (exists $PDL::Config{F77CONF}...

my $compiler_available = $f77->testcompiler;

if (!$compiler_available) {
   $msg = "No f77 compiler found. Ought to skip PDL::Slatec on this system\n";
}

skip:

if ($msg ne "") {
    write_dummy_make( $msg );
    return;
}

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

    for(shellwords ExtUtils::F77->runtime) {
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

my @fit = qw(Linfit.pm LM.pm);
@{$hash{PM}}{@fit} = map '$(INST_LIBDIR)/Fit/'.$_, @fit;
my @filter = qw(LinPred.pm);
@{$hash{PM}}{@filter} = map '$(INST_LIBDIR)/Filter/'.$_, @filter;
my @gauss = qw(Gaussian.pm);
@{$hash{PM}}{@gauss} = map '$(INST_LIBDIR)/'.$_, @gauss;
WriteMakefile(
 %hash,
 %items
);
