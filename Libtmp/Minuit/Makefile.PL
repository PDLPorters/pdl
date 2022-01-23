use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;
use version;
use Text::ParseWords qw(shellwords);

# This mess sorts out the Fortran availability
# Modified from Makefile.PL from Slatec
# Depends on ExtUtils::F77

our ($f2cbased, $g2cbased);

my $donot = 0;
my $msg = ""; 
my $forcebuild = 0;
my $f77;
my @minuitfiles;

if (defined $PDL::Config{WITH_MINUIT} && $PDL::Config{WITH_MINUIT}==0) {
  $msg = "Will skip build of PDL::Minuit on this system\n";
  goto skip;
}

if (defined $PDL::Config{WITH_MINUIT} && $PDL::Config{WITH_MINUIT}==1) {
  print "Will try and build PDL::Minuit on this system\n";
  $forcebuild = 1;
}

if (exists $PDL::Config{F77CONF} && -f $PDL::Config{F77CONF}) {
  print "loading F77 configuration from $PDL::Config{F77CONF}...\n";
  eval { require $PDL::Config{F77CONF} };
  if ($@ ne "") {
    $msg = "F77CONF file not loaded: $@\nOught not build PDL::Minuit\n" ;
    goto skip unless $forcebuild;
  }
  $f77 = 'F77Conf';
} else {
  eval { require ExtUtils::F77; ExtUtils::F77->import; };  # Might want "ExtUtils::F77->import(qw(generic f2c))"
  if ($@ ne "") {
    $msg = "ExtUtils::F77 module not found. Ought not build PDL::Minuit";
    goto skip unless $forcebuild;
  } else {
    $f77 = 'ExtUtils::F77';
    if ($ExtUtils::F77::VERSION < 1.03 ) {
      $msg = "Need a version of ExtUtils::F77 >= 1.03. Ought not build PDL::Minuit\n" ;
      goto skip unless $forcebuild;
    }
  }  # end if ($@ ne "")
} # if (exists $PDL::Config{F77CONF}...

if (!$f77->testcompiler) {
  $msg = "No f77 compiler found. Ought to skip PDL::Minuit on this system";
  $PDL::Config{WITH_MINUIT} = 0;
} else {
  $PDL::Config{WITH_MINUIT} = 1;
}

skip:

if ($msg ne "" && !$forcebuild) {
  write_dummy_make( $msg );
  $PDL::Config{WITH_MINUIT} = 0;
  $donot = 1;
} else {
  $PDL::Config{WITH_MINUIT} = 1;
}

return if $donot;

my @pack = (["minuit.pd", qw(Minuit PDL::Minuit)]);

if (defined($PDL::Config{MINUIT_LIB})){
    # If libraries are specified, just need to build futils
    @minuitfiles = ("futils");
}
else{
    # Otherwise, we need to build the Minuit library as well
    @minuitfiles = ("futils", "minuit", "intracfalse");
}
   

my %hash = pdlpp_stdargs_int(@pack);

$hash{OBJECT} .= join ' ', '', 'FCN$(OBJ_EXT)', map "minuitlib/${_}\$(OBJ_EXT)", @minuitfiles;

if($Config{cc} eq 'cl') {
# Link to MinGW's libg2c.a and libgcc.a, if appropriate
  my @f = ();
  my $drive = (split /:/, `gcc -v 2>&1`)[0];
  $drive = substr($drive, -1, 1);
  for(grep /^-L/, shellwords $77->runtime) {
     $_ =~ s#^-L##;
     unless($_ =~ /:/) {$_ = $drive . ':' . $_}
     if(-e $_ . '/libg2c.a') {push @f, $_ . '/libg2c.a'}
     if(-e $_ . '/libgcc.a') {push @f, $_ . '/libgcc.a'}
  }
  $hash{LDFROM} = join ' ', $hash{OBJECT}, @f;
}

my $fortran_libs = $f77->runtime;
if ($fortran_libs =~ /quadmath.*gfortran/) { # wrong order, thanks EUMM
  my @words = shellwords $fortran_libs;
  my @other = grep !/quadmath/, @words;
  my @quadmath = grep /quadmath/, @words;
  $fortran_libs = join ' ', map /\s/ ? qq{"$_"} : $_, @other, @quadmath;
}
$hash{LIBS}[0] .= $fortran_libs;
$hash{clean}{FILES} .= join '', map {" minuitlib/$_\$(OBJ_EXT) "} @minuitfiles;

# Handle multiple compilers

$f2cbased = ($f77->runtime =~ /-lf2c/);
$g2cbased = ($f77->runtime =~ /-lg2c/) unless $f2cbased;

$hash{DEFINE} .= " -DPDL_USCORE=" . ($f77->trail_ ? "_" : "");

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
        my $mycompiler     = $f77->compiler();
        my $mycflags       = $f77->cflags();
	my $orig = pdlpp_postamble_int(@pack);
	my $hack_64bit = ($Config{archname}=~m/x86_64|i686/ ?" -fPIC " : "");
	$orig =~ s/:\s*minuit\.pd/: minuit.pd/;
	$orig .= "FFLAGS = $hack_64bit $mycflags \$(OPTIMIZE)\n";
	$orig .= join "\n",map {
	    ("minuitlib/$_\$(OBJ_EXT): minuitlib/$_.f
	$mycompiler -c \$(FFLAGS) -o minuitlib/$_\$(OBJ_EXT) minuitlib/$_.f
" )} @minuitfiles;
	if (!defined($PDL::Config{MINUIT_LIB})) {
          my $outflag = $Config{cc} eq 'cl' ? '-out:' : 'rv ';
          $orig .= "
minuitlib/libminuit\$(LIB_EXT): minuitlib/minuit\$(OBJ_EXT) minuitlib/intracfalse\$(OBJ_EXT)
	\$(AR) ${outflag}minuitlib/libminuit\$(LIB_EXT) minuitlib/minuit\$(OBJ_EXT) minuitlib/intracfalse\$(OBJ_EXT)
	\$(RANLIB) minuitlib/libminuit\$(LIB_EXT)
";
	}
	return $orig;
};

# Remove i386 option for OS X recent versions for better build, dual arch does not work anyway. KG 25/Oct/2015
my %items;
if ($Config{osname} =~ /darwin/ && version->parse($Config{osvers}) >=version->parse("14")) { # OS X Mavericks+
       print "Forcing single arch build for MINUIT\n";
       $items{LDDLFLAGS} = $Config{lddlflags};
       $items{LDDLFLAGS} =~ s/-arch i386/ /g;
}

WriteMakefile(
 %hash,
 %items
);
