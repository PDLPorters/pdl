
# Minuit module

use ExtUtils::MakeMaker;
use Config;
PDL::Core::Dev->import();

# This mess sorts out the Fortran availability
# Modified from Makefile.PL from Slatec
# Depends on ExtUtils::F77

BEGIN {
   $donot = 0;
   $msg = ""; 
   $forcebuild = 0;

   if (defined $PDL::Config{WITH_MINUIT} && $PDL::Config{WITH_MINUIT}==0) {
      $msg = "\n   Will skip build of PDL::Minuit on this system   \n";
      goto skip;
   }

   if (defined $PDL::Config{WITH_MINUIT} && $PDL::Config{WITH_MINUIT}==1) {
      print "\n   Will forcibly try and build PDL::Minuit on this system   \n\n";
      $forcebuild = 1;
   }

   if (exists $PDL::Config{F77CONF} && -f $PDL::Config{F77CONF}) {
     print "loading F77 configuration from $PDL::Config{F77CONF}...\n";
     eval "require '$PDL::Config{F77CONF}'";
     if ($@ ne "") {
       $msg = "\n".$@.
	 "\n\tF77CONF file not loaded. Ought not build PDL::Minuit\n" ;
       goto skip unless $forcebuild;
     }
     $f77 = 'F77Conf';
   } else {
     eval "use ExtUtils::F77";  # Might want "use ExtUtils::F77 qw(generic f2c)"
     if ($@ ne "") {
       $msg = "\n".$@.
	 "\n\tExtUtils::F77 module not found. Ought not build PDL::Minuit\n" ;
       goto skip unless $forcebuild;
     } else {
       $f77 = 'ExtUtils::F77';
       print "(ExtUtils Version $ExtUtils::F77::VERSION)\n";
       if ($ExtUtils::F77::VERSION < 1.03 ) {
	 $msg = "\n\tneed a version of ExtUtils::F77 >= 1.03. Ought not build PDL::Minuit\n" ;
	 goto skip unless $forcebuild;
       }
     }  # end if ($@ ne "")
   } # if (exists $PDL::Config{F77CONF}...

   $compiler_available = $f77->testcompiler;

   if (!$compiler_available) {
      $msg = "\n   No f77 compiler found. Ought to skip PDL::Minuit on this system    \n";
      $PDL::Config{WITH_MINUIT} = 0;
   } else {
      $PDL::Config{WITH_MINUIT} = 1;
   }

   skip:

   if ($msg ne "" && $forcebuild==0) {
       warn $msg . "\n";
       $msg =~ s/\n//g;
       write_dummy_make( $msg );
      $PDL::Config{WITH_MINUIT} = 0;
       $donot = 1;
   } else {
      print "\n   Building PDL::Minuit. Turn off WITH_MINUIT if there are any problems\n\n";
      $PDL::Config{WITH_MINUIT} = 1;
   }
}

return if $donot;


@pack = (["minuit.pd",Minuit,PDL::Minuit]);

if (defined($PDL::Config{MINUIT_LIB})){
    # If libraries are specified, just need to build futils
    print "Using compiled CERN library: ".$PDL::Config{MINUIT_LIB}."...\n";
    @minuitfiles = ("futils");
}
else{
    # Otherwise, we need to build the Minuit library as well
    print "Stand alone Minuit library will be built...\n";
     @minuitfiles = ("futils", "minuit", "intracfalse");
 }
   

%hash = pdlpp_stdargs_int(@::pack);

$hash{OBJECT} .= join '', map {" minuitlib/${_}$Config{obj_ext} "} @minuitfiles;

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
$hash{clean}{FILES} .= join '', map {" minuitlib/$_.o "} @minuitfiles;

# Handle multiple compilers

$f2cbased = ($f77->runtime =~ /-lf2c/);
$g2cbased = ($f77->runtime =~ /-lg2c/) unless $f2cbased;

$hash{DEFINE} .= $f77->trail_ ? "" : "-DNO_TRAILING_USCORE";

### unless($^O =~ /mswin32/i) {
###   if (defined($PDL::Config{MINUIT_LIB})){
###       $hash{MYEXTLIB} .= (" ".$PDL::Config{MINUIT_LIB}." ");
###   }
###   else{
###       $hash{MYEXTLIB} .= " ./minuitlib/libminuit$Config::Config{lib_ext} ";
###       $hash{clean}{FILES} .= " ./minuitlib/libminuit$Config::Config{lib_ext} ";
###   }
### }


WriteMakefile(
 %hash,
 VERSION => "0.1",   # This is overridden by VERSION_FROM in %hash 
);

sub MY::postamble {
        $mycompiler     = $f77->compiler();
        $mycflags       = $f77->cflags();
	my $orig = pdlpp_postamble_int(@::pack);
	$hack_64bit = ($Config{archname}=~m/x86_64/ ?" -fPIC " : "");
	$orig =~ s/:\s*minuit\.pd/: minuit.pd/;
	$orig .= join "\n",map {
	    ("

minuitlib/$_\$(OBJ_EXT): minuitlib/$_.f 
	$mycompiler -c $hack_64bit -o minuitlib/$_\$(OBJ_EXT) $mycflags minuitlib/$_.f
" )} @minuitfiles;

	if (!defined($PDL::Config{MINUIT_LIB})){
          my $libbuild;
          if($Config::Config{cc} eq 'cl') {
            $libbuild = "

minuitlib/libminuit\$(LIB_EXT): minuitlib/minuit\$(OBJ_EXT) minuitlib/intracfalse\$(OBJ_EXT)
	\$(AR) -out:minuitlib/libminuit\$(LIB_EXT) minuitlib/minuit\$(OBJ_EXT) minuitlib/intracfalse\$(OBJ_EXT)
	\$(RANLIB) minuitlib/libminuit\$(LIB_EXT)
";
          }
          else {
	      $libbuild = "

minuitlib/libminuit\$(LIB_EXT): minuitlib/minuit\$(OBJ_EXT) minuitlib/intracfalse\$(OBJ_EXT)
	\$(AR) rv minuitlib/libminuit\$(LIB_EXT) minuitlib/minuit\$(OBJ_EXT) minuitlib/intracfalse\$(OBJ_EXT)
	\$(RANLIB) minuitlib/libminuit\$(LIB_EXT)
";
          }
	    $orig .= $libbuild;
	}
	return $orig;
}
