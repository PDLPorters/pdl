

# Slatec module

use lib "."; # For included ExtUtils::F77

use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

# This mess sorts out the Fortran availability - KGB.
# Depends on ExtUtils::F77 (copy included)

BEGIN {
   $msg = ""; $forcebuild=0;
   
   if (defined $PDL_CONFIG{WITH_SLATEC} && $PDL_CONFIG{WITH_SLATEC}==0) {
      $msg = "\n   Will skip build of PDL::Slatec on this system   \n";
      goto skip;
   }
   
   if (defined $PDL_CONFIG{WITH_SLATEC} && $PDL_CONFIG{WITH_SLATEC}==1) {
      print "\n   Will forcibly try and build PDL::Slatec on this system   \n\n";
      $forcebuild=1;
   }
   
   if ($^O =~ /win32/i) {
      $msg = "\n    Win32 systems not yet supported. Will not build PDL::Slatec    \n";
      goto skip unless $forcebuild;
   }

   eval "use ExtUtils::F77";  # Might want "use ExtUtils::F77 qw(generic f2c)"
   print "(ExtUtils Version $ExtUtils::F77::VERSION)\n";
   if ($@ ne "") {
      $msg = "\n".$@."\n    ExtUtils::F77 module not found. Ought not build PDL::Slatec    \n" ;
      goto skip unless $forcebuild;
   }
   if ($ExtUtils::F77::VERSION < 1.03 ) {
      $msg = "\n   need a version of ExtUtils::F77 >= 1.03. Ought not build PDL::Slatec   \n" ;
      goto skip unless $forcebuild;
   }

   $compiler_available = ExtUtils::F77->testcompiler;

   if (!$compiler_available) {
      $msg = "\n   No f77 compiler found. Ought to skip PDL::Slatec on this system    \n";
      $PDL_CONFIG{WITH_SLATEC} = 0;
   } else {
      $PDL_CONFIG{WITH_SLATEC} = 1;
   }
        
   skip:
   
   if ($msg ne "" && $forcebuild==0) {
      warn $msg."\n";
      open(OUT,">Makefile");
      print OUT "fred:\n";
      print OUT "\t\@echo \n";
	  my $emsg = substr($msg, 1);	# Get rid of leading \n
	  $emsg =~ s/\n+$//;			# Remove final \n(s)
	  $emsg =~ s/"//g;	
	  $emsg =~ s/\n/"\n\t\@echo "/g;		# Echo other lines
      print OUT "\t\@echo \"$emsg\"\n";
      print OUT "\t\@echo \n";
      print OUT "\nall: fred\n";
      print OUT "\ntest: fred\n";
      print OUT <<EOT;

clean ::
	-mv Makefile Makefile.old

realclean ::
	rm -rf Makefile Makefile.old

EOT
      close(OUT);
      $donot = 1;
   } else {
   print "\n   Building PDL::Slatec. Turn off WITH_SLATEC if there are any problems\n\n";
   }
}

return if $donot;


@pack = (["slatec.pd",Slatec,PDL::Slatec]);
@slatecfiles = map {s/^slatec\///; s/\.f$//; $_} glob("slatec/*.f");

%hash = pdlpp_stdargs_int(@::pack);

$hash{OBJECT} .= join '', map {" slatec/$_.o "} @slatecfiles;
$hash{LIBS}[0] .= ExtUtils::F77->runtime ;
$hash{clean}{FILES} .= " f77_underscore SlatecProtos.h" .
  join '', map {" slatec/$_.o "} @slatecfiles;

# Handle multiple compilers

$f2cbased = (ExtUtils::F77->runtime =~ /-lf2c/);
$g2cbased = (ExtUtils::F77->runtime =~ /-lg2c/) unless $f2cbased;
$trail = ExtUtils::F77->trail_;

print "Generating Slatec Prototypes\n";

open(OUT, ">SlatecProtos.h");
for $f (@slatecfiles) {
 open IN, "slatec/$f.P" or die "file slatec/$f.P not found";
 print OUT '#include "g2c.h"',"\n" if $g2cbased;

 while(<IN>) {
 
    next unless /extern/; # Ignore garbage
 
    # Get rid of the function type

    s/extern\s+\S+\s/extern /; 
    
    # Get rid of the argument types (we must trust Fortran/C standard
    # correspondences).
    
    s/\(.*\)/\(\)/g;
    
    # Get rid of underscores if required
  
    s/_\(/\(/g unless $trail;
  
 print OUT $_;
 }
 close IN;
}
close(OUT);

# Create flag file according to whether or not to use
# underscores (pretty hacky)

unlink("f77_underscore") if -e "f77_underscore";
if ($trail) {
   open OUT, ">f77_underscore" or die "unable to write scratch file";
   close OUT;
}

WriteMakefile(
 %hash,
 VERSION => "0.10_0",
);

sub MY::postamble {
        $mycompiler     = ExtUtils::F77->compiler();
        $mycflags       = ExtUtils::F77->cflags();
	my $orig = pdlpp_postamble_int(@::pack);
	$orig =~ s/:\s*slatec\.pd/: slatec.pd SlatecProtos.h/;
	$orig .join "\n",map {
("

slatec/$_.o: slatec/$_.f slatec/$_.P
	$mycompiler -c -o slatec/$_.o $mycflags slatec/$_.f
" )} @slatecfiles;
 
}
