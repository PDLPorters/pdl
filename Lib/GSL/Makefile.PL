# PDL interface to GSL RNG and randist
# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

BEGIN {


  
	$msg = ""; $forcebuild=0;
	
	if (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==0) {
	   $msg = "\n   Will skip build of PDL::GSL on this system   \n";
	   goto skip;
	}
	
	if (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==1) {
	   print "\n   Will forcibly try and build PDL::GSL on this system   \n\n";
	   $forcebuild=1;
	}
	
	if ($^O =~ /win32/i) {
	   $msg = "\n    Win32 systems not yet supported. Will not build PDL::GSL    \n";
	   goto skip unless $forcebuild;
	}

	$donot = 1;

	# Look for GSL libs:
	# default locations:
	@GSL_lib_locations = ('/lib/libgsl.a','/usr/lib/libgsl.a','/usr/local/lib/libgsl.a'); 

	# get locations from perldl.conf, if specified there:
	@GSL_lib_locations = @{$PDL_CONFIG{GSL_LIBS}} if( defined $PDL_CONFIG{GSL_LIBS} ); 

	foreach $dir (@GSL_lib_locations) {
		if (-e "$dir/libgsl.a") {
			$gsl = "$dir/libgsl.a";
			$gsldir = $dir;
			$donot = 0;
			print "${gsl} found, hope is version >= 0.4.1 for full functionality!\n";
		}
	}
	$msg =  "\n GSL Libraries not found... Skipping build of PDL::GSL.\n" if $donot;
	
skip:

	if ($msg ne "" && $forcebuild==0) {
	    warn $msg . "\n";
	    $msg =~ s/\n//g;
	    write_dummy_make( $msg );
	    $donot = 1;
	} else {
		print "\n   Building PDL::GSL. Turn off WITH_GSL if there are any problems\n\n";
	}
}

return if $donot;

@pack = (["gsl_random.pd",RNG,PDL::GSL::RNG]);
%hash = pdlpp_stdargs_int(@::pack);

# GSL includes:
# default locations:
$GSL_includes = ('-I/usr/local/include'); 

# get locations from perldl.conf, if specified there:
$GSL_includes = $PDL_CONFIG{GSL_INC} if( defined $PDL_CONFIG{GSL_INC} ); 

$hash{INC} .= ' '.$GSL_includes;
push @{$hash{LIBS}},"-lm -L$gsldir -lgsl";
WriteMakefile(%hash);


sub MY::postamble {
        pdlpp_postamble_int(@::pack);
}  # Add genpp rule                             
