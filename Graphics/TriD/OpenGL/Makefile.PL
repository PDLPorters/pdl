# Makefile.PL for a package defined by PP code.

#use PDL::Core::Dev;            # Pick up development utilities
use ExtUtils::MakeMaker;

$seen_pdlconf = 0;

# Scan ARGV for config file argument
@ARGV = map {
	if(/^PDLCONF=(.*)$/) {
		$seen_pdlconf=1;
		$pdl_conf_file = $1; ();
	} else {
		$_
	}
} @ARGV;

if(!$seen_pdlconf) {
	my $defname = "$ENV{HOME}/.perldl.conf";
	if(-f $defname) {
		$pdl_conf_file = $defname;
	}
}


# First read in distribution config file 


require '../../../perldl.conf';

%PDL_CONFIG_DIST = %PDL_CONFIG; # Save standard values


# Now read in the users config file if specified

if (-f $pdl_conf_file) {
    warn "\nINFORMATION: using file $pdl_conf_file to set configuration defaults\n\n";
    require $pdl_conf_file;
}

# Sanity checking of user supplied keys (look for ones not defined in dist)

for(keys %PDL_CONFIG) {
	if(!exists($PDL_CONFIG_DIST{$_})) {
	   die("Invalid key $_ found in user supplied $pdl_conf_file - this key appears to be no longer in use. Please review configuration options");
	}
}

# Merge in default options where not supplied in users file

for(keys %PDL_CONFIG_DIST) {
   $PDL_CONFIG{$_} = $PDL_CONFIG_DIST{$_} unless exists $PDL_CONFIG{$_};
}


@pack = (["opengl.pd",OpenGL,PDL::Graphics::OpenGL]);

%hash = pdlpp_stdargs(@::pack);

push @{$hash{LIBS}}, $PDL_CONFIG{OPENGL_LIBS};
$hash{INC} .= $PDL_CONFIG{OPENGL_INC};
$hash{DEFINE} .= $PDL_CONFIG{OPENGL_DEFINE};

#use Data::Dumper;
#my $out = Dumper(\%hash);
#print $out;

WriteMakefile(%hash);

# add a dependancy for the perldl.conf file

sub MY::postamble {
  my $amb = pdlpp_postamble_int(@::pack);
  $amb =~ s/pm_to_blib$/pm_to_blib \.\.\/\.\.\/\.\.\/perldl.conf/m;
  $amb;
}  # Add genpp rule

