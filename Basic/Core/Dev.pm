

# Stuff used in development/install environment of PDL Makefile.PL's
# - not part of PDL itself.

package PDL::Core::Dev;

use English; use Exporter; use DynaLoader;
@ISA    = qw( Exporter DynaLoader );

@EXPORT = qw(genpp %PDL_DATATYPES PDL_INCLUDE PDL_TYPEMAP
		 PDL_INST_INCLUDE PDL_INST_TYPEMAP
		 pdlpp_postamble_int pdlpp_stdargs_int
		 pdlpp_postamble pdlpp_stdargs write_dummy_make
                unsupported getcyglib
		 );

# Installation locations
# beware: whereami_any now appends the /Basic or /PDL directory as appropriate

# The INST are here still just in case we want to change something later.

sub PDL_INCLUDE { '-I'.whereami_any().'/Core' };
sub PDL_TYPEMAP { whereami_any().'/Core/typemap.pdl' };
sub PDL_INST_INCLUDE { '-I'.whereami_any().'/Core' };
sub PDL_INST_TYPEMAP { whereami_any().'/Core/typemap.pdl' };

sub PDL_INST_INCLUDE {&PDL_INCLUDE}
sub PDL_INST_TYPEMAP {&PDL_TYPEMAP}

# Data types to C types mapping
# get the map from Types.pm
{
eval('do "'.whereami_any().'/Core/Types.pm"');
if($@) {
  my $foo = $@;
  $@="";
  do('require "'.whereami_any().'/Core/Types.pm"');
  if($@) {
   die "can't find PDL::Types.pm: $foo and $@" unless $@ eq "";
  }
}
}
PDL::Types->import();

%PDL_DATATYPES = ();
foreach $key (keys %PDL::Types::typehash) {
  $PDL_DATATYPES{$PDL::Types::typehash{$key}->{'sym'}} =
    $PDL::Types::typehash{$key}->{'ctype'};
}

# non-blocking IO configuration

$O_NONBLOCK = defined $Config{'o_nonblock'} ? $Config{'o_nonblock'}
                : 'O_NONBLOCK';


#################### PDL Generic PreProcessor ####################
#
# Preprocesses *.g files to *.c files allowing 'generic'
# type code which is converted to code for each type.
#
# e.g. the code:
#
#    pdl x;
#    GENERICLOOP(x.datatype)
#       generic *xx = x.data;
#       for(i=0; i<nvals; i++)
#          xx[i] = i/nvals;
#    ENDGENERICLOOP
#
# is converted into a giant switch statement:
#
#     pdl x;
#     switch (x.datatype) {
#
#     case PDL_L:
#        {
#           long *xx = x.data;
#           for(i=0; i<nvals; i++)
#              xx[i] = i/nvals;
#        }break;
#
#     case PDL_F:
#        {
#           float *xx = x.data;
#
#       .... etc. .....
#
# 'generic' is globally substituted for each relevant data type.
#
# This is used in PDL to write generic functions (taking pdl or void
# objects) which is still efficient with perl writing the actual C
# code for each type.
#
#     1st version - Karl Glazebrook 4/Aug/1996.
#
# Also makes the followings substitutions:
#
# (i) O_NONBLOCK - open flag for non-blocking I/O (5/Aug/96)
#


sub genpp {

   $gotstart = 0; @gencode = ();

   while (<>) { # Process files in @ARGV list - result to STDOUT

      # Do the miscellaneous substitutions first

      s/O_NONBLOCK/$O_NONBLOCK/go;   # I/O

      if ( /(\s*)?\bGENERICLOOP\s*\(([^\)]*)\)(\s*;)?/ ){  # Start of generic code
         #print $MATCH, "=$1=\n";

         die "Found GENERICLOOP while searching for ENDGENERICLOOP\n" if $gotstart;
         $loopvar = $2;
         $indent = $1;
         print $PREMATCH;

         @gencode = ();  # Start saving code
         push @gencode, $POSTMATCH;
         $gotstart = 1;
         next;
      }

      if ( /\bENDGENERICLOOP(\s*;)?/ ) {

         die "Found ENDGENERICLOOP while searching for GENERICLOOP\n" unless $gotstart;

         push @gencode, $PREMATCH;

         flushgeneric();  # Output the generic code

         print $POSTMATCH;  # End of genric code
         $gotstart = 0;
         next;
      }

      if ($gotstart) {
         push @gencode, $_;
      }
      else {
         print;
      }

   } # End while
}

sub flushgeneric {  # Construct the generic code switch

   print $indent,"switch ($loopvar) {\n\n";

   for $case (keys %PDL_DATATYPES) {

     $type = $PDL_DATATYPES{$case};

     print $indent,"case $case:\n"; # Start of this case
     print $indent,"   {";

     # Now output actual code with substutions

     for  (@gencode) {
        $line = $_;

        $line =~ s/\bgeneric\b/$type/g;

        print "   ",$line;
     }

     print "}break;\n\n";  # End of this case
   }
   print $indent,"default:\n";
   print $indent,'   croak ("Not a known data type code=%d",'.$loopvar.");\n";
   print $indent,"}";

}


# Standard PDL postamble

sub postamble {

  if ($^O =~ /win32/i) {
    open FI,'>./getdev.pl' or die "couldn't open getdev.pl";
    my $location = whereami_any();
    print FI << "EOD";

  require \"$location/Core/Dev.pm\";
  PDL::Core::Dev->import();				\
  genpp();
  1;
EOD
     close FI;
     return q~

# Rules for the generic preprocessor

.SUFFIXES: .g
.g.c:
	$(PERL) -e "require './getdev.pl'" $<  > $@

.c$(OBJ_EXT):
	$(CCCMD) $(CCCDLFLAGS) -I$(PERL_INC) $(DEFINE) $*.c

     ~;
} else {

q~

# Rules for the generic preprocessor

.SUFFIXES: .g
.g.c:
	$(PERL) -e 'require "~.whereami_any().q~/Core/Dev.pm"; \
		PDL::Core::Dev->import();				\
		genpp()' $<  > $@

.g$(OBJ_EXT):
	$(PERL) -e 'require "~.whereami_any().q~/Core/Dev.pm"; \
		PDL::Core::Dev->import();				\
		genpp()' $<  > $*.c
	$(CCCMD) $(CCCDLFLAGS) -I$(PERL_INC) $(DEFINE) $*.c

~;}

}

# Return library locations

# whereami_any returns appended 'Basic' or 'PDL' dir as appropriate
sub whereami_any {
	&whereami(1) or &whereami_inst(1) or
          die "Unable to determine ANY directory path to PDL::Core::Dev module\n";
}

sub whereami {
   for $dir (@INC,qw|. .. ../.. ../../..|) {
      return ($_[0] ? $dir . '/Basic' : $dir)
	if -e "$dir/Basic/Core/Dev.pm";
   }
   die "Unable to determine UNINSTALLED directory path to PDL::Core::Dev module\n"
    if !$_[0];
}

sub whereami_inst {
   for $dir (@INC,map {$_."/blib"} qw|. .. ../.. ../../..|) {
      return ($_[0] ? $dir . '/PDL' : $dir)
	if -e "$dir/PDL/Core/Dev.pm";
   }
   die "Unable to determine INSTALLED directory path to PDL::Core::Dev module\n"
    if !$_[0];
}

# Expects list in format:
# [gtest.pd, GTest, PDL::GTest], [...]
# source,    prefix,module/package
# The idea is to support in future several packages in same dir.

# This is the function internal for PDL.
# Later on, we shall provide another for use outside PDL.
sub pdlpp_postamble_int {
	join '',map { my($src,$pref,$mod) = @$_;
	my $w = whereami_any();
	$w =~ s%/((PDL)|(Basic))$%%;  # remove the trailing subdir
qq|

$pref.pm: $src $w/Basic/Gen/pm_to_blib
	\$(PERL) -I$w/blib/lib -I$w/blib/arch \"-MPDL::PP qw/$mod $mod $pref/\" $src

$pref.xs: $pref.pm
	\$(TOUCH) \$@

$pref.c: $pref.xs

$pref\$(OBJ_EXT): $pref.c
|
	} (@_)
}


# This is the function internal for PDL.
# Later on, we shall provide another for use outside PDL.
sub pdlpp_postamble {
	join '',map { my($src,$pref,$mod) = @$_;
	my $w = whereami_any();
	$w =~ s%/((PDL)|(Basic))$%%;  # remove the trailing subdir
qq|

$pref.pm: $src
	\$(PERL) -I$w/blib/lib -I$w/blib/arch \"-MPDL::PP qw/$mod $mod $pref/\" $src

$pref.xs: $pref.pm
	\$(TOUCH) \$@

$pref.c: $pref.xs

$pref\$(OBJ_EXT): $pref.c
|
	} (@_)
}

sub pdlpp_stdargs_int {
 my($rec) = @_;
 my($src,$pref,$mod) = @$rec;
 my $w = whereami();
 return (
 	%::PDL_OPTIONS,
	 'NAME'  	=> $mod,
	 'VERSION_FROM' => "$w/Basic/Core/Version.pm",
	 'TYPEMAPS'     => [&PDL_TYPEMAP()],
	 'OBJECT'       => "$pref\$(OBJ_EXT)",
	 PM 	=> {"$pref.pm" => "\$(INST_LIBDIR)/$pref.pm"},
	 MAN3PODS => {"$pref.pm" => "\$(INST_MAN3DIR)/$mod.\$(MAN3EXT)"},
	 'INC'          => &PDL_INCLUDE(),
	 'LIBS'         => [''],
	 'clean'        => {'FILES'  => "$pref.xs $pref.pm $pref\$(OBJ_EXT) $pref.c"},
 );
}

sub pdlpp_stdargs {
 my($rec) = @_;
 my($src,$pref,$mod) = @$rec;
 return (
 	%::PDL_OPTIONS,
	 'NAME'  	=> $mod,
	 'TYPEMAPS'     => [&PDL_INST_TYPEMAP()],
	 'OBJECT'       => "$pref\$(OBJ_EXT)",
	 PM 	=> {"$pref.pm" => "\$(INST_LIBDIR)/$pref.pm"},
	 MAN3PODS => {"$pref.pm" => "\$(INST_MAN3DIR)/$mod.\$(MAN3EXT)"},
	 'INC'          => &PDL_INST_INCLUDE(),
	 'LIBS'         => [''],
	 'clean'        => {'FILES'  => "$pref.xs $pref.pm $pref\$(OBJ_EXT) $pref.c"},
 );
}


sub unsupported {
  my ($package,$os) = @_;
  "No support for $package on $os platform yet. Will skip build process";
}

sub write_dummy_make {
  my ($msg) = @_;
      open(OUT,">Makefile") or die "can't open Makefile";
      print OUT <<"EOT";
fred:
	\@echo ****
	\@echo \"$msg\"
	\@echo ****

all: fred

test: fred

clean ::
	-mv Makefile Makefile.old

realclean ::
	rm -rf Makefile Makefile.old

EOT
      close(OUT);
}

sub getcyglib {
my ($lib) = @_;
my $lp = `gcc -print-file-name=lib$lib.a`;
$lp =~ s|/[^/]+$||;
$lp =~ s|^([a-z,A-Z]):|//$1|g;
return "-L$lp -l$lib";
}

1; # Return OK



