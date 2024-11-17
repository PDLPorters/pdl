use strict;
use warnings;
use ExtUtils::MakeMaker;

# Files for each routine (.c assumed)
my %source = qw(
  j0 j0
  j1 j1
  jn jn
  y0 j0
  y1 j1
  yn yn
);
my @keys = sort keys %source;

my $libs = get_maths_libs();
# Test for presence of besfuncs
my $include = qq{#include "mconf.h"};
$source{$_} = 'system' for grep trylink('', $include, "$_(1.);", $libs), qw(j0 j1 y0 y1);
$source{$_} = 'system' for grep trylink('', $include, "$_(1,1.);", $libs), qw(jn yn);

my @pack = (["math.pd", qw(Math PDL::Math)]);
my %hash = pdlpp_stdargs_int(@pack);

my %seen; # Build object file list
foreach my $func (@keys) {
   my $file = $source{$func};
   next if $file eq 'system';
   die "File for function $func not found\n" if $file eq '';
   $hash{OBJECT} .= " $file\$(OBJ_EXT)" unless $seen{$file}++;
}

# Add support routines
$hash{OBJECT} .= join '', map " $_\$(OBJ_EXT)", qw(const mtherr polevl cpoly ndtri);

$hash{LIBS}[0] .= " $libs";

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
