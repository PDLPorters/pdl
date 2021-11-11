#  logic adapted from 00-report-prereqs.t

use strict;
use warnings;

use Test::More;

use ExtUtils::MakeMaker;
use File::Spec;
use PDL;

my $file = 'PDL::Core';
$file =~ s{::}{/}g;
$file .= ".pm";
my ($prefix) = grep { -e File::Spec->catfile($_, $file) } @INC;

my $filename         = File::Spec->catfile($prefix, $file);
my $pdl_core_version = MM->parse_version( $filename );

is $pdl_core_version, $PDL::VERSION, 'versions match: PDL and PDL::Core';

done_testing;
