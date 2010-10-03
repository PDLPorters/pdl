#!/usr/bin/perl
#
use PDL::NiceSlice;

my $prefile = "";

{
   local $/;
   $prefile = <>;
}

my ($postfile) = &PDL::NiceSlice::perldlpp("PDL::NiceSlice", $prefile);

print $postfile;
