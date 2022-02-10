# This is the new Filter::Simple engine for PDL::NiceSlice
#
use Filter::Simple;
use strict;
use warnings;

FILTER_ONLY
   code_no_comments =>
      sub {
      my ($text1,$text2) = ($_,'');
      print STDERR "**************** Input: \n$text1\n" if $PDL::NiceSlice::debug_filter;
      $text2 = perldlpp('PDL::NiceSlice', $text1);
      print STDERR "**************** Output: $text2\n" if $PDL::NiceSlice::debug_filter;
      $_ = $text2;
   },
   all => sub { print STDERR "*** Final: $_\n" if $PDL::NiceSlice::debug_filter };

1;
