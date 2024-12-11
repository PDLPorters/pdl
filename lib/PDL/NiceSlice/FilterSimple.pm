# This is the new Filter::Simple engine for PDL::NiceSlice
#
use Filter::Simple;
use strict;
use warnings;

{
no warnings 'redefine';
sub PDL::NiceSlice::FilterSimple::code_no_comments {
  print STDERR "***** Input: \n$_\n" if $PDL::NiceSlice::debug_filter;
  $_ = perldlpp('PDL::NiceSlice', $_);
  print STDERR "***** Output: $_\n" if $PDL::NiceSlice::debug_filter;
}
}

FILTER_ONLY
   code_no_comments => \&PDL::NiceSlice::FilterSimple::code_no_comments,
   all => sub { print STDERR "*** Final: $_\n" if $PDL::NiceSlice::debug_filter };

1;
