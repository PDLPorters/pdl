# This is the new Filter::Simple engine for PDL::NiceSlice
#
use Filter::Simple;

FILTER_ONLY
   all => sub { s/\r\n/\n/g if $^V lt v5.14.0 and $^O eq 'MSWin32'; },
   code_no_comments =>
      sub {
      my ($text1,$text2) = ($_,'');
      ## print STDERR "**************** Input: \n$text1\n";
      $text2 = perldlpp('PDL::NiceSlice', $text1);
      ## print STDERR "**************** Output: $text2\n";
      $_ = $text2;
   },
   all => sub { print if $PDL::NiceSlice::debug_filter };

1;
