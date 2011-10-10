# This is the new Filter::Simple engine for PDL::NiceSlice
#
use Filter::Simple;

FILTER_ONLY
   code_no_comments =>
   # all =>
      sub {
      my ($text1,$text2) = ($_,'');
      ## print STDERR "**************** Input: \n$text1\n";
      $text2 = perldlpp('PDL::NiceSlice', $text1);
      ## print STDERR "**************** Output: $text2\n";
      $_ = $text2;
   };

1;
