use blib; # we're inside the dist tree
use PDL;  # this must be called before (!) 'use Inline Pdlpp' calls

use PDL::NiceSlice; # only used to demonstrate how to switch off below
use Inline Pdlpp; # the actual code is in the __Pdlpp__ block below

$a = sequence 10;
print $a(0:4),"\n";
print $a->inc->(0:4),"\n";

# important!
no PDL::NiceSlice; # disable NiceSlice before(!) the data section

__END__

__Pdlpp__

# a silly function, really ;)
pp_def('inc',
       Pars => 'i();[o] o()',
       Code => '$o() = $i() + 1;',
      );
