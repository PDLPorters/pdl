use blib;
use PDL; # this must be called before (!) 'use Inline Pdlpp' calls
use Inline Pdlpp; # the actual code is in the __Pdlpp__ block below

$a = sequence 10;
print $a->inc,"\n";
print $a->inc->dummy(1,10)->tcumul,"\n";

__DATA__

__Pdlpp__

# a rather silly increment function
pp_def('inc',
       Pars => 'i();[o] o()',
       Code => '$o() = $i() + 1;',
      );

# a cumulative product
# essentially the same functionality that is
# already implemented by cumuprodover
# in the base distribution
pp_def('tcumul',
       Pars => 'in(n); float+ [o] mul()',
       Code => '$mul() = 1;
                loop(n) %{
                  $mul() *= $in();
                %}',
);
