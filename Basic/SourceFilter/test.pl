use PDL::LiteF;
use PDL::SFilter;

# called for colon-less args	
# preserves parens if present	
   sub intpars { $_[0] =~ /\(.*\)/ ? '('.int($_[0]).')' : int $_[0] }
   sub PDL::mslice : lvalue {
	   my($pdl) = shift;
	   return $pdl->slice(join ',',(map {
			   $_ eq "X" ? ":" :
			   ref $_ eq "ARRAY" ? $#$_ > 1 && @$_[2] == 0 ? 
			   "(".int(@$_[0]).")" : join ':', map {int $_} @$_ :
			   !ref $_ ? intpars $_ :
			   die "INVALID SLICE DEF $_"
		   } @_));
   }
   
$a = sequence(10);

print "\n",'source $a'.'((4)) translated -> $a((4))',"\n";
print "Result ",$a((4)),"\n\n";

print 'alternative syntax: $a->'.'((4)) translated -> $a->((4))',"\n\n";

print 'source $a'.'(1:4) .= 2; translated -> $a(1:4) .= 2;',"\n"; # this should be rewritten

$a(1:4) .= 2;

print "Result: $a","\n\n";

print << 'EOP';

The arglist is split at commas but commas within
matched brackets are protected. That should allow
function invocations etc within the arglist:

EOP

print '$a'.'(1:end(0,22)) -> $a(1:end(0,22))',"\n\n";

print "recursive invocation is also supported:\n";
print '$a'.'(1,$b'.'(0:22)) -> $a(1,$b(0:22))',"\n\n";

no PDL::SFilter;


print << 'EOP';

Now we have switched off source filtering by issuing

     no PDL::SFilter;

Therefore, the next slice expression should not be touched:

EOP

print 'Source $a'.'(1:4) translation -> $a(1:4)',"\n\n"; # this shouldn't
