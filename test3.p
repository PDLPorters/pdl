#!/usr/local/bin/perl
#
# test of FITS reading (PDL)
#

use PDL;
$PDL::verbose=1;

$a = rfits("foo.fits");

$h = $$a{Hdr}; 
for (keys %$h) {
    print $_," = ",$$h{$_},"\n";
}

print "\nsize = ",length($$a{Data}), " type = ", $$a{Datatype}, "\n";

$,=" "; print "Dims =",@{$$a{Dims}},"\n";

print "Float:  Max = ",max($a)," min = ",min($a),"\n";

$b = short $a;

print "short:  Max = ",max($b)," min = ",min($b),"\n";

imag log($a+300);

