# -*- cperl -*-

#use strict;
use Test;

BEGIN { 
  eval 'use Storable 1.0';
  unless ($@) {
    plan tests => 6;
  } else {
    plan tests => 1;
    print "ok 1 # Skipped: Storable >= 1.0 not installed\n";
    exit;
  }
}

use Storable qw/freeze thaw/;
use PDL::LiteF;
use PDL::IO::Storable;
use PDL::Dbg;

# my ($a,$olda,$serialized);

$a = sequence(2,2);
#print "Old object value: $a";

$serialized = freeze $a;
#print "Serialized by Storable: ",$serialized,"\n";

$olda = thaw $serialized;
# $olda->showsv('oldasv');
printf "olda: %d\n",\$$olda;
$olda->dump;
printf "a: %d\n", \$$a;
$a->dump;

#print "restored value : $olda\n";

ok(sum(abs($a-$olda)),0);

$oldb = thaw $serialized;
$oldc = thaw $serialized;

$PDL::Dbg::Infostr = "%T %D %S %A";
PDL->px;

undef $a;

PDL->px;
undef $oldb;
print $oldc;

$data = {
	 key1 => 1,
	 key2 => sequence(3),
	 key3 => 'hallo',
};

$dfreeze = freeze $data;
$dthaw = thaw $dfreeze;

ok ref $dthaw eq 'HASH';

print "key2 => $dthaw->{key2}\n";

use Data::Dumper;
print Dumper $data;
print Dumper $dthaw;

ok all $data->{key2} == $dthaw->{key2};

$phash = bless {PDL => sequence 3}, 'PDL';
$pfreeze = $phash->freeze;  # try as a method
ok 1;

$phthaw = thaw $pfreeze;

ok all $phthaw == $phash;
ok UNIVERSAL::isa($phthaw,'HASH');
