use strict;
use warnings;
use PDL::LiteF;
use PDL::Types ':All';
use PDL::IO::FlexRaw;
use File::Temp;
use Test::More;
use Test::PDL;

our @types = grep $_ != indx(), types();

my $data = File::Temp::tmpnam();

for my $type (@types) {
  print "checking type $type...\n";
  my $pdl = sequence $type, 10;
  my $hdr = writeflex $data, $pdl;
  writeflexhdr($data,$hdr);
  my $npdl = eval {readflex $data};
  is $pdl->type, $npdl->type;
  is_pdl $pdl, $npdl;
}

unlink $data, "${data}.hdr";

done_testing;
