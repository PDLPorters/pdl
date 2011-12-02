no warnings qw(misc);
use PDL::LiteF;
use PDL::Types ':All';

use PDL::IO::FlexRaw;
use PDL::Config;
use File::Temp;

use Test;
use strict;

# eventually this should test all our io routines with all
# supported types

# $SIG{__DIE__} = sub {print Carp::longmess(@_); die ;};
BEGIN { 
  my @ntypes = (PDL::Types::typesrtkeys());
  plan tests => scalar @ntypes;
}

our @types = map { print "making type $_\n";
		   new PDL::Type typefld($_,'numval') } typesrtkeys();

##my $data = $PDL::Config{TEMPDIR} . "/tmprawdata";
my $data = File::Temp::tmpnam();

for my $type (@types) {
  print "checking type $type...\n";
  my $pdl = sequence $type, 10;
  my $hdr = writeflex $data, $pdl;
  writeflexhdr($data,$hdr);
  my $npdl = eval {readflex $data};
  ok ($pdl->type == $npdl->type && 
     all $pdl == $npdl);
}

unlink $data, "${data}.hdr";

