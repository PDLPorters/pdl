use PDL::LiteF;
use PDL::Types ':All';

use PDL::IO::FlexRaw;
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

for my $type (@types) {
  print "checking type $type...\n";
  my $pdl = sequence $type, 10;
  my $hdr = writeflex 'tmprawdata', $pdl;
  writeflexhdr('tmprawdata',$hdr);
  my $npdl = eval {readflex 'tmprawdata'};
  ok ($pdl->type == $npdl->type && 
     all $pdl == $npdl);
}

unlink qw/ tmprawdata tmprawdata.hdr /;

