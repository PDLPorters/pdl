use strict;
use warnings;
use lib 'lib';
use PDL::Doc;
use File::Spec::Functions;

my $outindex = $ARGV[0];
my @dirs = qw(lib script);
my $onldc = PDL::Doc->new;
$onldc->scantree($_) for @dirs;
write_index($onldc, $outindex);
$onldc->scantree($dirs[0]); # catch ourself now
write_index($onldc, $outindex);

sub write_index {
  my ($onldc, $outindex) = @_;
  my @mods = $onldc->search('module:',['Ref'],1);
  my @mans = $onldc->search('manual:',['Ref'],1);
  my @scripts = $onldc->search('script:',['Ref'],1);
  open my $podfh, ">", $outindex or die "couldn't open $outindex: $!";
  my $pod;
  $pod = <<'EOPOD';
  =head1 NAME

  PDL::Index - an index of PDL documentation

  =head1 DESCRIPTION

  A meta document listing the documented PDL modules and
  the PDL manual documents

  =head1 PDL manuals

EOPOD
  $pod =~ s/^  //gm;
  print $podfh $pod;
  print $podfh "=over 4\n\n";
  for (@mans) {
    my $ref = $_->[2]->{Ref};
    $ref =~ s/Manual:/L<$_->[0]> -/;
    print $podfh "=item *\n\n$ref\n\n";
  }
  $pod = <<'EOPOD';
  =back

  =head1 PDL scripts

EOPOD
  $pod =~ s/^  //gm;
  print $podfh $pod;
  print $podfh "=over 4\n\n";
  for (@scripts) {
    my $ref = $_->[2]->{Ref};
    $ref =~ s/Script:/L<$_->[0]> -/;
    print $podfh "=item *\n\n$ref\n\n";
  }
  $pod = <<'EOPOD';
  =back

  =head1 PDL modules

EOPOD
  $pod =~ s/^  //gm;
  print $podfh $pod;
  print $podfh "=over 4\n\n";
  for (@mods) {
    my $ref = $_->[2]->{Ref};
    $ref =~ s/Module:/L<$_->[0]> -/;
    print $podfh "=item *\n\n$ref\n\n";
  }

  $pod = <<EOPOD;
  =back
EOPOD
  $pod =~ s/^  //gm;
  print $podfh $pod;
  close $podfh;
}
