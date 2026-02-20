use strict;
use warnings;
use Test::More;
use PDL::Doc;

my $got = PDL::Doc::scantext(<<'EOF', 'Example.pod');
=head1 NAME

PDL::Example - does stuff

=head1 FUNCTIONS

=head2 convert_flowing

=for ref

Generic datatype data-flowing conversion function

=for usage

 $y = convert_flowing($x, $newtype);
 $y = $x->convert_flowing($newtype);

C<$newtype> is a type number.
EOF
is_deeply $got, {
  'PDL::Example' => {
    'PDL::Example' => {
      'File' => 'Example.pod',
      'Ref' => 'Manual: does stuff'
    }
  },
  'convert_flowing' => {
    'PDL::Example' => {
      'File' => 'Example.pod',
      'Module' => 'PDL::Example',
      'Ref' => 'Generic datatype data-flowing conversion function',
      'Usage' => ' $y = convert_flowing($x, $newtype);
 $y = $x->convert_flowing($newtype);'
    }
  }
} or diag explain $got;

done_testing;
