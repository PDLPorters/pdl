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

open my $fh, '>', \(my $encoded_text);
PDL::Doc::encodedb($got, $fh, 'DIRNAME');
my @splitup = split "\x00", $encoded_text;
is_deeply \@splitup, [
  'A',
  'PDL::Example',
    'PDL::Example',
    File => 'Example.pod',
    Ref => "Manual: does stuff\xC7",
  'convert_flowing',
    'PDL::Example',
    File => 'Example.pod',
    Module => 'PDL::Example',
    Ref => 'Generic datatype data-flowing conversion function',
    Usage => ' $y = convert_flowing($x, $newtype);
 $y = $x->convert_flowing($newtype);'
] or warn explain \@splitup;

open $fh, '<', \$encoded_text;
my $decode_hash = PDL::Doc::decodedb($fh, 'FILENAME');
is_deeply $decode_hash, {
  'PDL::Example' => {
    'PDL::Example' => {
      'Dbfile' => 'FILENAME',
      'File' => 'Example.pod',
      'Ref' => 'Manual: does stuff'
    }
  },
  'convert_flowing' => {
    'PDL::Example' => {
      'Dbfile' => 'FILENAME',
      'File' => 'Example.pod',
      'Module' => 'PDL::Example',
      'Ref' => 'Generic datatype data-flowing conversion function',
      'Usage' => ' $y = convert_flowing($x, $newtype);
 $y = $x->convert_flowing($newtype);'
    }
  }
} or diag explain $decode_hash;

my %into = (
  f1 => { 'PDL::M1' => { Ref => 'a func' } },
);
PDL::Doc::merge_hash(\%into, { f1 => { 'PDL::M2' => { Ref => 'another' } } });
is_deeply \%into, {
  f1 => {
    'PDL::M1' => { Ref => 'a func' },
    'PDL::M2' => { Ref => 'another' },
  },
};

done_testing;
