use strict;
use warnings;
use Test::More;
use PDL::Doc;
use PDL::Doc::Perldl;

my $pod_text = <<'EOF';
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
my $got = PDL::Doc::scantext($pod_text, 'Example.pod');
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

my $onldc = PDL::Doc->new_from_hash($got);
my @searched = $onldc->search('manual:',['Ref'],1);
is_deeply \@searched, [
  [
    'PDL::Example',
    'PDL::Example',
    {
      'File' => 'Example.pod',
      'Ref' => 'Manual: does stuff'
    }
  ]
] or diag explain \@searched;

undef &PDL::Doc::Perldl::screen_width;
*PDL::Doc::Perldl::screen_width = sub { 72 };
my $formatted = PDL::Doc::Perldl::format_ref(@searched);
is $formatted, "PDL::Example    P::Example  Manual: does stuff\n";

open my $pod_fh, '<', \$pod_text;
open my $func_fh, '>', \(my $func_text);
PDL::Doc::getfuncdocs('convert_flowing', $pod_fh, $func_fh);
is $func_text, <<'EOF';
=head2 convert_flowing

=for ref

Generic datatype data-flowing conversion function

=for usage

 $y = convert_flowing($x, $newtype);
 $y = $x->convert_flowing($newtype);

C<$newtype> is a type number.
EOF

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
] or diag explain \@splitup;

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

my $mzeroes_text = <<'EOF';
=head1 NAME

PDL::Matrix -- a convenience matrix class for column-major access

=head1 FUNCTIONS

=head2 mzeroes, mones, msequence

=for ref

constructs a PDL::Matrix object similar to the ndarray constructors
zeroes, ones, sequence.
EOF
my $mzeroes_got = PDL::Doc::scantext($mzeroes_text, 'Matrix.pm');
is_deeply $mzeroes_got, {
  'PDL::Matrix' => {
    'PDL::Matrix' => {
      'File' => 'Matrix.pm',
      'Ref' => 'Module: a convenience matrix class for column-major access'
    }
  },
  'mones' => {
    'PDL::Matrix' => {
      'Crossref' => 'mzeroes',
      'File' => 'Matrix.pm',
      'Module' => 'PDL::Matrix'
    }
  },
  'msequence' => {
    'PDL::Matrix' => {
      'Crossref' => 'mzeroes',
      'File' => 'Matrix.pm',
      'Module' => 'PDL::Matrix'
    }
  },
  'mzeroes' => {
    'PDL::Matrix' => {
      'File' => 'Matrix.pm',
      'Module' => 'PDL::Matrix',
      'Names' => 'mzeroes,mones,msequence',
      'Ref' => 'constructs a PDL::Matrix object similar to the ndarray constructors
zeroes, ones, sequence.'
    }
  }
} or diag explain $mzeroes_got;
open $pod_fh, '<', \$mzeroes_text;
open $func_fh, '>', \$func_text;
PDL::Doc::getfuncdocs('msequence', $pod_fh, $func_fh);
is $func_text, <<'EOF';
=head2 mzeroes, mones, msequence

=for ref

constructs a PDL::Matrix object similar to the ndarray constructors
zeroes, ones, sequence.
EOF

done_testing;
