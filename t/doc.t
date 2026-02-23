use strict;
use warnings;
use Test::More;
use PDL::Doc;
use PDL::Doc::Perldl;

my $mod_text = <<'EOF';
=head1 NAME

PDL::Example - does stuff

=head1 FALSE POSITIVES

=head2 not_function

=for ref

Not a doctor.

=head1 FUNCTIONS

EOF
my $cvf_text = <<'EOF';
=head2 convert_flowing

=for ref

Generic datatype data-flowing conversion function

=for usage

 $y = convert_flowing($x, $newtype);
 $y = $x->convert_flowing($newtype);

C<$newtype> is a type number.

=for example

 $y = convert_flowing($x, $newtype);

=for options

 CONVERTER  => 'ppmtogif',   # explicitly specify pbm converter
 FLAGS      => '-interlaced -transparent 0',  # flags for converter

=for sig

 Signature: (a(n), [o]b(), [t]tmp(n))

=for bad

Handles bad data.
EOF
my $pod_text = $mod_text . $cvf_text;
my $got = PDL::Doc::scantext($pod_text, 'Example.pod');
my %cvf_hash = (
  'Bad' => 'Handles bad data.',
  'Example' => ' $y = convert_flowing($x, $newtype);',
  'File' => 'Example.pod',
  'Module' => 'PDL::Example',
  'Ref' => 'Generic datatype data-flowing conversion function',
  'Opt' => ' CONVERTER  => \'ppmtogif\',   # explicitly specify pbm converter
 FLAGS      => \'-interlaced -transparent 0\',  # flags for converter',
  'Sig' => 'a(n), [o]b(), [t]tmp(n)',
  'Usage' => ' $y = convert_flowing($x, $newtype);
 $y = $x->convert_flowing($newtype);'
);
is_deeply $got, {
  'PDL::Example' => {
    'PDL::Example' => {
      'File' => 'Example.pod',
      'Ref' => 'Manual: does stuff'
    }
  },
  'convert_flowing' => {
    'PDL::Example' => \%cvf_hash,
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
$func_text =~ s#\n+\z#\n#;
is $func_text, $cvf_text;

open my $fh, '>', \(my $encoded_text);
PDL::Doc::encodedb($got, $fh, 'DIRNAME');
my @splitup = split "\x00", $encoded_text;
is_deeply \@splitup, [
  'A',
  'PDL::Example',
    'PDL::Example',
    File => 'Example.pod',
    Ref => "Manual: does stuff".
   "\xAE\x01convert_flowing",
    'PDL::Example',
    (map +($_=>$cvf_hash{$_}), sort keys %cvf_hash),
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
      %cvf_hash,
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

my $m_mod_text = <<'EOF';
=head1 NAME

PDL::Matrix -- a convenience matrix class for column-major access

=head1 FUNCTIONS

EOF
my $mz_text = <<'EOF';
=head2 mzeroes, mones, msequence

=for ref

constructs a PDL::Matrix object similar to the ndarray constructors
zeroes, ones, sequence.

EOF
my $m_also_text = <<'EOF';
=head2 funcparen()

=for ref

Has parens in title.
EOF
my $mzeroes_text = $m_mod_text . $mz_text . $m_also_text;
my $mzeroes_got = PDL::Doc::scantext($mzeroes_text, 'Matrix.pm');
is_deeply $mzeroes_got, {
  'PDL::Matrix' => {
    'PDL::Matrix' => {
      'File' => 'Matrix.pm',
      'Ref' => 'Module: a convenience matrix class for column-major access'
    }
  },
  'funcparen' => {
    'PDL::Matrix' => {
      'File' => 'Matrix.pm',
      'Module' => 'PDL::Matrix',
      'Ref' => 'Has parens in title.'
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
is $func_text, $mz_text;
open $pod_fh, '<', \$mzeroes_text;
open $func_fh, '>', \$func_text;
PDL::Doc::getfuncdocs('funcparen', $pod_fh, $func_fh);
$func_text =~ s#\n+\z#\n#;
is $func_text, $m_also_text;

done_testing;
