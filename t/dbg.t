use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Dbg;

sub capture_stdout (&) {
  my ($code) = @_;
  my $out = '';
  open my $fh, '>', \$out or die "open scalar: $!";
  local *STDOUT = $fh;
  $code->();
  close $fh;
  return $out;
}

{
  package PDL::DbgTest::Sub;
  our @ISA = qw(PDL);
}

{
  package PDL::DbgTest::Pkg;
  our $arr = PDL::Core::pdl(1, 2, 3);
  our $sub = bless PDL::Core::pdl(4, 5), 'PDL::DbgTest::Sub';
  our $not_pdl = 'hello';
}

{
  package PDL::DbgTest::Empty;
  our $scalar = 42;
}

subtest 'instance method px' => sub {
  my $x = pdl(1, 2, 3);

  {
    local $PDL::debug = 0;
    my $out = capture_stdout {
      my $ret = $x->px('ignored');
      is($ret->address, $x->address, 'px returns same ndarray when debug=0');
    };
    is($out, '', 'px prints nothing when $PDL::debug is false');
  }

  {
    local $PDL::debug = 1;
    my $out_default = capture_stdout { $x->px };
    like($out_default, qr/Double/, 'default px prints type info');

    my $out_prefix = capture_stdout { $x->px('mytag') };
    like($out_prefix, qr/^mytag\s+.*Double/, 'px with prefix prepends label');

    my $out_fmt = capture_stdout { $x->px('%C Type:%T') };
    like($out_fmt, qr/^PDL Type:Double$/, 'px with % format passes format to info');
  }
};

subtest 'class method px and vars alias' => sub {
  my $out = capture_stdout { PDL->px('PDL::DbgTest::Pkg') };
  like($out, qr/PDL variables in package PDL::DbgTest::Pkg::/, 'header printed');
  like($out, qr/\$arr\b/, 'lists package ndarray $arr');
  like($out, qr/\$sub\b.*PDL::DbgTest::Sub/, 'lists derived subclass $sub');
  unlike($out, qr/\$not_pdl\b/, 'skips non-PDL package variables');

  my $out_trailing = capture_stdout { PDL->vars('PDL::DbgTest::Pkg::') };
  like($out_trailing, qr/\$arr\b/, 'vars alias works with trailing ::');

  my $out_empty = capture_stdout { PDL->px('PDL::DbgTest::Empty') };
  like($out_empty, qr/no PDL objects in package PDL::DbgTest::Empty::/, 'reports empty package');

  my $out_caller = capture_stdout { PDL->px };
  like($out_caller, qr/PDL variables in package main::/, 'defaults to caller package');
};

done_testing;
