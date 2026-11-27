use strict;
use warnings;
use Test::More;
use PDL::Exporter;

{
  package PDL::ExporterTest::Mod;
  our @ISA = qw(PDL::Exporter);
  our @EXPORT_OK = qw(func_a func_b internal_c);
  our %EXPORT_TAGS = (
    Func     => [qw(func_a func_b)],
    Internal => [qw(internal_c)],
  );
  sub func_a     { 'a' }
  sub func_b     { 'b' }
  sub internal_c { 'c' }
}

{
  package PDL::ExporterTest::DefaultUser;
  PDL::ExporterTest::Mod->import;
}

{
  package PDL::ExporterTest::EmptyUser;
  PDL::ExporterTest::Mod->import('');
}

{
  package PDL::ExporterTest::TagUser;
  PDL::ExporterTest::Mod->import(':Internal');
}

subtest 'default :Func import' => sub {
  ok(PDL::ExporterTest::DefaultUser->can('func_a'), 'func_a imported by default');
  ok(PDL::ExporterTest::DefaultUser->can('func_b'), 'func_b imported by default');
  ok(!PDL::ExporterTest::DefaultUser->can('internal_c'), 'internal_c not imported by default');
  is(PDL::ExporterTest::DefaultUser::func_a(), 'a', 'imported func_a works');
};

subtest 'empty string OO import' => sub {
  ok(!PDL::ExporterTest::EmptyUser->can('func_a'), 'func_a not imported with ""');
  ok(!PDL::ExporterTest::EmptyUser->can('func_b'), 'func_b not imported with ""');
};

subtest 'explicit tag import and Verbose flag' => sub {
  ok(PDL::ExporterTest::TagUser->can('internal_c'), 'internal_c imported via :Internal');
  ok(!PDL::ExporterTest::TagUser->can('func_a'), 'func_a not imported via :Internal');

  my $out = '';
  {
    open my $fh, '>', \$out or die "open scalar: $!";
    local *STDOUT = $fh;
    local $PDL::Exporter::Verbose = 1;
    package PDL::ExporterTest::VerboseUser;
    PDL::ExporterTest::Mod->import('func_a');
    close $fh;
  }
  like($out, qr/DBG: pkg=PDL::ExporterTest::Mod/, 'Verbose flag logs import');
};

done_testing;
