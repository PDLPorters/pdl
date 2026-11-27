use strict;
use warnings;
use Test::More;
use PDL::Options;

subtest 'ifhref' => sub {
  my $h = { a => 1 };
  is_deeply(ifhref($h), { a => 1 }, 'returns hashref unchanged');
  is_deeply(ifhref(undef), {}, 'undef -> empty hashref');
  is_deeply(ifhref([1, 2]), {}, 'arrayref -> empty hashref');
  is_deeply(ifhref('str'), {}, 'scalar -> empty hashref');
};

subtest 'parse and iparse non-OO interface' => sub {
  my %got = parse({ LINE => 1, COLOUR => 'red', CENTER => 0 }, { COLOR => 'blue', CEN => 1 });
  is($got{LINE}, 1, 'default preserved');
  is($got{COLOUR}, 'blue', 'default synonym COLOR -> COLOUR');
  is($got{CENTER}, 1, 'minimum matching CEN -> CENTER');

  my %igot = iparse({ LineStyle => 'solid', COLOUR => 'green' }, { linestyle => 'dashed', color => 'yellow' });
  is($igot{LineStyle}, 'dashed', 'iparse case-insensitive match');
  is($igot{COLOUR}, 'yellow', 'iparse case-insensitive synonym');

  eval { parse({ a => 1 }) };
  like($@, qr/Usage:/, 'parse with wrong arg count croaks');

  eval { parse('not_a_hash', {}) };
  like($@, qr/hash/i, 'parse with non-hashref defaults croaks');
};

subtest 'OO interface: defaults, synonyms, options, flags' => sub {
  my $opt = PDL::Options->new({ Colour => 'red', LineWidth => 1, Style => 'solid' });
  isa_ok($opt, 'PDL::Options');

  $opt->synonyms({ Color => 'Colour', LW => 'LineWidth' });
  $opt->add_synonym({ C => 'Colour' });
  is(ref($opt->synonyms), 'HASH', 'synonyms returns hashref');

  $opt->casesens(0);
  is($opt->casesens, 0, 'casesens accessor');
  $opt->minmatch(1);
  is($opt->minmatch, 1, 'minmatch accessor');
  $opt->incremental(0);
  is($opt->incremental, 0, 'incremental accessor');
  $opt->full_options(1);
  is($opt->full_options, 1, 'full_options accessor');
  $opt->warnonmissing(0);
  is($opt->warnonmissing, 0, 'warnonmissing accessor');
  $opt->debug(0);
  is($opt->debug, 0, 'debug accessor');

  my $res = $opt->options({ color => 'blue', lw => 3, unknown_opt => 99 });
  is($res->{Colour}, 'blue', 'synonym + case-insensitive');
  is($res->{LineWidth}, 3, 'added synonym');
  is($res->{Style}, 'solid', 'full_options includes defaults');

  my $cur = $opt->current;
  is($cur->{Colour}, 'blue', 'current returns merged options');
  my @keys = sort $opt->curr_keys;
  is_deeply(\@keys, [qw(Colour LineWidth)], 'curr_keys returns user-supplied keys');
  is(ref($opt->curr_full), 'HASH', 'curr_full returns hashref');

  $opt->incremental(1);
  $opt->options({ Style => 'dotted' });
  is($opt->current->{Colour}, 'blue', 'incremental preserves prior options');
  is($opt->current->{Style}, 'dotted', 'incremental updates new option');

  $opt->full_options(0);
  my $partial = $opt->options({ LineWidth => 5 });
  is($partial->{LineWidth}, 5, 'full_options=0 returns updated key');

  $opt->clear_current;
  is_deeply($opt->current, {}, 'clear_current resets current options');

  my $ext = $opt->extend({ Extra => 42 });
  isa_ok($ext, 'PDL::Options');
  is($ext->defaults->{Extra}, 42, 'extend adds new default');
};

subtest 'translations and warnings' => sub {
  my $opt = PDL::Options->new({ Colour => 'red', Scale => 'linear' });
  $opt->translation({
    Colour => { red => 1, green => 2, blue => 3 },
    Scale  => { linear => 10, log => 20 },
  });
  $opt->add_translation({ Colour => { yellow => 4 } });
  $opt->autotrans(1);
  is($opt->autotrans, 1, 'autotrans accessor');
  $opt->casesenstrans(0);
  is($opt->casesenstrans, 0, 'casesenstrans accessor');
  $opt->minmatchtrans(1);
  is($opt->minmatchtrans, 1, 'minmatchtrans accessor');

  my $res = $opt->options({ Colour => 'YEL', Scale => 'LOG' });
  is($res->{Colour}, 4, 'added hash translation with case-insensitive minmatch');
  is($res->{Scale}, 20, 'second hash translation');

  my $err = '';
  {
    open my $efh, '>', \$err or die "open scalar: $!";
    local *STDOUT = $efh;
    $opt->warnonmissing(1);
    $opt->options({ NotARealOption => 1 });
    close $efh;
  }
  like($err, qr/NotARealOption is not a valid option/, 'warnonmissing prints warning on unknown option');
};

done_testing;
