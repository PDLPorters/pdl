use strict;
use warnings;
use Config;
use Test::More $Config{usedl}
    ? (tests => 5)
    : (skip_all => 'No dynaload; double-blib static build too difficult');
use File::Spec;
use IPC::Cmd qw(run);
use Cwd;
use File::Basename;
use File::Path;

my %PPTESTFILES = (
    'Makefile.PL' => <<'EOF',
use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;
my @pack = (["tests.pd", qw(Tests PDL::Tests)]);
sub MY::postamble {
	pdlpp_postamble(@pack);
};  # Add genpp rule
WriteMakefile(pdlpp_stdargs(@pack));
EOF

    'tests.pd' => <<'EOF',
# make sure the deprecation mechanism throws warnings
pp_deprecate_module( infavor => "PDL::Test::Fancy" );

sub pp_deft {
    my ($name,%hash) = @_;
##    $hash{Doc} = "=for ref\n\ninternal\n\nonly for internal testing purposes\n";
    $hash{Doc} = undef;
    $name = "test_$name";  # prepend test_ to name
    pp_def($name,%hash);
}

pp_addhdr('
/* to test the $P vaffining */
void ppcp(PDL_Byte *dst, PDL_Byte *src, int len)
{
  int i;

  for (i=0;i<len;i++)
     *dst++=*src++;
}
');

# test the $P vaffine behaviour
# when 'phys' flag is in.
pp_deft('foop',
	Pars => 'byte [phys]a1(n); byte [o,phys]b(n)',
	GenericTypes => [B],
	Code => 'ppcp($P(b),$P(a1),$SIZE(n));',
);

# float qualifier
# and also test if numerals in variable name work
pp_deft(
	'fsumover',
	Pars => 'a1(n); float [o]b();',
	Code => 'PDL_Float tmp = 0;
	 loop(n) %{ tmp += $a1(); %}
	 $b() = tmp;'
);

# test GENERIC with type+ qualifier
pp_deft(
	'nsumover',
	Pars => 'a(n); int+ [o]b();',
	Code => '$GENERIC(b) tmp = 0;
	 loop(n) %{ tmp += $a(); %}
	 $b() = tmp;'
);

# test to set named dim with 'OtherPar'
pp_deft('setdim',
	Pars => '[o] a(n)',
	OtherPars => 'int ns => n',
	Code => 'loop(n) %{ $a() = n; %}',
);

pp_deft('fooseg',
        Pars => 'a(n);  [o]b(n);',
        Code => '
	   loop(n) %{ $b() = $a(); %}
');

pp_addhdr << 'EOH';

void tinplace_c1(int n, PDL_Float* data)
{
  int i;
  for (i=0;i<n;i++) {
    data[i] = 599.0;
  }
}

void tinplace_c2(int n, PDL_Float* data1, PDL_Float* data2)
{
  int i;
  for (i=0;i<n;i++) {
    data1[i] = 599.0;
    data2[i] = 699.0;
  }
}

void tinplace_c3(int n, PDL_Float* data1, PDL_Float* data2, PDL_Float* data3)
{
  int i;
  for (i=0;i<n;i++) {
    data1[i] = 599.0;
    data2[i] = 699.0;
    data3[i] = 799.0;
  }
}

EOH

pp_deft('fooflow1',
	Pars => '[o,nc]a(n)',
        GenericTypes => ['F'],
	Code => 'tinplace_c1($SIZE(n),$P(a));',
	);

pp_deft('fooflow2',
	Pars => '[o,nc]a(n);[o,nc]b(n)',
        GenericTypes => ['F'],
	Code => 'tinplace_c2($SIZE(n),$P(a),$P(b));',
	);

pp_deft('fooflow3',
	Pars => '[o,nc]a(n);[o,nc]b(n);[o,nc]c(n)',
        GenericTypes => ['F'],
	Code => 'tinplace_c3($SIZE(n),$P(a),$P(b),$P(c));',
	);

pp_deft( 'threadloop_continue',
	 Pars => 'in(); [o] out()',
	 Code => q[
	    int cnt = 0;
	    threadloop %{

	    if ( ++cnt %2 )
	      continue;

	    $out() = $in();
	 %}
        ],
       );

pp_done;
EOF

    't/all.t' => <<'EOF',
use strict;
use warnings;
use Test::More tests => 25;
use Test::Warn;
use PDL::LiteF;
use PDL::Types;
use PDL::Dbg;

BEGIN {
  warning_like{ require PDL::Tests; PDL::Tests->import; }
    qr/deprecated.*PDL::Test::Fancy/,
    "PP deprecation should emit warnings";
}

# Is there any good reason we don't use PDL's approx function?
sub tapprox {
    my($a,$b) = @_;
    my $c = abs($a-$b);
    my $d = max($c);
    return $d < 0.01;
}

$a = xvals(zeroes(byte, 2, 4));

# $P() affine tests
test_foop($a,($b=null));
ok( tapprox($a,$b) )
  or diag $b;

test_foop($a->xchg(0,1),($b=null));
ok( tapprox($a->xchg(0,1),$b) )
  or diag $b;

my $vaff = $a->dummy(2,3)->xchg(1,2);
test_foop($vaff,($b=null));
ok( tapprox($vaff,$b) )
  or diag ($vaff, $vaff->dump);

# float qualifier
$a = ones(byte,3000);
test_fsumover($a,($b=null));
is( $b->get_datatype, $PDL_F );
is( $b->at, 3000 );

# int+ qualifier
for (byte,short,ushort,long,float,double) {
  $a = ones($_,3000);
  test_nsumover($a,($b=null));
  is( $b->get_datatype, (($PDL_L > $_->[0]) ? $PDL_L : $_->[0]) );
  is( $b->at, 3000 );
}

test_setdim(($a=null),10);
is( join(',',$a->dims), "10" );
ok( tapprox($a,sequence(10)) );

# this used to segv under solaris according to Karl
{ no warnings 'uninitialized';
  my $ny=7;
  $a = double xvals zeroes (20,$ny);
  test_fooseg $a, $b=null;

  ok( 1 );  # if we get here at all that is alright
  ok( tapprox($a,$b) )
    or diag($a, "\n", $b);
}

# test the bug alluded to in the comments in
# pdl_changed (pdlapi.c)
# used to segfault
my $xx=ones(float,3,4);
my $sl1 = $xx->slice('(0)');
my $sl11 = $sl1->slice('');
my $sl2 = $xx->slice('(1)');
my $sl22 = $sl2->slice('');

test_fooflow2($sl11, $sl22);

ok(all $xx->slice('(0)') == 599);
ok(all $xx->slice('(1)') == 699);

# test that continues in a threadloop work
{
    my $in = sequence(10);
    my $got = $in->zeroes;
    my $exp = $in->copy;
    my $tmp = $exp->where( ! ($in % 2) );
    $tmp .= 0;

    test_threadloop_continue( $in, $got );

    ok( tapprox( $got, $exp ), "continue works in threadloop" )
      or do { diag "got     : $got"; diag "expected: $exp" };
}
EOF

);

my %OTHERPARSFILES = (
    'Makefile.PL' => <<'EOF',
use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;
my @pack = (["otherpars.pd", qw(Otherpars PDL::Otherpars)]);
sub MY::postamble {
	pdlpp_postamble(@pack);
};  # Add genpp rule
WriteMakefile(pdlpp_stdargs(@pack));
EOF

    'otherpars.pd' => <<'EOF',
pp_core_importList( '()' );

pp_def( "myexternalfunc",
  Pars => " p(m);  x(n);  [o] y(); [t] work(wn); ",
    RedoDimsCode => '
    int im = $PDL(p)->dims[0];
    int in = $PDL(x)->dims[0];
    int min = in + im * im;
    int inw = $PDL(work)->dims[0];
    $SIZE(wn) = inw >= min ? inw : min;',
	OtherPars => 'int flags;',
    Code => 'int foo = 1;  ');

pp_done();
EOF

    't/all.t' => <<'EOF',
use strict;
use warnings;
use Test::More tests => 1;
use PDL::LiteF;
use_ok 'PDL::Otherpars';
EOF

);

do_tests(\%PPTESTFILES);
in_dir(
    sub {
        hash2files(File::Spec->curdir, \%OTHERPARSFILES);
        local $ENV{PERL5LIB} = join $Config{path_sep}, @INC;
        run_ok(qq{"$^X" Makefile.PL});
        my $cmd = qq{"$Config{make}" test};
        my $buffer;
        my $res = run(command => $cmd, buffer => \$buffer);
        ok !$res, 'Fails to build if invalid';
        like $buffer, qr/Invalid OtherPars name/, 'Fails if given invalid OtherPars name';
    },
);

sub do_tests {
    my ($hash) = @_;
    in_dir(
        sub {
            hash2files(File::Spec->curdir, $hash);
            local $ENV{PERL5LIB} = join $Config{path_sep}, @INC;
            run_ok(qq{"$^X" Makefile.PL});
            run_ok(qq{"$Config{make}" test});
        },
    );
}

sub run_ok {
    my ($cmd) = @_;
    my $buffer;
    my $res = run(command => $cmd, buffer => \$buffer);
    if (!$res) {
        ok 0, $cmd;
        diag $buffer;
        return;
    }
    ok 1, $cmd;
}

sub hash2files {
    my ($prefix, $hashref) = @_;
    while(my ($file, $text) = each %$hashref) {
        # Convert to a relative, native file path.
        $file = File::Spec->catfile(File::Spec->curdir, $prefix, split m{\/}, $file);
        my $dir = dirname($file);
        mkpath $dir;
        my $utf8 = ($] < 5.008 or !$Config{useperlio}) ? "" : ":utf8";
        open(my $fh, ">$utf8", $file) || die "Can't create $file: $!";
        print $fh $text;
        close $fh;
    }
}

sub in_dir {
    my $code = shift;
    require File::Temp;
    my $dir = shift || File::Temp::tempdir(TMPDIR => 1, CLEANUP => 1);
    # chdir to the new directory
    my $orig_dir = getcwd();
    chdir $dir or die "Can't chdir to $dir: $!";
    # Run the code, but trap the error so we can chdir back
    my $return;
    my $ok = eval { $return = $code->(); 1; };
    my $err = $@;
    # chdir back
    chdir $orig_dir or die "Can't chdir to $orig_dir: $!";
    # rethrow if necessary
    die $err unless $ok;
    return $return;
}
