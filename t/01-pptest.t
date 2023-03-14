use strict;
use warnings;
use ExtUtils::MakeMaker::Config; # to pick up EUMM-targeted config overrides
use Test::More $Config{usedl}
    ? ()
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
my @pack = (["tests.pd", qw(Tests PDL::Tests), '', 1]);
sub MY::postamble {
	pdlpp_postamble(@pack);
};  # Add genpp rule
my %hash = pdlpp_stdargs(@pack);
$hash{OBJECT} .= ' ppcp$(OBJ_EXT)';
WriteMakefile(%hash);
EOF

    'ppcp.c' => <<'EOF',
#include "pdl.h"
/* to test the $P vaffining */
void ppcp(PDL_Byte *dst, PDL_Byte *src, int len)
{
  int i;
  for (i=0;i<len;i++)
     *dst++=*src++;
}

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
EOF

    'tests.pd' => <<'EOF',
# make sure the deprecation mechanism throws warnings
pp_deprecate_module( infavor => "PDL::Test::Fancy" );
our $VERSION = '0.01'; # so the Makefile.PL's VERSION_FROM picks it up
pp_setversion(qq{'0.01'}); # this doesn't use $VERSION only to check a bug is fixed
pp_add_macros(SUCC => sub { "($_[0] + 1)" });

sub pp_deft {
    my ($name,%hash) = @_;
##    $hash{Doc} = "=for ref\n\ninternal\n\nonly for internal testing purposes\n";
    $hash{Doc} = undef;
    $name = "test_$name";  # prepend test_ to name
    pp_def($name,%hash);
}

pp_addhdr('
void ppcp(PDL_Byte *dst, PDL_Byte *src, int len);
');

# test the $P vaffine behaviour
# when 'phys' flag is in.
pp_deft('foop',
	Pars => 'byte [phys]a1(n); byte [o,phys]b(n)',
	GenericTypes => [B],
	Code => 'ppcp($P(b),$P(a1),$SIZE(n));',
);

# test single-used phys dim of 1 ok
pp_deft('foop1',
	Pars => 'byte a1(z); byte [o,phys]b(n)',
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

pp_deft("gelsd",
        Pars => '[io,phys]A(m,n); [io,phys]B(p,q); [phys]rcond(); [o,phys]s(r); int [o,phys]rank();int [o,phys]info()',
        RedoDimsCode => '$SIZE(r) = PDLMIN($SIZE(m),$SIZE(n));',
        GenericTypes => ['F'],
        Code => '$CROAK("croaking");'
);

pp_deft('fooseg',
        Pars => 'a(n);  [o]b(n);',
        Code => '
	   loop(n) %{ $b() = $a(); %}
');

# adapted from PDL::NDBin: if in=null and b is a scalar, was SEGV-ing
pp_deft( '_flatten_into',
        Pars => "in(m); indx b(m); [o] idx(m)",
        Code => '
                loop(m) %{ $idx() = $in(); %}
        ',
);

pp_addhdr << 'EOH';
void tinplace_c1(int n, PDL_Float* data);
void tinplace_c2(int n, PDL_Float* data1, PDL_Float* data2);
void tinplace_c3(int n, PDL_Float* data1, PDL_Float* data2, PDL_Float* data3);
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

pp_deft( 'broadcastloop_continue',
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

pp_deft('succ',
  Pars => 'a(); [o] b()',
  GenericTypes => ['F'],
  Code => '$b() = $SUCC($a());',
);

# test whitespace problem with POD and pp_addxs
pp_addxs( '', <<'EOXS' );

int
just_one()
     CODE:
     RETVAL = 1;
     OUTPUT:
     RETVAL

=pod

=begin comment

A comment.

=end comment

=cut

EOXS

# test whitespace problem with pp_line_numbers and pp_add_boot
pp_add_boot pp_line_numbers(__LINE__, q{
        /* nothing happening here */
});

# test fixed value for named dim, wrong Code for simplicity
pp_deft('Cpow',
	Pars => 'a(m=2); b(m=2); [o]c(m=2)',
	Code => '$c(m => 0) = $a(m => 0) + $b(m => 0);',
);

# test XS args with OtherPars
pp_deft('gl_arrows',
	Pars => 'coords(tri=3,n); int indsa(); int indsb();',
	OtherPars => 'float headlen; float width;',
	Code => ';', # do nothing
);

# test XS args with funky Pars ordering
pp_deft('polyfill_pp',
	Pars => 'int [o,nc] im(m,n); float ps(two=2,np); int col()',
	Code => ';', # do nothing
);

# test valid non-single-letter GenericTypes arg
pp_deft("rice_compress",
        Pars => 'in(n); [o]out(m); int[o]len(); lbuf(n)',
        GenericTypes =>['B','S','US','L'],
        Code => ';', # do nothing
);

pp_deft('output_op',
  Pars => 'in(n=2)',
  OtherPars => '[o] PDL_Anyval v0; [o] PDL_Anyval v1',
  Code => '
    pdl_datatypes dt = $PDL(in)->datatype;
    ANYVAL_FROM_CTYPE($COMP(v0), dt, $in(n=>0));
    ANYVAL_FROM_CTYPE($COMP(v1), dt, $in(n=>1));
  ',
);
pp_deft('output_op2',
  Pars => 'in(n=2); [o] out()',
  OtherPars => '[o] PDL_Anyval v0; [o] PDL_Anyval v1',
  Code => '
    pdl_datatypes dt = $PDL(in)->datatype;
    ANYVAL_FROM_CTYPE($COMP(v0), dt, $in(n=>0));
    ANYVAL_FROM_CTYPE($COMP(v1), dt, $in(n=>1));
  ',
);
pp_deft('output_op3',
  Pars => 'in(n=2); [o] out()',
  OtherPars => '[o] PDL_Anyval v0; [o] PDL_Anyval v1',
  Code => '
    pdl_datatypes dt = $PDL(in)->datatype;
    ANYVAL_FROM_CTYPE($COMP(v0), dt, $in(n=>0));
    ANYVAL_FROM_CTYPE($COMP(v1), dt, $in(n=>1));
  ',
  PMCode => 'sub PDL::test_output_op3 { goto &PDL::_test_output_op3_int }',
);

pp_addhdr('
typedef NV NV_ADD1;
');
pp_add_typemaps(string=><<'EOT');
TYPEMAP: <<END_OF_TYPEMAP
TYPEMAP
NV_ADD1 T_NV_ADD1

INPUT
T_NV_ADD1
  $var = SvNV($arg) + 1;

OUTPUT
T_NV_ADD1
  sv_setnv($arg, $var - 1);
END_OF_TYPEMAP
EOT
pp_deft('typem',
  Pars => 'int [o] out()',
  OtherPars => '[o] NV_ADD1 v1',
  Code => '$out() = $COMP(v1); $COMP(v1) = 8;',
);

pp_done;

# this tests the bug with a trailing comment and *no* newline
EOF

    't/all.t' => <<'EOF',
use strict;
use warnings;
use Test::More;
use Test::Warn;
BEGIN { $ENV{PDL_AUTOPTHREAD_TARG} = 1 } # for continue-in-broadcastloop test
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
    my($x,$y) = @_;
    my $c = abs($x-$y);
    my $d = max($c);
    return $d < 0.01;
}

my $x = xvals(zeroes(byte, 2, 4));
my $y;

# $P() affine tests
test_foop($x,($y=null));
ok( tapprox($x,$y) )
  or diag $y;

test_foop($x->transpose,($y=null));
ok( tapprox($x->transpose,$y) )
  or diag $y;

my $vaff = $x->dummy(2,3)->xchg(1,2);
test_foop($vaff,($y=null));
ok( tapprox($vaff,$y) )
  or diag ($vaff, $vaff->dump);

eval { test_foop($x,($y=pdl([1]))) };
isnt $@, '', '[phys] with multi-used mismatched dim of 1 throws exception';

eval { test_foop(pdl([1]),($y=pdl([1]))) };
is $@, '', '[phys] with multi-used matched dim of 1 no exception';

eval { test_foop1($x,($y=pdl([1]))) };
is $@, '', '[phys] with single-used dim of 1 no exception';

# float qualifier
$x = ones(byte,3000);
test_fsumover($x,($y=null));
is( $y->get_datatype, $PDL_F );
is( $y->at, 3000 );

# int+ qualifier
for (byte,short,ushort,long,float,double) {
  $x = ones($_,3000);
  test_nsumover($x,($y=null));
  is( $y->get_datatype, (($PDL_L > $_->[0]) ? $PDL_L : $_->[0]) );
  is( $y->at, 3000 );
}

test_setdim(($x=null),10);
is( join(',',$x->dims), "10" );
ok( tapprox($x,sequence(10)) );

{
my @msg;
local $SIG{__WARN__} = sub { push @msg, @_ };
eval { nan(2,2)->test_gelsd(nan(2,2), -3) };
like $@, qr/croaking/, 'right error message';
is_deeply \@msg, [], 'no warnings' or diag explain \@msg;
}

# this used to segv under solaris according to Karl
{
  my $ny=7;
  $x = double xvals zeroes (20,$ny);
  test_fooseg $x, $y=null;
  ok( 1 );  # if we get here at all that is alright
  ok( tapprox($x,$y) )
    or diag($x, "\n", $y);
}

eval { test__flatten_into(null, 2) };
ok 1; #was also segfaulting

# test the bug alluded to in the comments in pdl_changed (pdlapi.c)
# used to segfault
my $xx=ones(float,3,4);
my $sl1 = $xx->slice('(0)');
my $sl11 = $sl1->slice('');
my $sl2 = $xx->slice('(1)');
my $sl22 = $sl2->slice('');

test_fooflow2($sl11, $sl22);

ok(all $xx->slice('(0)') == 599);
ok(all $xx->slice('(1)') == 699);

# test that continues in a broadcastloop work
{
    my $in = sequence(10);
    my $got = $in->zeroes;
    my $exp = $in->copy;
    my $tmp = $exp->where( ! ($in % 2) );
    $tmp .= 0;

    test_broadcastloop_continue( $in, $got );

    ok( tapprox( $got, $exp ), "continue works in broadcastloop" )
      or do { diag "got     : $got"; diag "expected: $exp" };
}

test_Cpow(sequence(2), 1);

test_polyfill_pp(zeroes(5,5), ones(2,3), 1);

is test_succ(2)."", 3, 'test pp_add_macros works';

test_output_op([5,7], my $v0, my $v1);
is_deeply [$v0,$v1], [5,7], 'output OtherPars work';
eval { test_output_op(sequence(2,3), my $v0, my $v1) };
isnt $@, '', 'broadcast with output OtherPars throws';

test_output_op2([5,7], my $v0_2, my $v1_2);
is_deeply [$v0_2,$v1_2], [5,7], 'output OtherPars work 2';
eval { test_output_op2(sequence(2,3), my $v0_2, my $v1_2) };
isnt $@, '', 'broadcast with output OtherPars throws 2';

test_output_op3([5,7], my $out3 = PDL->null, my $v0_3, my $v1_3);
is_deeply [$v0_3,$v1_3], [5,7], 'output OtherPars work 3' or diag "got: ",$v0_3," ",$v1_3;

my $o = test_typem(my $oth = 3);
is "$o", 4;
is "$oth", 7;

done_testing;
EOF

);

my %BADOTHERPARSFILES = (
    'Makefile.PL' => <<'EOF',
use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;
my @pack = (["otherpars.pd", qw(Otherpars PDL::Otherpars)]);
sub MY::postamble { pdlpp_postamble(@pack) }
WriteMakefile(pdlpp_stdargs(@pack));
EOF
    'otherpars.pd' => <<'EOF',
pp_def( "myexternalfunc",
  Pars => " p(m);  x(n);  [o] y(); [t] work(wn); ",
  OtherPars => 'int flags;',
    RedoDimsCode => '
    int im = $PDL(p)->dims[0];
    int in = $PDL(x)->dims[0];
    int min = in + im * im;
    int inw = $PDL(work)->dims[0];
    $SIZE(wn) = inw >= min ? inw : min;',
    Code => 'int foo = 1;  ');

pp_def( "myexternalfunc2",
  Pars => "x(m);",
  OtherPars => 'int I;',
  Code => 'int foo = 1;  '
);

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

my %BADPARSFILES = (
    'Makefile.PL' => <<'EOF',
use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;
my @pack = (["otherpars.pd", qw(Otherpars PDL::Otherpars)]);
sub MY::postamble { pdlpp_postamble(@pack) }
WriteMakefile(pdlpp_stdargs(@pack));
EOF
    'otherpars.pd' => <<'EOF',
pp_def( "myexternalfunc3",
  Pars => "I(m);",
  Code => 'int foo = 1;  '
);
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

my %THREADTESTFILES = (
    'Makefile.PL' => <<'EOF',
use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;
my @pack = (["threadtest.pd", qw(ThreadTest PDL::ThreadTest)]);
sub MY::postamble {
	pdlpp_postamble(@pack);
};  # Add genpp rule
WriteMakefile(pdlpp_stdargs(@pack));
EOF

    'threadtest.pd' => <<'EOF',
# previously in t/inline-comment-test.t

pp_addpm(pp_line_numbers(__LINE__-1, q{ sub myfunc { } }));

pp_def('testinc',
        Pars => 'a(); [o] b()',
        Code => q{
           /* emulate user debugging */

           /* Why doesn't this work???!!!! */
       threadloop %{
    /*         printf("  %f, %f\r", $a(), $b());
             printf("  Here\n");
        */

                 /* Sanity check */
                 $b() = $a() + 1;

         %}

        },
);

# make sure that if the word "broadcastloop" appears, later automatic broadcastloops
# will not be generated, even if the original broadcastloop was commented-out

pp_def('testinc2',
        Pars => 'a(); [o] b()',
        Code => q{
           /* emulate user debugging */

           /* Why doesn't this work???!!!! */
   /*    threadloop %{
             printf("  %f, %f\r", $a(), $b());
             printf("  Here\n");
         %}
        */
          /* Sanity check */
          $b() = $a() + 1;

        },
);

pp_done();
EOF

    't/all.t' => <<'EOF',
use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use_ok 'PDL::ThreadTest';

my $x = sequence(3,3);

my $y = $x->testinc;

ok(all ($y == $x+1), 'Sanity check runs correctly');

# Test the inability to comment-out a broadcastloop. This is documented on the
# 11th page of the PDL::PP chapter of the PDL book. If somebody ever fixes this
# wart, this test will fail, in which case the book's text should be updated.
$y = $x->testinc2;
TODO: {
        # Note: This test appears to fail on Cygwin and some flavors of Linux.
        local $TODO = 'This test inexplicably passes on some machines';
        ok(not (all $y == $x + 1), 'WART: commenting out a broadcastloop does not work')
                or diag("\$x is $x and \$y is $y");
}

done_testing;
EOF

);

do_tests(\%THREADTESTFILES);
do_tests(\%PPTESTFILES);
do_tests(\%BADOTHERPARSFILES, qr/Invalid OtherPars name/);
do_tests(\%BADPARSFILES, qr/Invalid Pars name/);

sub do_tests {
    my ($hash, $error_re, $dir) = @_;
    in_dir(
        sub {
            hash2files(File::Spec->curdir, $hash);
            local $ENV{PERL5LIB} = join $Config{path_sep}, @INC;
            run_ok(qq{"$^X" Makefile.PL});
            run_ok(qq{"$Config{make}" test}, $error_re);
        },
        $dir,
    );
}

sub run_ok {
    my ($cmd, $error_re) = @_;
    my $res = run(command => $cmd, buffer => \my $buffer);
    if ($error_re) {
        ok !$res, 'Fails to build if invalid';
        like $buffer, $error_re, 'Fails with expected error';
        return;
    }
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

done_testing;
