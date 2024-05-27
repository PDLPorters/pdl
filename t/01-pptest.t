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
EOF

    'tests.pd' => <<'EOF',
# make sure the deprecation mechanism throws warnings
pp_deprecate_module( infavor => "PDL::Test::Fancy" );
our $VERSION = '0.01'; # so the Makefile.PL's VERSION_FROM picks it up
pp_setversion(qq{'0.01'}); # this doesn't use $VERSION only to check a bug is fixed
pp_add_macros(SUCC => sub { "($_[0] + 1)" });

pp_addhdr('
void ppcp(PDL_Byte *dst, PDL_Byte *src, int len);
');

# test the $P vaffine behaviour
# when 'phys' flag is in.
pp_def('foop',
	Pars => 'byte [phys]a1(n); byte [o,phys]b(n)',
	GenericTypes => [B],
	Code => 'ppcp($P(b),$P(a1),$SIZE(n));',
);

# test single-used phys dim of 1 ok
pp_def('foop1',
	Pars => 'byte a1(z); byte [o,phys]b(n)',
	GenericTypes => [B],
	Code => 'ppcp($P(b),$P(a1),$SIZE(n));',
);

# float qualifier
# and also test if numerals in variable name work
pp_def(
	'fsumover',
	Pars => 'a1(n); float [o]b();',
	Code => 'PDL_Float tmp = 0;
	 loop(n) %{ tmp += $a1(); %}
	 $b() = tmp;'
);

# test GENERIC with type+ qualifier
pp_def(
	'nsumover',
	Pars => 'a(n); int+ [o]b();',
	Code => '$GENERIC(b) tmp = 0;
	 loop(n) %{ tmp += $a(); %}
	 $b() = tmp;'
);

pp_def("gelsd",
        Pars => '[io,phys]A(m,n); [io,phys]B(p,q); [phys]rcond(); [o,phys]s(r=CALC(PDLMIN($SIZE(m),$SIZE(n)))); int [o,phys]rank();int [o,phys]info()',
        GenericTypes => ['F'],
        Code => '$CROAK("croaking");'
);

pp_def( 'broadcastloop_continue',
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

pp_def('succ',
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

# test XS args with OtherPars
pp_def('gl_arrows',
	Pars => 'coords(tri=3,n); int indsa(); int indsb();',
	OtherPars => 'float headlen; float width;',
	Code => ';', # do nothing
);

# test XS args with funky Pars ordering
pp_def('polyfill_pp',
	Pars => 'int [io] im(m,n); float ps(two=2,np); int col()',
	Code => ';', # do nothing
);

# test valid non-single-letter GenericTypes arg
pp_def("rice_compress",
        Pars => 'in(n); [o]out(m); int[o]len(); lbuf(n)',
        GenericTypes =>['B','S','US','L'],
        Code => ';', # do nothing
);

pp_def('output_op',
  Pars => 'in(n=2)',
  OtherPars => '[o] PDL_Anyval v0; [o] PDL_Anyval v1',
  Code => '
    pdl_datatypes dt = $PDL(in)->datatype;
    ANYVAL_FROM_CTYPE($COMP(v0), dt, $in(n=>0));
    ANYVAL_FROM_CTYPE($COMP(v1), dt, $in(n=>1));
  ',
);
pp_def('output_op2',
  Pars => 'in(n=2); [o] out()',
  OtherPars => '[o] PDL_Anyval v0; [o] PDL_Anyval v1',
  Code => '
    pdl_datatypes dt = $PDL(in)->datatype;
    ANYVAL_FROM_CTYPE($COMP(v0), dt, $in(n=>0));
    ANYVAL_FROM_CTYPE($COMP(v1), dt, $in(n=>1));
  ',
);
pp_def('output_op3',
  Pars => 'in(n=2); [o] out()',
  OtherPars => '[o] PDL_Anyval v0; [o] PDL_Anyval v1',
  Code => '
    pdl_datatypes dt = $PDL(in)->datatype;
    ANYVAL_FROM_CTYPE($COMP(v0), dt, $in(n=>0));
    ANYVAL_FROM_CTYPE($COMP(v1), dt, $in(n=>1));
  ',
  PMCode => 'sub PDL::output_op3 { goto &PDL::_output_op3_int }',
);

pp_def('incomp_dim',
  Pars => '[o] a();',
  OtherPars => 'PDL_Indx d[];',
  Code => '$a() = $COMP(d_count);',
);

pp_addhdr('
typedef NV NV_ADD1;
typedef HV* NV_HR;
typedef char thing;
');
pp_add_typemaps(string=><<'EOT');
TYPEMAP
NV_ADD1 T_NV_ADD1
NV_HR T_HVREF
thing* T_PTROBJ

INPUT
T_NV_ADD1
  $var = SvNV($arg) + 1

OUTPUT
T_NV_ADD1
  sv_setnv($arg, $var - 1);
EOT

pp_def('typem',
  Pars => 'int [o] out()',
  OtherPars => '[io] NV_ADD1 v1; NV_HR v2; thing *ptr',
  Code => '$out() = $COMP(v1); $COMP(v1) = 8;',
);

pp_def('incomp_in',
  Pars => '[o] out()',
  OtherPars => 'pdl *ins[]',
  RedoDimsCode => <<'EOC',
pdl **ins = $COMP(ins);
PDL_Indx i;
for (i = 0; i < $COMP(ins_count); i++) {
  pdl *in = ins[i];
  PDL_RETERROR(PDL_err, PDL->make_physdims(in));
  if (in->ndims != 1)
    $CROAK("input ndarray %"IND_FLAG" has %"IND_FLAG" dims, not 1", i, in->ndims);
  if (!$PRIV(bvalflag) && (in->state & PDL_BADVAL)) $PRIV(bvalflag) = 1;
}
EOC
  Code => <<'EOC',
pdl **ins = $COMP(ins);
PDL_Indx i;
for (i = 0; i < $COMP(ins_count); i++)
  PDL_RETERROR(PDL_err, PDL->make_physical(ins[i]));
$out() = 0;
for (i = 0; i < $COMP(ins_count); i++) {
  pdl *in = ins[i];
  PDL_Indx j;
#define X_CAT_INNER(datatype_in, ctype_in, ppsym_in, ...) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype_in, in, (in), 1, ppsym_in) \
  for(j=0; j<in->nvals; j++) { \
    if ($PRIV(bvalflag) && PDL_ISBAD2(in_datap[j], in_badval, ppsym_in, in_badval_isnan)) continue; \
    $out() += in_datap[j]; \
  }
  PDL_GENERICSWITCH(PDL_TYPELIST_ALL, in->datatype, X_CAT_INNER, $CROAK("Not a known data type code=%d", in->datatype))
#undef X_CAT_INNER
}
EOC
);

pp_def('incomp_out',
  Pars => 'in(n)',
  OtherPars => 'PDL_Indx howmany; [o] pdl *outs[]',
  HandleBad => 1,
  CallCopy => 0,
  GenericTypes => [PDL::Types::ppdefs_all()],
  Code => <<'EOC',
pdl **outs = malloc(($COMP(outs_count) = $COMP(howmany)) * sizeof(pdl*));
$COMP(outs) = outs;
PDL_Indx i, ndims = $PDL(in)->ndims, dims[ndims];
for (i = 0; i < ndims; i++) dims[i] = $PDL(in)->dims[i];
for (i = 0; i < $COMP(outs_count); i++) {
  pdl *o = outs[i] = PDL->pdlnew();
  if (!o) { for (i--; i >= 0; i--) PDL->destroy(outs[i]); free(outs); $CROAK("Failed to create ndarray"); }
  o->datatype = $PDL(in)->datatype;
  PDL_err = PDL->setdims(o, dims, ndims);
  if (PDL_err.error) { for (; i >= 0; i--) PDL->destroy(outs[i]); free(outs); return PDL_err; }
  PDL_err = PDL->allocdata(o);
  if (PDL_err.error) { for (; i >= 0; i--) PDL->destroy(outs[i]); free(outs); return PDL_err; }
  PDL_DECLARE_PARAMETER_BADVAL($GENERIC(in), o, (o), 1, $PPSYM(in))
  loop(n) %{ o_datap[n] = $in(); %}
}
EOC
);

pp_def('index_prec', # check $a(n=>x+1) works
  Pars => 'in(n); [o]out()',
  Code => 'loop (n) %{ if (n > 1) $out() += $in(n=>n-1); %}',
);

pp_def("diff_central",
  Pars => 'double x(); double [o] res();',
  OtherPars => 'SV* function;',
  Code => ';',
);

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

pp_def('or2',
  Pars => 'a(); b(); [o]c();',
  OtherPars => 'int swap; char *ign; int ign2',
  OtherParsDefaults => { swap => 0, ign=>'""', ign2=>0 },
  ArgOrder => 1,
  Code => '$c() = $a() | $b();',
  GenericTypes => [qw(A B S U L K N P Q)],
);

# from HMM
pp_def('logadd',
       Pars => 'a(); b(); [o]c()',
       GenericTypes => [qw(F D LD)],
       Inplace=>['a'], ##-- can run inplace on a()
       Code => ';',
      );

pp_def('ftr',
       Pars => 'a(); [o]b()',
       Code => ';',
       FtrCode => "  sv_setiv(perl_get_sv(\"main::FOOTERVAL\",TRUE), 1);\n",
      );

pp_def('ftrPM',
       Pars => 'a(); [o]b()',
       Code => ';',
       HdrCode => "  sv_setiv(perl_get_sv(\"main::HEADERVAL\",TRUE), 1);\n",
       FtrCode => "  sv_setiv(perl_get_sv(\"main::FOOTERVAL\",TRUE), 1);\n",
       PMCode => <<'EOPM',
sub PDL::ftrPM {
  my ($a, $b) = @_;
  $b //= PDL->null;
  PDL::_ftrPM_int($a, $b);
  $b;
}
EOPM
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
foop($x,($y=null));
ok( tapprox($x,$y) )
  or diag $y;

foop($x->transpose,($y=null));
ok( tapprox($x->transpose,$y) )
  or diag $y;

my $vaff = $x->dummy(2,3)->xchg(1,2);
foop($vaff,($y=null));
ok( tapprox($vaff,$y) )
  or diag ($vaff, $vaff->dump);

eval { foop($x,($y=pdl([1]))) };
isnt $@, '', '[phys] with multi-used mismatched dim of 1 throws exception';

eval { foop(pdl([1]),($y=pdl([1]))) };
is $@, '', '[phys] with multi-used matched dim of 1 no exception';

eval { foop1($x,($y=pdl([[1],[1],[1],[1]]))) };
is $@, '', '[phys] with single-used dim of 1 no exception';

# float qualifier
$x = ones(byte,3000);
fsumover($x,($y=null));
is( $y->get_datatype, $PDL_F );
is( $y->at, 3000 );

# int+ qualifier
for (byte,short,ushort,long,float,double) {
  $x = ones($_,3000);
  nsumover($x,($y=null));
  is( $y->get_datatype, (($PDL_L > $_->[0]) ? $PDL_L : $_->[0]) );
  is( $y->at, 3000 );
}

{
my @msg;
local $SIG{__WARN__} = sub { push @msg, @_ };
eval { nan(2,2)->gelsd(nan(2,2), -3) };
like $@, qr/croaking/, 'right error message';
is_deeply \@msg, [], 'no warnings' or diag explain \@msg;
}

# test that continues in a broadcastloop work
{
    my $in = sequence(10);
    my $got = $in->zeroes;
    my $exp = $in->copy;
    my $tmp = $exp->where( ! ($in % 2) );
    $tmp .= 0;

    broadcastloop_continue( $in, $got );

    ok( tapprox( $got, $exp ), "continue works in broadcastloop" )
      or do { diag "got     : $got"; diag "expected: $exp" };
}

polyfill_pp(zeroes(5,5), ones(2,3), 1);
eval { polyfill_pp(ones(2,3), 1) };
like $@, qr/Usage/;

is succ(2)."", 3, 'test pp_add_macros works';

output_op([5,7], my $v0, my $v1);
is_deeply [$v0,$v1], [5,7], 'output OtherPars work';
($v0, $v1) = output_op([5,7]);
is_deeply [$v0,$v1], [5,7], 'output OtherPars work 1a';
eval { output_op(sequence(2,3), my $v0, my $v1) };
isnt $@, '', 'broadcast with output OtherPars throws';

output_op2([5,7], my $n=PDL->null, my $v0_2, my $v1_2);
is_deeply [$v0_2,$v1_2], [5,7], 'output OtherPars work 2';
(undef, $v0_2, $v1_2) = output_op2([5,7]);
is_deeply [$v0_2,$v1_2], [5,7], 'output OtherPars work 2a';
eval { output_op2(sequence(2,3), my $n=PDL->null, my $v0_2, my $v1_2) };
like $@, qr/Can't broadcast/, 'broadcast with output OtherPars throws 2';

output_op3([5,7], my $out3 = PDL->null, my $v0_3, my $v1_3);
is_deeply [$v0_3,$v1_3], [5,7], 'output OtherPars work 3' or diag "got: ",$v0_3," ",$v1_3;

incomp_dim(my $o = PDL->null, [0..3]);
is "$o", 4;
$o = incomp_dim([0..3]);
is "$o", 4;

my $ptrObj = bless \(my $thing), 'thingPtr';
$o = typem(my $oth = 3, {}, $ptrObj);
is "$o", 4;
is "$oth", 7;

typem($o = PDL->null, $oth = 3, {}, $ptrObj);
is "$o", 4;
is "$oth", 7;

eval {typem($o = PDL->null, $oth = 3, [], $ptrObj);};
like $@, qr/^typem:.*not a HASH reference/i;

incomp_in($o = PDL->null, [sequence(3), sequence(byte, 4)]);
is "$o", 9;
$o = incomp_in([sequence(3), sequence(byte, 4)]);
is "$o", 9;
my $one_bad = sequence(byte, 4);
$one_bad->badflag(1);
$one_bad->badvalue(2);
$o = incomp_in([sequence(3), $one_bad]);
is "$o", 7;
incomp_in($o = PDL->null, []);
is "$o", 0;
incomp_in($o = PDL->null, undef);
is "$o", 0;
eval { incomp_in($o = PDL->null, 'hello') };
isnt $@, '';

incomp_out(sequence(3), 2, my $nds);
is 0+@$nds, 2;
is +($nds->[0]//'undef').'', "[0 1 2]";
$nds = incomp_out(sequence(3), 2);
is 0+@$nds, 2;
is +($nds->[0]//'undef').'', "[0 1 2]";

is index_prec(sequence(2,6)->slice('(1)')).'', 24, 'index precedence OK';

eval { diff_central(pdl(1), sub {}) };
is $@, '';

{
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
}

eval { is ''.or2(pdl(1), pdl(1), 0), '1' };
is $@, '';

eval { ldouble(4)->logadd(3) };
is $@, '';

undef $main::FOOTERVAL;
ftr(1);
is $main::FOOTERVAL, 1;

undef $main::HEADERVAL;
undef $main::FOOTERVAL;
ftrPM(1);
is $main::HEADERVAL, 1;
is $main::FOOTERVAL, 1;

done_testing;
EOF

);

do_tests(\%PPTESTFILES);

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
    my $dir = shift || File::Spec->catdir(File::Spec->curdir, './.pptest');
    mkpath $dir;
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
