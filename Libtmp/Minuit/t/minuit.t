use strict;
use warnings;
use PDL;
use Test::More;
use PDL::Minuit;
use File::Temp qw( tempfile tempdir );
require File::Spec;

my $tempd = tempdir( CLEANUP => 1 ) or die "Couldn't get tempdir\n";

my $logfile = File::Spec->catfile($tempd, 'minuit.log.' . $$);

my $x = sequence(10);
my $y = 3.0 + 4.0*$x;

mn_init(\&chi2,
        {Log => $logfile,
        Title => 'test title'});

my $pars = pdl(2.5,3.0);
my $steps = pdl(0.3,0.5);
my @names = ('intercept','slope');

mn_def_pars($pars,
            $steps,
            {Names => \@names});

my $arglis = pdl (3.0);

ok !mn_excm('set pri',$arglis);

ok !mn_excm('migrad');

ok !mn_excm('minos');

my $emat = mn_emat();
my $emat_test = pdl [[0.34545455, -0.054545455], [-0.054545455,  0.012121212]];
ok(all(approx $emat, $emat_test)) or diag $emat;

my @got = mn_pout(1);
ok(approx $got[0], pdl 3) or diag "@got";

mn_err(1);
mn_stat();

done_testing;

sub chi2{
    my ($npar,$grad,$fval,$xval,$iflag) = @_;
    if($iflag == 4){
        $fval = (($y - $xval->slice(0) - $xval->slice(1)*$x)**2)->sumover;
    }
    return ($fval,$grad);
}
