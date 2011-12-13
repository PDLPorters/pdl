
no warnings qw(misc);
use PDL;
use PDL::Config;
use Test::More;
        
BEGIN{
  eval " use PDL::Minuit; ";
  unless ($@){
    plan tests => 5;
  }
  else {
    print "$@\n";
    plan skip_all => 'PDL::Minuit not available';
    exit;
  }
}

my $tempd = $PDL::Config{TEMPDIR} or die "TEMPDIR not found in %PDL::Config";
require File::Spec;
my $logfile = File::Spec->catfile($tempd, 'minuit.log.' . $$);

END {
  unlink $logfile if defined $logfile and -e $logfile;
}

$x = sequence(10);
$y = 3.0 + 4.0*$x;

mn_init(\&chi2,
        {Log => $logfile, 
        Title => 'test title'});

$pars = pdl(2.5,3.0);
$steps = pdl(0.3,0.5);
@names = ('intercept','slope');

mn_def_pars($pars,
            $steps,
            {Names => \@names});

$arglis = pdl (3.0);

$iflag = mn_excm('set pri',$arglis);
ok($iflag == 0);

$iflag = mn_excm('migrad');
ok($iflag == 0);

$iflag = mn_excm('minos');
ok($iflag == 0);


$emat = mn_emat();
my $emat_test = pdl [[0.34545455, -0.054545455], [-0.054545455,  0.012121212]];
my $diff = ((($emat - $emat_test)**2)->sum);
ok($diff < 1e-6);

@temp = mn_pout(1);
ok(($temp[0]-3) < 1e-6);

@test = mn_err(1);

@test = mn_stat();

sub chi2{
    my ($npar,$grad,$fval,$xval,$iflag) = @_;
    if($iflag == 4){
        $fval = (($y - $xval->slice(0) - $xval->slice(1)*$x)**2)->sumover;
    }
    return ($fval,$grad);
}

