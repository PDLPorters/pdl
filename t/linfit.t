use Test::More;
use PDL::LiteF;

use strict;
use warnings;

eval {
	require PDL::Fit::Linfit;
	PDL::Fit::Linfit->import();
	1;
} or plan skip_all => "PDL::Fit::Linfit: $@";

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

plan tests => 2;

{
# Simple Test Case:

# Generate data from a set of functions
my $xvalues = sequence(100);
my $data = 3*$xvalues + 2*cos($xvalues) + 3*sin($xvalues*2);

# Fit functions are the linear, cos, and sin2x functions used in
#   the data generation step above:
my $fitFuncs = cat $xvalues, cos($xvalues), sin($xvalues*2);

# Perform the fit, Coefs should equal 3,2,3
my ($yfit, $coeffs) = PDL::linfit1d($data, $fitFuncs);

my @coefs = $coeffs->list;

ok all approx( $coeffs, pdl([3,2,3]) );
}

{
# More Complex Example


my $noPoints = 501;

my @expectedCoefs = qw( 0.988375918186647 -0.000278823311771375 0.161669997297754 0.0626069008452451);

my $noCoefs = 4;
my $i;
my  ($deltaT,$Amp,$lin,$HOper,$AmpHO,$Amphalf,$Ampfull);

my @PulsedB;

my  @Pulse;
my  $psum = 0;

my  $pi = 3.1415926;

my  $Pwidth = 2000;
my $pave;

$deltaT = 4;  # 4 nS increments
$Amp = 2.8;   # 2.8V amplitude of pulse
$lin = .2;
$HOper = 200;  # HO period
$AmpHO=.1;
$Amphalf = .5;
$Ampfull = .2;

# generate waveform:
for(my $i = 0; $i < $noPoints; $i++){
	$PulsedB[$i]=
		-$lin*1e-3*$i*$deltaT +
		$Amphalf*sin($pi/$Pwidth*$i*$deltaT)  +
		$Ampfull*sin(2*$pi/$Pwidth*$i*$deltaT) +
		$AmpHO*sin(2*$pi/$HOper*$i*$deltaT);

	$Pulse[$i] = $Amp*exp($PulsedB[$i]/20*2.3025851);
	$psum += $Pulse[$i];  # used to get DC value

	# printf(" %4d  %g  %g\n",$i,$PulsedB[$i],$Pulse[$i]);

}

$pave = $psum/$noPoints;

# printf("DC Value = %g\n",$pave);

# Make PDL from waveform:
my $data = new PDL( \@Pulse);
my @functions;

# setup matrix contains functions to fit
for ($i=0; $i<$noPoints; $i++) {

$functions[0][$i] = $pave;
$functions[1][$i] = $i;
$functions[2][$i] = sin($pi*$i/($noPoints-1));
$functions[3][$i] = sin(2*$pi*$i/($noPoints-1));

}

my $fitFuncs = new PDL( \@functions);

my ($yfit, $coeffs) = linfit1d( $data, $fitFuncs);

my @coefs = $coeffs->list;

ok all approx( $coeffs, pdl( \@expectedCoefs ) );
}
