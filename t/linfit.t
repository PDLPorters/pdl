use PDL::LiteF;
BEGIN {
        eval " use PDL::Fit::Linfit; ";
        $loaded = ($@ ? 0 : 1);
}

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
print "1..2\n";

unless ($loaded) {
        for (1..2) {
                print "ok $_ # Skipped: probably PDL::Slatec not available.\n";
        }
        exit;
}


my $testNo = 1;


# Simple Test Case:

# Generate data from a set of functions
my $xvalues = sequence(100);
$data = 3*$xvalues + 2*cos($xvalues) + 3*sin($xvalues*2); 

# Fit functions are the linear, cos, and sin2x functions used in
#   the data generation step above:
$fitFuncs = cat $xvalues, cos($xvalues), sin($xvalues*2);

# Perform the fit, Coefs should equal 3,2,3
my ($yfit, $coeffs) = PDL::linfit1d($data,$fitFuncs);

my @coefs = $coeffs->list;

ok( $testNo++, tapprox( $coefs[0], 3) && tapprox( $coefs[1], 2) && tapprox( $coefs[2], 3) );


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
for($i=0;$i<$noPoints;$i++){
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


# setup matrix contains functions to fit
for ($i=0; $i<$noPoints; $i++) {

$functions[0][$i] = $pave;
$functions[1][$i] = $i;
$functions[2][$i] = sin($pi*$i/($noPoints-1));
$functions[3][$i] = sin(2*$pi*$i/($noPoints-1));

}

my $fitFuncs = new PDL( \@functions);

($yfit, $coeffs) = linfit1d( $data, $fitFuncs);

@coefs = $coeffs->list;

ok( $testNo++, tapprox( $coefs[0], $expectedCoefs[0]) && 
		tapprox( $coefs[1], $expectedCoefs[1]) &&
		tapprox( $coefs[2], $expectedCoefs[2]) &&
		tapprox( $coefs[3], $expectedCoefs[3]) 
	 );


sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = ref($c) ? $c->{PDL}->max : $c ;  # don't do a make if were are dealing 
					  # with a scalar
        $d < 0.00001;
}
#  Testing utility functions:
sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

