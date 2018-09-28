# Test routine for PDL::Fit::Gaussian module

use PDL;
use PDL::Fit::Gaussian;

use Test::More tests => 3;

use strict;
use warnings;

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub nint{int($_[0]->at+0.5)};

my $g1 = pdl qw[ 2.1990459  1.9464173  2.1565406  2.1672124  2.2701938
1.82992   1.914893  2.1466146  1.8822749  2.0293979  2.0101469   2.210302
2.6183602  4.3191846  7.8333737  11.525845  13.069404  11.364827  7.2853706
4.3667506  2.2601078  2.0051197   1.802916  2.1735853  1.7985277  1.9498281
1.7745239  1.7534224  2.6137111  1.8443813  2.0064845  2.1981632  2.0572412
1.8928303  2.0703847  2.0121833  1.9967828  2.3846479  1.8907906  2.1486651];

my $g2 = pdl qw[  13.013418  11.397573  7.4494489  4.5594057  2.5728955
2.0687907  2.1953927  2.2819689  1.7046446  2.3276816  2.0130417    1.72691
1.8260466  2.0842572  2.2455532  1.9223378   1.695866  1.5893454  1.9787549
1.6941413  1.8576307  2.3780392  2.2588472  2.2080773  1.8754143   2.019966
1.9363813  2.1414206  2.0062853  2.0867273  2.0158617  1.6481802  1.9686077
2.2979197  2.2963699  2.1171346  1.8859732  2.1277667  2.0716804  1.9251175];

my $x1 = xvals($g1);
my $x2 = xvals($g2);

{ #test fitgauss1d specifying all output piddles in the call
    my ($xc, $pk, $fwhm, $back, $err, $fit);
    fitgauss1d($x1, $g1,$xc=null,$pk=null,$fwhm=null,$back=null,$err=null,$fit=null);
    ok( nint($xc)==16 && nint($pk)==11 && nint($fwhm)==4 && nint($back)==2
	&& nint($err)==0 && sum(abs($g1-$fit))<10,"fitgauss1d output=null");
}


{ #test fitgauss1d specifying only the input piddles
    my ($xc, $pk, $fwhm, $back, $err, $fit) = fitgauss1d($x1, $g1);
    ok( nint($xc)==16 && nint($pk)==11 && nint($fwhm)==4 && nint($back)==2
	&& nint($err)==0 && sum(abs($g1-$fit))<10,"fitgauss1d normal");
}

{ #test fitgauss1dr specifying only the input piddles
    my ($pk, $fwhm, $back, $err, $fit) = fitgauss1dr($x2,$g2);
    ok(nint($pk)==11 && nint($fwhm)==4 && nint($back)==2
       && nint($err)==0 && sum(abs($g2-$fit))<10,"fitgauss1dr normal");
}

done_testing;
