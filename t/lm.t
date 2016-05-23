# -*-perl-*-

use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Config;
use PDL::IO::Misc 'rcols';

BEGIN {
    if ($PDL::Config{WITH_SLATEC}) {
	eval " use PDL::Fit::LM; ";
	unless ($@) {
	    plan tests => 2;
	} 
	else {
	    plan skip_all => 'PDL::Fit::LM did not load. Is PDL::Slatec available?';
	}
    }
    else {
	plan skip_all => 'PDL::Fit::LM not available (needs PDL::Slatec)';
    }
}    

my ($t,$count,$sigma)=rcols(\*DATA,0,1,2);
my $initp = pdl(10,900,80,27,225);
my $gnuplot_pf_unweighted = pdl(7.96, 282.5, 70.0, 28.5, 117.7);
my $gnuplot_pf_weighted = pdl(5.53, 290.7, 46.6, 33.3, 162.7);

my ($yf,$pf,$cf,$if) = lmfit($t, $count, 1, \&const_2exp, $initp);
ok(all(abs(log10($pf/$gnuplot_pf_unweighted))<0.02),"Unweighted fit");

($yf,$pf,$cf,$if) = lmfit($t, $count, $sigma, \&const_2exp, $initp);
ok(all(abs(log10($pf/$gnuplot_pf_weighted))<0.02),"Weighted fit");

1;

sub const_2exp{
#constant plus 2 exponentials
    my ($x,$par,$ym,$dyda) = @_;
    my ($a1,$a2,$a3,$a4,$a5) = map { $par->slice("($_)") } (0..4);
    $ym .= $a1 + $a2*exp(-$x/$a4) + $a3*exp(-$x/$a5);
    my (@dy) = map {$dyda -> slice(",($_)") } (0..4);
    $dy[0] .= 1;
    $dy[1] .= exp(-$x/$a4);
    $dy[2] .= exp(-$x/$a5);
    $dy[3] .= $a2 * $x * exp(-$x/$a4)/$a4/$a4;
    $dy[4] .= $a3 * $x * exp(-$x/$a5)/$a5/$a5; 
}


__DATA__
# $Id: silver.dat,v 1.1.1.1 1998/04/15 19:16:42 lhecking Exp $
# This sample data was distributed with Gnuplot, which contains the following notice:
# Copyright (C) 1986 - 1993, 1998, 2004, 2007  Thomas Williams, Colin Kelley
# Permission to use, copy, and distribute this software and its
# documentation for any purpose with or without fee is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that copyright notice and this permission notice appear in
# supporting documentation.
10.000000 280.000000 16.733201 
20.000000 191.000000 13.820275 
30.000000 152.000000 12.328828 
40.000000 150.000000 12.247449 
50.000000 104.000000 10.198039 
60.000000 77.000000 8.774964 
70.000000 69.000000 8.306624 
80.000000 60.000000 7.745967 
90.000000 60.000000 7.745967 
100.000000 51.000000 7.141428 
110.000000 41.000000 6.403124 
120.000000 34.000000 5.830952 
130.000000 35.000000 5.916080 
140.000000 34.000000 5.830952 
150.000000 24.000000 4.898979 
160.000000 24.000000 4.898979 
170.000000 19.000000 4.358899 
180.000000 21.000000 4.582576 
190.000000 20.000000 4.472136 
200.000000 18.000000 4.242641 
210.000000 21.000000 4.582576 
220.000000 15.000000 3.872983 
230.000000 19.000000 4.358899 
240.000000 12.000000 3.464102 
250.000000 20.000000 4.472136 
260.000000 20.000000 4.472136 
270.000000 18.000000 4.242641 
280.000000 18.000000 4.242641 
290.000000 20.000000 4.472136 
300.000000 12.000000 3.464102 
310.000000 26.000000 5.099020 
320.000000 17.000000 4.123106 
330.000000 8.000000 2.828427 
340.000000 6.000000 2.449490 
350.000000 8.000000 2.828427 
360.000000 10.000000 3.162278 
370.000000 20.000000 4.472136 
380.000000 14.000000 3.741657 
390.000000 8.000000 2.828427 
400.000000 10.000000 3.162278 
410.000000 9.000000 3.000000 
420.000000 8.000000 2.828427 
430.000000 10.000000 3.162278 
440.000000 13.000000 3.605551 
450.000000 9.000000 3.000000 
460.000000 5.000000 2.236068 
470.000000 7.000000 2.645751 
480.000000 11.000000 3.316625 
500.000000 7.000000 2.645751 
510.000000 9.000000 3.000000 
520.000000 12.000000 3.464102 
530.000000 4.000000 2.000000 
540.000000 7.000000 2.645751 
550.000000 10.000000 3.162278 
560.000000 9.000000 3.000000 
580.000000 8.000000 2.828427 
590.000000 9.000000 3.000000 
600.000000 5.000000 2.236068 
