=head1 NAME

PDL::Filter::LinPred - Linear predictive filtering

=head1 SYNOPSIS

	$a = new PDL::Filter::LinPred(
		{NLags => 10,
		 LagInterval => 2,
		 LagsBehind => 2,
		 Data => $dat});

	($pd,$corrslic) = $a->predict($dat);

=head1 DESCRIPTION

A filter by doing linear prediction: tries to predict the next value
in a data stream as accurately as possible. The filtered data is the
predicted value. The parameters are

=over 8

=item NLags

Number of time lags used for prediction

=item LagInterval

How many points each lag should be

=item LagsBehind

If, for some strange reason, you wish to predict not the next but
the one after that (i.e. usually f(t) is predicted from f(t-1) and f(t-2)
etc., but with LagsBehind => 2, f(t) is predicted from f(t-2) and f(t-3)).

=item Data

The input data, which may contain other dimensions past the first (time).
The extraneous dimensions are assumed to represent epochs so the data
is just concatenated.

=item AutoCovar

As an alternative to B<Data>, you can just give the temporal autocorrelation
function.

=item Smooth

Don't do prediction or filtering but smoothing.

=back

The method B<predict> gives a prediction for some data plus a corresponding
slice of the data, if evaluated in list context. This slice is given
so that you may, if you wish, easily plot them atop each other.

The rest of the documentation is under lazy evaluation.

=head1 AUTHOR

Copyright (C) Tuomas J. Lukka 1997.
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut

package PDL::Filter::LinSmooth;
use PDL;
use PDL::Basic;
use PDL::Slatec;
use PDL::Slices;
use PDL::Primitive;
use strict;

@PDL::Filter::LinSmooth::ISA = qw/PDL::Filter::LinPred/;

sub _ntotlags {
	my($this) = @_;
	return 2 * ( $this->{NLags} + $this->{LagsBehind} );
}

# nlags == 3, lagsbehind == 1 -> totlags = 7
#
# Symautocor: 6543210123456
# -> lags(nlags) ->
#
#	43210123456
#	54321012345
#	65432101234

# SMOOTH
sub _mk_mat {
	my($this) = @_;

	local $PDL::Debug = 1;

	my $n = $this->{LagsBehind};
	my $nl = $this->{NLags};
	my $nl1 = $nl-1;

	my $auc = $this->{AutoCor};

	my $autocov = PDL::float PDL->zeroes($nl*2,$nl*2);
	$this->{AutoCov} = $autocov;

	my $sal = $this->{SymAutoCor}->px->lags(0,1,$this->{NLags})->px;
	print "L,LB: $nl,$n\n";

	my ($tmp,$tmp2);
	PDL::Graphics::PG::imag ($sal->copy);

# First, the 2 diagonal slices
	($tmp = $autocov->slice("$nl:-1,$nl:-1")->px) .=
	($tmp2 = $autocov->slice("0:".($nl-1).",0:".($nl-1))->px) .=
		$sal->slice(($this->{NLags}+2*$this->{LagsBehind}-1).":".
			    (-1-($this->{NLags}+2*$this->{LagsBehind}+1)))->px;

# Then, the off-diagonal slices
	($tmp = $autocov->slice("-1:$nl,$nl1:0")) .=
	($tmp2 = $autocov->slice("0:$nl1,$nl:-1")) .=
		$sal->slice("0:$nl1");

	# Invert it

	my $autocinv = inv($autocov);
#	print "$autocinv,$auc,$n\n"; $auc->slice("$n:-1");
	$this->{AutoSliceUsed} = PDL->zeroes(2*$nl)->float;

	($tmp = $this->{AutoSliceUsed}->slice("0:$nl1"))
		.= $auc->slice(($n+$nl-1).":$n");

	($tmp = $this->{AutoSliceUsed}->slice("-1:$nl"))
		.= $auc->slice(($n+$nl-1).":$n");

	inner($autocinv->xchg(0,1),$this->{AutoSliceUsed},(my $tdw=PDL->null));

	$this->{AutoCov} = $autocov;
	$this->{AutoCovInv} = $autocinv;
	$this->{Weights} = $tdw;
}

sub predict ($$) {
	my($this,$data) = @_;
	my $nl = $this->{NLags};
	my $nl1 = $nl - 1;

	my $ldata = $data->lags(0,$this->{LagInterval},$this->{NTotLags}+1);
	print "PREDICT, weights: $this->{Weights}\n";

	inner($ldata->xchg(0,1)->slice("-$nl:-1"),
	      $this->{Weights}->slice("-$nl:-1"),
	  (my $pred1=PDL->null));
	inner($ldata->xchg(0,1)->slice("0:$nl1"),
	      $this->{Weights}->slice("0:$nl1"),
	  (my $pred2=PDL->null));

	my $pred = $pred1 + $pred2;

	return wantarray ?
	   ($pred,$ldata->slice(":,(".($nl+$this->{LagsBehind}).")"),
	    $pred1, $pred2) :
		$pred ;
}

package PDL::Filter::LinPred;
use PDL;
use PDL::Basic;
use PDL::Slatec;
use PDL::Slices;
use PDL::Primitive;
use strict;

sub _ntotlags {
	my($this) = @_;
	return $this->{NLags} + $this->{LagsBehind} + 1;
}

# Create the autocovariance matrix in Toeplitz form

# FILTER
sub _mk_mat {
	my($this) = @_;

	local $PDL::Debug = 1;

	my $n = $this->{LagsBehind};
	my $nl = $this->{NLags};
	my $nl1 = $nl-1;

	my $auc = $this->{AutoCor};

	print "AUTOCOR: $auc\n";

	my $sal = $this->{SymAutoCor}->lags(0,1,$this->{NLags})->px;

	my $autocov =
		$sal->slice(($this->{LagsBehind}-1).":".(-1-($this->{LagsBehind}+1)))
		 ->copy()->px;
	$this->{AutoCov} = $autocov;

	$| = 1;
	print "AUTOCOV: \n\n\n";
	$autocov->dump;

	print "FOOBAR\n";

# Invert it
	my $autocinv = inv($autocov);

	$this->{AutoSliceUsed} = $auc->slice("$n:-1");
	inner($autocinv->xchg(0,1),$this->{AutoSliceUsed},(my $tdw=PDL->null));

	$this->{AutoCov} = $autocov;
	$this->{AutoCovInv} = $autocinv;
	$this->{Weights} = $tdw;
}

sub chkdefault ($$) {
  my ($var,$def);
  return $def if !ref $var && $var == 0;
  return defined $var ? $var : $def;
}

sub new ($$) {
	my($type,$pars) = @_;
	my $this = bless {},$type;
	$this->{NLags} = chkdefault(delete $pars->{NLags}, 2);
	$this->{LagInterval} = chkdefault(delete $pars->{LagInterval}, 1);
	$this->{LagsBehind} = chkdefault(delete $pars->{LagsBehind}, 1);
	$this->{Smooth} = (delete $pars->{Smooth});

	$this->{NDeleted} = $this->{LagInterval} * ($this->{NLags} +
				$this->{LagsBehind}) - 1;
	$this->{NTotLags} = $this->_ntotlags();
	(my $data = delete $pars->{Data}) ;
	my ($auc,$auc1);
	if(defined $data) {
		my $atmp;
		my $n = $this->{NTotLags};
		my $da = avg($data);
# Compute autocovariance
		my $ldata = $data->lags(0,$this->{LagInterval},$n);
# XXX This takes too much space.. define a special function.
		inner($ldata->slice(":,0"),$ldata, ($atmp=PDL->null));
		sumover($atmp->xchg(0,1),($auc=PDL->null));
		$auc /= $ldata->getdim(0) * $data->getdim(1);
		$auc -= $da ** 2;
#		print "AUC: $auc\n";
	} elsif(defined ($auc1 = delete $pars->{AutoCovar})) {
		if($this->{LagInterval} != 1) {
			$auc = $auc1->slice("0:$this->{LagInterval}:-1");
		} else {
			$auc = $auc1;
		}
	} else {
		barf "Nothing to compute autocovariance from!";
	}
	$this->{AutoCor} = $auc;
	my $n = $this->{NTotLags};
	$this->{SymAutoCor} =
		(PDL->zeroes($n * 2 - 1)->float);
	my $tmp;
	($tmp = $this->{SymAutoCor}->slice("0:".($n-2)))  .=
		$auc->slice("-1:1");
	($tmp = $this->{SymAutoCor}->slice(($n-1).":-1")) .=
		$auc->slice("0:-1");
	$this->_mk_mat();
	$this;
}

sub predict ($$) {
	my($this,$data) = @_;
	my $ldata = $data->lags(0,$this->{LagInterval},$this->{NTotLags});
	print "PREDICT, weights: $this->{Weights}\n";
	inner($ldata->xchg(0,1)->slice("$this->{LagsBehind}:-1"),
	      $this->{Weights},
	  (my $pred=PDL->null));
	return wantarray ?  ($pred,$ldata->slice(":,(0)")) :
		$pred ;
}

