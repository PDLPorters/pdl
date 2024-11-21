package PDL::Demos::GSL_RNG;

sub info {('gsl_rng', 'GSL randomness functions (Req.: PDL::Graphics::Simple)')}

my @demo = (
[act => q|
# This demo illustrates the PDL::GSL::RNG module.
# It shows the power of PDL with a concise way to generate graphs of
# different random number distributions.
use PDL::Graphics::Simple;
use PDL::GSL::RNG;
$x = zeroes(100)->xlinvals(-5,5);
$w = pgswin();
# Exponential Power Distribution
$w->plot(
  with=>'lines', key=>'a=1 b=2.5', $x, ran_exppow_pdf($x, 1, 2.5),
  with=>'lines', key=>'a=1 b=0.5', $x, ran_exppow_pdf($x, 1, 0.5),
  {le=>'tr', yrange=>[0,0.8], title=>'Exponential Power Distribution',
    xlabel=>'x', ylabel=>'p(x)'}
);
|],

[act => q|
# Cauchy Distribution
$w->plot(
  with=>'lines', key=>'a=1', $x, ran_cauchy_pdf($x, 1),
  with=>'lines', key=>'a=2', $x, ran_cauchy_pdf($x, 2),
  {le=>'tr', yrange=>[0,0.4], title=>'Cauchy Distribution',
    xlabel=>'x', ylabel=>'p(x)'}
);
|],

[act => q|
# Rayleigh Tail Distribution
$x = zeroes(100)->xlinvals(0,5);
$w->plot(
  with=>'lines', key=>'a=1 sigma=1', $x, ran_rayleigh_tail_pdf($x, 1, 1),
  with=>'lines', key=>'a=0.5 sigma=2', $x, ran_rayleigh_tail_pdf($x, 0.5, 2),
  {le=>'tr', yrange=>[0,1.1], title=>'Rayleigh Tail Distribution',
    xlabel=>'x', ylabel=>'p(x)'}
);
|],

[act => q|
# Gamma Distribution
$x = zeroes(100)->xlinvals(0,5);
$w->plot(
  with=>'lines', key=>'a=1 b=1', $x, ran_gamma_pdf($x, 1, 1),
  with=>'lines', key=>'a=2 b=1', $x, ran_gamma_pdf($x, 2, 1),
  with=>'lines', key=>'a=3 b=1', $x, ran_gamma_pdf($x, 3, 1),
  {le=>'tr', yrange=>[0,1], title=>'Gamma Distribution',
    xlabel=>'x', ylabel=>'p(x)'}
);
|],

[act => q|
# Bivariate Gaussian Distribution
# inspired by https://www.perlmonks.org/?node_id=11104262
$points = pdl '[219 88 2.7; 38 95 1.7; 45 268 0.8]';
($XSIZE, $YSIZE) = (300, 300);
($xcoord, $ycoord, $weight) = $points       # xyw nweights
  ->slice(",*$XSIZE,*$YSIZE,")              # xyw nx ny nweights
  ->using(0..2);                            # nx ny nweights
$xbase = xvals($XSIZE)->slice(",*$YSIZE");  # nx ny
$ybase = xvals($YSIZE)->slice("*$XSIZE,");  # nx ny
for (1..90) {
  $h = (
    $weight * ran_bivariate_gaussian_pdf(
      $xcoord-$xbase, $ycoord-$ybase, $_, $_, 0
    )                                           # nx ny nweights
  )->mv(-1,0)->sumover;                         # nx ny
  $w->plot(with=>'image', $h, {title=>'Bivariate Gaussian Distribution',j=>1});
}
|],

[act => q|
# Same, but with a colourful heatmap (if you have the right libraries)
sub as_heatmap {
  my ($d) = @_;
  my $max = $d->max;
  die "as_heatmap: can't work if max == 0" if $max == 0;
  $d /= $max; # negative OK
  my $hue   = (1 - $d)*240;
  $d = cat($hue, pdl(1), pdl(1));
  (hsv_to_rgb($d->mv(-1,0)) * 255)->byte->mv(0,-1);
}
if (eval 'use PDL::Graphics::ColorSpace; 1') {
  for (1..90) {
    $h = (
      $weight * ran_bivariate_gaussian_pdf(
        $xcoord-$xbase, $ycoord-$ybase, $_, $_, 0
      )                                           # nx ny nweights
    )->mv(-1,0)->sumover;                         # nx ny
    $w->plot(
      with=>'image', as_heatmap($h),
      {title=>'Bivariate Gaussian Distribution (heatmap)',j=>1}
    );
  }
}
|],

[comment => q|
See https://www.gnu.org/software/gsl/doc/html/randist.html for more.
|],
);

sub demo { @demo }
sub done {'
  undef $w;
'}

1;
