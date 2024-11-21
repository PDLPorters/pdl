use strict;
use warnings;
use Test::More;
use PDL::Stats::Distr;
use PDL::LiteF;
use Test::PDL;

{
  my $a = sequence(5) / 10;
  is_pdl pdl($a->mme_beta), pdl(1.4, 5.6);
  is_pdl $a->pdf_beta(1, 3), pdl '3 2.43 1.92 1.47 1.08';
}
{
  my $a = sequence 5;
  $a %= 2;
  is_pdl pdl($a->mme_binomial), pdl(1, .4);
  is_pdl $a->pmf_binomial(2,.4), pdl '0.36 0.48 0.36 0.48 0.36';
}
{
  my $a = sequence 5;
  is_pdl pdl($a->mle_exp), pdl .5;
  is_pdl $a->pdf_exp(2.5), pdl '2.5 0.205212 0.016844 0.001382 0.000113';
}
{
  my $a = sequence 5;
  is_pdl pdl($a->mle_gaussian), pdl(2,2);
  is_pdl $a->pdf_gaussian(1,2), pdl '0.219695644733861 0.282094791773878 0.219695644733861 0.103776874355149 0.0297325723059073';
}
{
  my $a = sequence 5;
  is_pdl pdl($a->mle_geo), pdl 0.333333333333333;
  is_pdl $a->pmf_geo(.5), pdl '0.5 0.25 0.125 0.0625 0.03125';
}
{
  my $a = sequence 5;
  $a += 1;
  is_pdl pdl($a->mle_geosh), pdl 0.333333333333333;
  is_pdl $a->pmf_geosh(.5), pdl '0.5 0.25 0.125 0.0625 0.03125';
}
{
  my $a = sequence(5) + 1;
  is_pdl pdl($a->mle_lognormal), pdl(0.957498348556409, 0.323097797388514);
  is_pdl pdl($a->mme_lognormal), pdl(2.19722457733622, 0.200670695462151);
  is_pdl $a->pdf_lognormal(1,2), pdl '0.219695644733861 0.137765961149997 0.0938032750793072 0.0679412228146167 0.0514161127408299';
}
{
  my $a = sequence 5;
  $a *= $a;
  is_pdl pdl($a->mme_nbd), pdl(1.25, 0.172413793103448);
  is_pdl $a->pmf_nbd(2, .4), pdl '0.16 0.192 0.10368 0.0161243136 0.000767341894828032';
}
{
  my $a = sequence 5;
  $a += 1;
  is_pdl pdl($a->mme_pareto), pdl(1.4, 0.857142857142857);
  is_pdl $a->pdf_pareto(2, .4), pdl '0.32 0.04 0.0118518518518519 0.005 0.00256';
}
{
  my $a = sequence 5;
  $a %= 2;
  is_pdl pdl($a->mle_poisson), pdl .4;
  is_pdl $a->pmf_poisson(.4), pdl '0.670320046035639 0.268128018414256 0.670320046035639 0.268128018414256 0.670320046035639';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  $a /= 10;
  is_pdl pdl($a->mme_beta), pdl(1.4, 5.6);
  is_pdl $a->pdf_beta(1, 3), pdl '3 2.43 1.92 1.47 1.08 BAD';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  $a %= 2;
  is_pdl pdl($a->mme_binomial), pdl(1, .4);
  is_pdl $a->pmf_binomial(2,.4), pdl '0.36 0.48 0.36 0.48 0.36 BAD';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  is_pdl pdl($a->mle_exp), pdl .5;
  is_pdl $a->pdf_exp(2.5), pdl '2.5 0.205212496559747 0.0168448674977137 0.00138271092536958 0.000113499824406212 BAD';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  is_pdl pdl($a->mle_gaussian), pdl(2,2);
  is_pdl $a->pdf_gaussian(1,2), pdl '0.219695644733861 0.282094791773878 0.219695644733861 0.103776874355149 0.0297325723059073 BAD';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  is_pdl pdl($a->mle_geo), pdl 0.333333333333333;
  is_pdl $a->pmf_geo(.5), pdl '0.5 0.25 0.125 0.0625 0.03125 BAD';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  $a += 1;
  is_pdl pdl($a->mle_geosh), pdl 0.333333333333333;
  is_pdl $a->pmf_geosh(.5), pdl '0.5 0.25 0.125 0.0625 0.03125 BAD';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  $a += 1;
  is_pdl pdl($a->mle_lognormal), pdl(0.957498348556409, 0.323097797388514);
  is_pdl pdl($a->mme_lognormal), pdl(2.19722457733622, 0.200670695462151);
  is_pdl $a->pdf_lognormal(1,2), pdl '0.219695644733861 0.137765961149997 0.0938032750793072 0.0679412228146167 0.0514161127408299 BAD';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  $a *= $a;
  is_pdl pdl($a->mme_nbd), pdl(1.25, 0.172413793103448);
  is_pdl $a->pmf_nbd(2, .4), pdl '0.16 0.192 0.10368 0.0161243136 0.000767341894828032 BAD';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  $a += 1;
  is_pdl pdl($a->mme_pareto), pdl(1.4, 0.857142857142857);
  is_pdl $a->pdf_pareto(2, .4), pdl '0.32 0.04 0.0118518518518519 0.005 0.00256 BAD';
}
{
  my $a = sequence 6;
  $a->setbadat(-1);
  $a %= 2;
  is_pdl pdl($a->mle_poisson), pdl .4;
  is_pdl $a->pmf_poisson(.4), pdl '0.670320046035639 0.268128018414256 0.670320046035639 0.268128018414256 0.670320046035639 BAD';
  is_pdl $a->pmf_poisson_factorial(.4), pdl '0.670320046035639 0.268128018414256 0.670320046035639 0.268128018414256 0.670320046035639 BAD';
  is_pdl $a->pmf_poisson_stirling(.4), pdl '0.670320046035639 0.268050878476493 0.670320046035639 0.268050878476493 0.670320046035639 BAD';

  $a += 171;
  is_pdl $a->pmf_poisson_stirling(10), pdl '0 0 0 0 0 BAD';
}

done_testing;
