package PDL::Demos::GSL_CDF;

sub info {('gsl_cdf', 'GSL cumulative distribution functions')}

my @demo = (
[act => q|
# This demo illustrates the PDL::GSL::CDF module.
# It shows the power of PDL with a concise way to generate a table of
# more extreme small p-values, and the associated Z scores.
use PDL::GSL::CDF;
$pvalue = ipow(pdl(10),-(sequence(32) + 1));
$z = gsl_cdf_ugaussian_Qinv($pvalue);
$pdl = $pvalue->cat($z)->transpose;
print $pdl->splitdim(1,8)->mv(2,1)->clump(-2)->string("%4.4g");
|],

[act => q|
# And more extreme high Z scores, and the associated p-values.
$z = sequence(24) + 1;
$pvalue = gsl_cdf_ugaussian_Q($z);
$pdl = $z->cat($pvalue)->transpose;
print $pdl->splitdim(1,8)->mv(2,1)->clump(-2)->string("%4.4g");
|],
);

sub demo { @demo }

1;
