
# Makefile.PL for PDL::Image2D module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

@pack = (["image2d.pd",Image2D,PDL::Image2D]);

%hash = pdlpp_stdargs_int( @pack );
$hash{LIBS} = [ '-lm' ];

# On windows we do not have an rint function (at least on VC++)
# Should do a proper test for rint similar to that done for
# PDL::Math. For now, simply test architecture
if ($^O =~ /MSWin/i) {
  $hash{DEFINE} = " -DNEEDS_RINT";
}

# what code do we want compiled and linked in?
#   rotate.c is included directly into image2d.pd
#
#   for $file ( qw( rotate resample ) ) {
for $file ( qw( resample ) ) {
    my $n = "$file\$(OBJ_EXT)";
    $hash{OBJECT} .= " $n";
    $hash{clean}{FILES} .= " $n";
}

WriteMakefile( %hash );

# Add genpp rule
sub MY::postamble { pdlpp_postamble_int(@::pack); }  

