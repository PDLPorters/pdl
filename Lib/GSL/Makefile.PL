use ExtUtils::MakeMaker;

# the real stuff happens in the subdirs

WriteMakefile(
     'NAME'	=> 'PDL::GSL',
);

sub MY::postamble {}
