
use ExtUtils::MakeMaker;
WriteMakefile(
		NAME	=> 'PDL::SFilter',
		VERSION_FROM => 'SFilter.pm',
		PREREQ_PM => { Filter::Simple => 0, 
			       Text::Balanced => 0,
			     },
	     );

sub MY::postamble {
"
README: README.head SFilter.pm
\tcp README.head README
\tpod2text SFilter.pm >> README

";
}
