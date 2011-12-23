# Makefile.PL -- Makefile template for PodParser.
#
# This file is part of "PodParser". PodParser is free software;
# you can redistribute it and/or modify it under the same terms
# as Perl itself.

use ExtUtils::MakeMaker;

WriteMakefile(
    NAME         => "PDL::Pod::Parser",
    VERSION_FROM => "Parser.pm",
    INSTALLSCRIPT => '$(INST_LIBDIR)/PDL/',
    PM     => {
	 map {$_ => '$(INST_LIBDIR)/'.$_}
	 	<*.pm>
	},
);
