use strict;
use warnings;
use ExtUtils::MakeMaker;

# NOTE:
#   we hijack m51.fits from the parent directory and 
#   install that into the demos directory
WriteMakefile(
     'NAME'	=> 'PDL::Demos',
     'VERSION_FROM' => '../Basic/PDL.pm',
     'PM'       => {'General.pm'     => '$(INST_LIBDIR)/Demos/General.pm',
                    'Demos.pm'    => '$(INST_LIBDIR)/Demos.pm',
                    'TriD1.pm'    => '$(INST_LIBDIR)/Demos/TriD1.pm',
                    'TriD2.pm'    => '$(INST_LIBDIR)/Demos/TriD2.pm',
                    'TriDGallery.pm'    => '$(INST_LIBDIR)/Demos/TriDGallery.pm',
		    'PGPLOT_demo.pm'	=> '$(INST_LIBDIR)/Demos/PGPLOT_demo.pm',
		    'PGPLOT_OO_demo.pm'	=> '$(INST_LIBDIR)/Demos/PGPLOT_OO_demo.pm',
                    'BAD_demo.pm'       => '$(INST_LIBDIR)/Demos/BAD_demo.pm',
                    'Transform_demo.pm' => '$(INST_LIBDIR)/Demos/Transform_demo.pm',
                    'Cartography_demo.pm' => '$(INST_LIBDIR)/Demos/Cartography_demo.pm',
                    '../m51.fits'       => '$(INST_LIBDIR)/Demos/m51.fits',
                    'Prima.pm' => '$(INST_LIBDIR)/Demos/Prima.pm',
		   },
     NO_MYMETA => 1,
);
