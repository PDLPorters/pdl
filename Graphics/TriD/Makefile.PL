use strict;
use warnings;
use ExtUtils::MakeMaker;

if (
  !eval { require OpenGL; OpenGL->VERSION(0.70); 1; } or
  !eval { require OpenGL::GLUT; OpenGL::GLUT->VERSION(0.72); 1; }
) {
  write_dummy_make("Will not build TriD graphics, need OpenGL >=0.70 and OpenGL::GLUT >=0.72\n");
  return;
}

WriteMakefile (
  NAME         => "PDL::Graphics::TriD",
  VERSION_FROM => 'TriD.pm',
  NO_MYMETA => 1,
  PM => {
    'TriD.pm' => '$(INST_LIBDIR)/TriD.pm',
    (map +($_ => '$(INST_LIBDIR)/'.$_), glob 'TriD/*.pm'),
    'DemoTriD1.pm' => '$(INST_LIB)/PDL/Demos/TriD1.pm',
    'DemoTriD2.pm' => '$(INST_LIB)/PDL/Demos/TriD2.pm',
    'DemoTriDGallery.pm' => '$(INST_LIB)/PDL/Demos/TriDGallery.pm',
  },
);
