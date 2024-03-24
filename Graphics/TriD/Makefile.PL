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
  VERSION_FROM => '../../Basic/PDL.pm',
  NO_MYMETA => 1,
);
