use ExtUtils::MakeMaker;

WriteMakefile(
	NAME => "PDL::Graphics::TriD",
     'VERSION_FROM' => '../../Basic/Core/Version.pm',
	DIR => [qw/OpenGL OpenGLQ Rout VRML/],
);
