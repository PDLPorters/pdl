use ExtUtils::MakeMaker;

@subdirs = ("PGPLOT", "IIS", "Karma");

if($PDL_CONFIG{WITH_3D}) {
	print "\t Building TriD extension. Turn off WITH_3D if there are problems\n";
	push @subdirs,"TriD";
} else {
	print "\t Not building TriD. You should try it, though.\n";
}

WriteMakefile(
	'NAME' => 'PDL',
	VERSION_FROM => '../Basic/Core/Version.pm',
	DIR => [@subdirs]
);
