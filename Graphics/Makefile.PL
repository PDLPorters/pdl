use ExtUtils::MakeMaker;

@subdirs = ("PGPLOT", "LUT", "IIS", "Karma");

# we try and build unless WITH_3d == 0
$t = $PDL_CONFIG{WITH_3D};
if( defined($t) and $t == 0 ) {
	print "\n   Not building TriD. Turn on WITH_3D if this is incorrect.\n\n";
} else {
	print "\n   Building TriD extension. Turn off WITH_3D if there are problems\n\n";
	push @subdirs,"TriD";
}

#WriteMakefile(
#     'NAME' => 'PDL',
#     VERSION_FROM => '../Basic/Core/Version.pm',
#     DIR => [@subdirs]
#);
WriteMakefile(
        'NAME' => 'PDL',
        VERSION_FROM => '../Basic/Core/Version.pm',
        DIR => [@subdirs],
        LIB => ['-lm'],
);
