use ExtUtils::MakeMaker;
PDL::Core::Dev->import(); # for trylink

@subdirs = qw(PGPLOT LUT IIS PLplot Limits);

# we try and build unless WITH_3D == 0
$t = $PDL::Config{WITH_3D};
if ( defined($t) and not $t ) {
   print "\n   WITH_3D:  WITH_3D => 0, not building TriD or OpenGL.  Set WITH_3D => 1 if this is incorrect.\n\n";
} elsif ( $PDL::Config{USE_POGL} ) {
   print "\n   WITH_3D:  USE_POGL => 1, will build TriD using OpenGL.\n\n";
   $PDL::Config{WITH_3D} = 1;
   unshift @subdirs,"TriD";
} else {
   print "\n   WITH_3D:  USE_POGL => 0, setting WITH_3D => 0.  Will not build TriD graphics.\n\n";
   $PDL::Config{WITH_3D}=0; # don't build TriD if no POGL
}

#WriteMakefile(
#     'NAME' => 'PDL',
#     VERSION_FROM => '../Basic/Core/Version.pm',
#     DIR => [@subdirs]
#);
my @pm_names = qw (Graphics2D.pm State.pm);

my %pm = map { $h = '$(INST_LIBDIR)/';
               $h .= 'PDL/' if $_ !~ /PDL.pm$/;
               $h .= 'Graphics/' if $_ =~ /State.pm$/;
               ( $_, $h . $_ );
             } ( @pm_names);

my %man3pods = map { $h = '$(INST_MAN3DIR)/';
                     $h .= 'PDL::' if $_ !~ /PDL.pm$/;
                     ( $_, $h . substr($_,0,length($_)-3) . '.$(MAN3EXT)' ); 
                   } @pm_names;

# Write makefile
WriteMakefile(
               'NAME'         => 'PDL',
               'VERSION_FROM' => '../Basic/Core/Version.pm',
               'PM'           => \%pm,
               'MAN3PODS'     => \%man3pods,
               'DIR'          => [@subdirs],
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
