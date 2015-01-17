use strict;
use warnings;
use ExtUtils::MakeMaker;

my @subdirs = qw(PGPLOT LUT IIS Limits);

# we try and build unless WITH_3D == 0
my $t = $PDL::Config{WITH_3D};
if ( defined($t) and not $t ) {
   print "   WITH_3D:  WITH_3D => 0, not building TriD or OpenGL.  Set WITH_3D => 1 if this is incorrect.\n";
} elsif ( $PDL::Config{USE_POGL} ) {
   print "   WITH_3D:  USE_POGL => 1, will build TriD using OpenGL.\n";
   $PDL::Config{WITH_3D} = 1;
   unshift @subdirs,"TriD";
} else {
   print "   WITH_3D:  USE_POGL => 0, setting WITH_3D => 0.  Will not build TriD graphics.\n";
   $PDL::Config{WITH_3D}=0; # don't build TriD if no POGL
}

my @pm_names = qw (Graphics2D.pm State.pm);
my %pm = map { my $h = '$(INST_LIBDIR)/';
               $h .= 'PDL/' if $_ !~ /PDL.pm$/;
               $h .= 'Graphics/' if $_ =~ /State.pm$/;
               ( $_, $h . $_ );
             } ( @pm_names);
my %man3pods = map { my $h = '$(INST_MAN3DIR)/';
                     $h .= 'PDL::' if $_ !~ /PDL.pm$/;
                     ( $_, $h . substr($_,0,length($_)-3) . '.$(MAN3EXT)' ); 
                   } @pm_names;
WriteMakefile(
               'NAME'         => 'PDL',
               'VERSION_FROM' => '../Basic/Core/Version.pm',
               'PM'           => \%pm,
               'MAN3PODS'     => \%man3pods,
               'DIR'          => [@subdirs],
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
