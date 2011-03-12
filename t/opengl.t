# -*-perl-*-
BEGIN{
	  # Set perl to not try to resolve all symbols at startup
	  #   The default behavior causes some problems because 
	  #    opengl.pd builds an interface for all functions
	  #    defined in gl.h and glu.h even though they might not
	  #    actually be in the opengl libraries.
	  $ENV{'PERL_DL_NONLAZY'}=0;
}

# use PDL::Graphics::OpenGL;

sub hasDISPLAY {
  return defined $ENV{DISPLAY} && $ENV{DISPLAY} !~ /^\s*$/;
}

use Test::More;

BEGIN { 
   use PDL::Config;
   if ( $PDL::Config{WITH_3D} ) {  # check if compiled
      if ( $PDL::Config{USE_POGL} ) {  # check if using Perl OpenGL
         if ( hasDISPLAY or exists($ENV{'PDL_INT'}) ) {
            plan tests => 4;
            use_ok("OpenGL $PDL::Config{POGL_VERSION}", qw(:all));
            use_ok('PDL::Graphics::OpenGL::Perl::OpenGL');
         } else {  # no DISPLAY
            plan tests => 2;
            use_ok("OpenGL $PDL::Config{POGL_VERSION}", qw(:all));
            use_ok('PDL::Graphics::OpenGL::Perl::OpenGL');
            exit;
         }
      } else {
         plan skip_all => 'Non-POGL TriD graphics not supported';
      }
   } else {
      plan skip_all => 'TriD graphics not compiled';
   }
}

#
# Try opening 2 GL windows
#

SKIP: {

   if ( hasDISPLAY and OpenGL::_have_glx ) {
      eval  { OpenGL::glpDisplay($ENV{DISPLAY}) };
      skip "can't open X display", 2 if $@;
   }

   my $numwins = 2;
   my @windows;
   my $opt;
   $opt->{width} = 90;
   $opt->{height} = 90;

   foreach(0..$numwins-1){
      $opt->{x} = ($numwins % 10) *100;
      $opt->{y} = int($numwins / 10) *100;
      my $win=new PDL::Graphics::OpenGL::OO($opt);
      isa_ok($win, 'PDL::Graphics::OpenGL::OO');
      push @windows, $win;
   }
}
