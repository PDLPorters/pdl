package Alien::Proj4;

use strict;
use warnings;
use Config;
use Devel::CheckLib;

my $find_libs = [ "libproj.$Config{dlext}", "libproj$Config{lib_ext}" ];
my @NEEDED = qw(projects.h proj_api.h);
my @DEFAULT_LIB = (
  '/usr/lib64',
  '/usr/local/lib64',
  '/lib64',
  '/usr/lib',
  '/usr/local/lib',
  '/lib',
  split(/ /, $Config{libpth}),
);
my @DEFAULT_INC = (
  '/usr/include',
  '/usr/local/include',
  $Config{usrinc},
);
my @lib_locations = @DEFAULT_LIB;
my @inc_locations = @DEFAULT_INC;

sub import {
  my ($class, $lib, $inc) = @_;
  @lib_locations = @$lib if $lib and @$lib;
  @inc_locations = @$inc if $inc and @$inc;
}

sub default_lib {
  my ($class) = @_;
  @DEFAULT_LIB;
}

sub default_inc {
  my ($class) = @_;
  @DEFAULT_INC;
}

sub libdir {
  my ($class) = @_;
  foreach my $libdir ( @lib_locations ) {
    foreach my $find_lib ( @$find_libs ) {
      next unless -e "$libdir/$find_lib";
      return $libdir;
    }
  }
}

sub libflags {
  my ($class) = @_;
  my $lib_path = $class->libdir;
  my $libflags = qq{"-L$lib_path" -lproj -lm};
  $libflags;
}

sub incdir {
  my ($class) = @_;
  my %dir2true;
  my %stillneeded = map { ($_=>1) } @NEEDED;
  my @inc; # array because need to keep ordering
  foreach my $incdir ( @inc_locations ) {
    foreach my $find_inc ( keys %stillneeded ) {
      next unless -e "$incdir/$find_inc";
      push @inc, $incdir unless $dir2true{$incdir};
      $dir2true{$incdir} = 1;
      delete $stillneeded{$find_inc};
    }
  }
  @inc;
}

sub incflags {
  my ($class) = @_;
  join ' ', map qq{"-I$_"}, $class->incdir;
}

sub installed {
  my ($class) = @_;
  return 0 unless my $lib_path = $class->libdir;
  return 0 unless my @incdirs = $class->incdir;
  return 0 unless check_lib(
    function=>'projPJ mypj = pj_init_plus("+proj=eqc +lon_0=0 +datum=WGS84"); if (! mypj) return 1; else return 0;',
    header=>'proj_api.h',
    incpath=>\@incdirs,
    lib=>'proj',
    libpath=>$lib_path,
  );
  return 1;
}

# dup of code currently in PDL::GIS::Proj
sub load_projection_descriptions {
  my ($class) = @_;
  my $libflags = $class->libflags;
  my $incflags = $class->incflags;
  require Inline;
  Inline->bind(C => <<'EOF', inc => $incflags, libs => $libflags) unless defined &list_projections;
#include "projects.h"
HV *list_projections() {
  struct PJ_LIST *lp;
  SV* scalar_val;
  HV *hv = newHV();
  for (lp = pj_get_list_ref() ; lp->id ; ++lp) {
      scalar_val  = newSVpv( *lp->descr, 0 );
      hv_store( hv, lp->id, strlen( lp->id ), scalar_val, 0 );
  }
  return hv;
}
EOF
  list_projections();
}

# dup of code currently in PDL::GIS::Proj
sub load_projection_information {
    my ($class) = @_;
    my $descriptions = $class->load_projection_descriptions();
    my $info = {};
    foreach my $projection ( keys %$descriptions ) {
        my $description = $descriptions->{$projection};
        my $hash = {};
        $hash->{CODE} = $projection;
        my @lines = split( /\n/, $description );
        chomp @lines;
        # Full name of this projection:
        $hash->{NAME} = $lines[0];
        # The second line is usually a list of projection types this one is:
        my $temp = $lines[1];
        $temp =~ s/no inv\.*,*//;
        $temp =~ s/or//;
        my @temp_types = split(/[,&\s]/, $temp );
        my @types = grep( /.+/, @temp_types );
        $hash->{CATEGORIES} = \@types;
        # If there's more than 2 lines, then it usually is a listing of parameters:
        # General parameters for all projections:
        $hash->{PARAMS}->{GENERAL} =
            [ qw( x_0 y_0 lon_0 units init no_defs geoc over ) ];
        # Earth Figure Parameters:
        $hash->{PARAMS}->{EARTH} =
            [ qw( ellps b f rf e es R R_A R_V R_a R_g R_h R_lat_g ) ];
        # Projection Specific Parameters:
        my @proj_params = ();
        if( $#lines >= 2 ) {
            foreach my $i ( 2 .. $#lines ) {
                my $text = $lines[$i];
                my @temp2 = split( /\s+/, $text );
                my @params = grep( /.+/, @temp2 );
                foreach my $param (@params) {
                    $param =~ s/=//;
                    $param =~ s/[,\[\]]//sg;
                    next if $param =~ /^and|or|Special|for|Madagascar|fixed|Earth|For|CH1903$/;
                    push(@proj_params, $param);
                }
            }
        }
        $hash->{PARAMS}->{PROJ} = \@proj_params;
        # Can this projection do inverse?
        $hash->{INVERSE} = ( $description =~ /no inv/ ) ? 0 : 1;
        $info->{$projection} = $hash;
    }
    # A couple of overrides:
    $info->{ob_tran}->{PARAMS}->{PROJ} =
        [ 'o_proj', 'o_lat_p', 'o_lon_p', 'o_alpha', 'o_lon_c',
          'o_lat_c', 'o_lon_1', 'o_lat_1', 'o_lon_2', 'o_lat_2' ];
    $info->{nzmg}->{CATEGORIES} = [ 'fixed Earth' ];
    return $info;
}

1;

__END__

=head1 NAME

Alien::Proj4 - Give install info for already-installed proj4

=head1 SYNOPSIS

In Makefile.PL:

  use Alien::Proj4 [ 'overridelibdirs' ], [ 'overrideincdirs' ];
  my $proj4_installed = Alien::Proj4->installed;
  my $proj4_lib = Alien::Proj4->libflags;
  my $proj4_inc = Alien::Proj4->incflags;

In a module like L<PDL::Transform::Proj4> that wants available proj4
projections:

  my @projections = Alien::Proj4->projections;

=head1 DESCRIPTION

If no override is given for the library or include directories, the
defaults are used. The projections are listed using L<Inline::C>, so
that needs to be installed.

An alternative idiom to the above compile-time example of supplying
overrides for directories is:

  use Alien::Proj4;
  Alien::Proj4->import(\@libdirs, undef) if @libdirs; # set to different
  Alien::Proj4->import(undef, [ Alien::Proj4->default_inc, @incdirs ]); # add

since the overrides have no effect if C<undef> is supplied as the
array-ref, OR the array is empty.

Note as above that there are C<default_inc> and C<default_lib> methods
to get the defaults.

Currently the C<incflags> method includes a separate search for
F<proj_api.h> and F<projects.h>. This is so that if (as in C<PJ_VERSION
= 480>) there is no F<projects.h> supplied, you can provide one and add
the location to the list of directories searched, using import as above.

=head1 AUTHOR

Ed J
