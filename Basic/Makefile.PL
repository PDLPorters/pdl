use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pm_names = qw ( PDL.pm Lite.pm LiteF.pm AutoLoader.pm Options.pm
		    Matrix.pm Reduce.pm Lvalue.pm Constants.pm);

my %pm = map { my $h = '$(INST_LIBDIR)/';
	       $h .= 'PDL/' if $_ !~ /PDL.pm$/;
	       ( $_, $h . $_ );
	   } ( @pm_names, 'default.perldlrc' );

my %man3pods = map { my $h = '$(INST_MAN3DIR)/';
		     $h .= 'PDL::' if $_ !~ /PDL.pm$/;
		     ( $_, $h . substr($_,0,length($_)-3) . '.$(MAN3EXT)' );
		 } @pm_names;

WriteMakefile(
  NAME	=> 'PDL',
  VERSION_FROM => 'PDL.pm',
  PM       => \%pm,
  MAN3PODS => \%man3pods,
  DIR      => [qw(
    Gen Core Bad Ops Ufunc Primitive Slices Math MatrixOps
    IO-FITS IO-FastRaw IO-FlexRaw IO-Misc IO-Pnm IO-Storable
    Lib-Compression Lib-FFT Lib-ImageND Lib-ImageRGB
    SourceFilter Pod Demos Doc
  )],
  NO_MYMETA => 1,
);

# modify clean method not to delete files named 'core'
# (required for MacOSX, where "Core" and "core" are
# indistinguishable)
package MY; # so that "SUPER" works right

sub clean {
  my $inherited = shift->SUPER::clean(@_);
  $inherited =~ s/\s+core\s/ /;
  # print STDERR "processed list :\n$inherited\n";
  $inherited;
}
