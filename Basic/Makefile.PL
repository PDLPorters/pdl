use strict;
use warnings;
use ExtUtils::MakeMaker;

my $defstartup = 'default.perldlrc';
if ($^O =~ /win32/i) {
  $defstartup = 'default.pdl';
  system("copy default.perldlrc $defstartup");
}

my @pm_names = qw ( PDL.pm Lite.pm LiteF.pm AutoLoader.pm Options.pm
		    Matrix.pm Reduce.pm Lvalue.pm Constants.pm);

my %pm = map { my $h = '$(INST_LIBDIR)/';
	       $h .= 'PDL/' if $_ !~ /PDL.pm$/;
	       ( $_, $h . $_ );
	   } ( @pm_names, $defstartup );

my %man3pods = map { my $h = '$(INST_MAN3DIR)/';
		     $h .= 'PDL::' if $_ !~ /PDL.pm$/;
		     ( $_, $h . substr($_,0,length($_)-3) . '.$(MAN3EXT)' );
		 } @pm_names;

WriteMakefile(
	      'NAME'	=> 'PDL',
	      'VERSION_FROM' => 'PDL.pm',
	      'PM'       => \%pm,
	      'MAN3PODS' => \%man3pods,
     'DIR'      => ['Pod','Gen','SourceFilter','Core','Bad','Ops','Ufunc',
		    'Primitive','Slices','Math','MatrixOps'],
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
