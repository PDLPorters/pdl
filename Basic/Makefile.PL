
require 'Core/Dev.pm'; PDL::Core::Dev->import();

use ExtUtils::MakeMaker;


$defstartup = 'default.perldlrc';
if ($^O =~ /win32/i) {
  $defstartup = 'default.pdl';
  system("copy default.perldlrc $defstartup");
}

# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.

my @pm_names = qw ( PDL.pm Lite.pm LiteF.pm AutoLoader.pm Options.pm
		    Matrix.pm Reduce.pm Lvalue.pm);

my %pm = map { $h = '$(INST_LIBDIR)/'; 
	       $h .= 'PDL/' if $_ !~ /PDL.pm$/;
	       ( $_, $h . $_ ); 
	   } ( @pm_names, $defstartup );

my %man3pods = map { $h = '$(INST_MAN3DIR)/';
		     $h .= 'PDL::' if $_ !~ /PDL.pm$/;
		     ( $_, $h . substr($_,0,length($_)-3) . '.$(MAN3EXT)' ); 
		 } @pm_names;

WriteMakefile(
	      'NAME'	=> 'PDL',
	      'VERSION_FROM' => 'Core/Version.pm',
	      'PM'       => \%pm,
	      'MAN3PODS' => \%man3pods,
     'DIR'      => ['Pod','Gen','SourceFilter','Core','Bad','Ops','Ufunc',
		    'Primitive','Slices','Test','Math','MatrixOps','Complex'],
);
