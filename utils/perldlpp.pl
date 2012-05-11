#!/usr/bin/perl
#
use PDL::NiceSlice;

my $prefile = "";

{
   local $/;
   $prefile = <>;
}

my ($postfile) = &PDL::NiceSlice::perldlpp("PDL::NiceSlice", $prefile);

print $postfile;

__END__

=head2 perldlpp.pl

=for ref

Script to filter PDL::NiceSlice constructs from argument file to STDOUT

=for usage

  perldlpp.pl file-w-niceslice.pm > file-no-niceslice.pm       ( unix systems)
  perl perldlpp.pl file-w-niceslice.pm > file-no-niceslice.pm  (win32 systems)

C<perldlpp.pl> is a preprocessor script for perl module files
to filter and translate the PDL::NiceSlice constructs.  The name of
the file(s) to be filtered is given as argument to the command and the
result of the source filtering is output to STDOUT.

One use for this script is to preprocess the .pm files installed for
PDL to remove the requirement for PDL::NiceSlice filtering in the
core PDL modules.  This allows PDL to be used with environments such
as C<perlapp> that are not compatible with source code filters.

It is planned to add C<Makefile> support for this filter to the PDL
configure, build, and install process.

=for example

  # For example (using the unix shell):
  mkdir fixed

  # filter all pm files in this directory into fixed/
  for pm in *.pm ; do perldlpp.pl $pm > fixed/$pm ; done

  Now the fixed/*.pm files have been PDL::NiceSlice processed
  and could be used to replace the original input files as
  "clean" (no source filter) versions.

=cut

1;
