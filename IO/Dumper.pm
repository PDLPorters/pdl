=head1 NAME

PDL::IO::Dumper -- data dumping for structs with PDLs

=head1 DESCRIPTION

This package allows you cleanly to save and restore complex data structures
which include PDLs, as ASCII strings and/or transportable ASCII files.

PDL::IO::Dumper traverses the same types of structure that Data::Dumper
knows about, because it uses a call to Data::Dumper.  Unlike Data::Dumper
it doesn't crash when accessing PDLs.

PDLs are stored as FITS files that are uuencoded and included in the
dump string.  (You have to have the FITSIO module and uuencode(1) to
make this work).

=head1 Bugs

Using rfits in an eval (which is how you get back dumped variables) 
exercises a bug in perl or PDL that causes coredumps if you include 
PDL::NiceSlice explicitly in your .perldlrc.  It seems to work fine 
with late-model PDL distributions and no explicit 'use PDL::NiceSlice'.  
(late-model distributions use NiceSlice automagically).

It's still possible to break this code and cause it to dump core, for
the same reason that Data::Dumper crashes.  In particular, other
external-hook variables aren't recognized (for that a more universal
Dumper would be needed) and will still exercise the Data::Dumper crash.  
Because most objects are actually safe, PDL::IO::Dumper attempts to 
dump & restore everything, even objects it desn't recognize.

=head1 Author, copyright, no warranty

Copyright 2002, Craig DeForest.

This code may be distributed under the same terms as Perl itself
(license available at http://ww.perl.org).  Copying, reverse
engineering, distribution, and modification are explicitly allowed so
long as this notice is preserved intact and modified versions are
clearly marked as such.

This package comes with NO WARRANTY.

=head1 FUNCTIONS

=cut

package PDL::IO::Dumper;
$VERSION = 1.0;
 
 @EXPORT_OK = qw( fdump sdump frestore );
 %EXPORT_TAGS = (Func=>[@EXPORT_OK]);
 
 use PDL;
 use PDL::Exporter;
 use Data::Dumper;
 use Carp;


######################################################################

=head2 sdump -- dump a data structure to a string

=for usage

Usage: 

  use PDL::IO::Dumper; 

  $s = sdump(<VAR>);

  ...

  <VAR> = eval $s;

=for description

sdump dumps a single complex data structure into a string.  You restore
the data structure by eval'ing the string.  Since eval is a builtin, no
convenience routine exists to use it.

=cut

sub PDL::IO::Dumper::sdump {
# Make an initial dump...
  my($s) = Data::Dumper->Dump([@_]);
  print "ok....\n";
# Find the bless(...,'PDL') lines
  while($s =~ s/bless\( do\{\\\(my \$o \= (\d+)\)\}\, \'PDL\' \)/\$PDL_$1/) {
    $pdls{$1}=1;
  }
  
  return "{my(\$VAR1);\n".&PDL::IO::Dumper::find_PDLs(@_).$s."\n}\n";
#
}
*main::sdump = *PDL::IO::Dumper::sdump;

######################################################################

=head2 fdump -- dump a data structure to a file

=for usage

Usage: 

  use PDL::IO::Dumper;

  fdump(<VAR>,$filename);

  ...

  <VAR> = frestore($filename);

=for description
 
fdump dumps a single complex data structure to a file.  You restore the
data structure by eval'ing the perl code put in the file.  A convenience
routine (frestore) exists to do it for you.

=cut
 
sub PDL::IO::Dumper::fdump { 
  my($struct,$file) = @_;
  if(!open(FDuMPFILE,">$file")) {
    Carp::cluck ("fdump: couldn't open '$file'\n");
     return undef;
  }
  print FDuMPFILE sdump($struct);
  close FDuMPFILE;
  return $struct;
}
*main::fdump = *PDL::IO::Dumper::fdump;

######################################################################

=head2 frestore -- restore a dumped file

=for usage

Usage:

  use PDL::IO::Dumper;

  fdump(<VAR>,$filename);

  ...

  <VAR> = frestore($filename);

=for description

frestore() is a convenience function that just reads in the named
file and executes it in an eval.  It's paired with fdump().

=cut

sub frestore {
  local($_);
  my($fname) = shift;

  if(!open(fREsTOrE,"<$fname")){
    Carp::cluck("frestore:  couldn't open '$file'\n");
      return undef;
    }

  my($file) = join("",<fREsTOrE>);
  eval $file;
}
      
######################################################################
# find_PDLs: finds references to PDLs in the Data::Dumper output
# and inserts a hardcoded fits restoral at the top, for each one.
sub PDL::IO::Dumper::find_PDLs {
  local($_);
  my($out);

  findpdl:foreach $_(@_) {
    next findpdl unless ref($_);

    if(UNIVERSAL::isa($_,'ARRAY')) {
      my($a);
      foreach $a(@{$_}) {
	$out .= find_PDLs($a);
      }
    } 
    elsif(UNIVERSAL::isa($_,'HASH')) {
      my($a);
      foreach $a(values %{$_}) {
	$out .= find_PDLs($a)
	}
    } elsif(UNIVERSAL::isa($_,'PDL')) {
      # This gets subclasses of PDL, but NOT magic-hash subclasses of
      # PDL (because they'd be gotten by the previous clause).
      # So if you subclass PDL but your actual data structure is still
      # just a straight PDL (and not a hash with PDL field), you end up here.
      #
      # Write the PDL to a fits file, uuencode it, and snarf it up.
      my($pdlid) = $$_;

      my($fname) = "/tmp/$$.fits";
      wfits($_,$fname);

      open(FITSFILE,"uuencode $fname $fname |");
      my(@uulines) = <FITSFILE>;
      
      # clean up /tmp
      unlink $fname;
      
      $out .= join("",
		   ("open DuMPERFILE,'|uudecode'; print DuMPERFILE <<'blat'\n",
		    @uulines,
		    "blat\n;\n",
		    "close DuMPERFILE;\n",
		    "my(\$PDL_$pdlid) = rfits('$fname'); unlink '$fname';\n"
		    ));
    }
    elsif(UNIVERSAL::isa($_,'SCALAR')) {
      # This gets other kinds of refs -- PDLs have already been gotten.
      # Naked PDLs are themselves SCALARs, so the SCALAR case has to come 
      # last to let the PDL case run.
      $out .= find_PDLs( ${$_} );
    }

  }
  return $out;
}
      
1;
