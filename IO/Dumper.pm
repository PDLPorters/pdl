use PDL::NiceSlice;
=head1 NAME

PDL::IO::Dumper -- data dumping for structs with PDLs

=head1 DESCRIPTION

This package allows you cleanly to save and restore complex data structures
which include PDLs, as ASCII strings and/or transportable ASCII files.  It
exports three functions into your namespace: sdump, fdump, and frestore.

PDL::IO::Dumper traverses the same types of structure that Data::Dumper
knows about, because it uses a call to Data::Dumper.  Unlike Data::Dumper
it doesn't crash when accessing PDLs.

The PDL::IO::Dumper routines have a slightly different syntax than
Data::Dumper does: you may only dump a single scalar perl expression
rather than an arbitrary one.  Of course, the scalar may be a ref to
whatever humongous pile of spaghetti you want, so that's no big loss.

The output string is intended to be about as readable as Dumper's
output is for non-PDL expressions. To that end, small PDLs (up to 8
elements) are stored as inline perl expressions, midsized PDLs (up to
200 elements) are stored as perl expressions above the main data
structure, and large PDLs are stored as FITS files that are uuencoded
and included in the dump string.  (You have to have the FITSIO module
and uuencode(1) to make this work).

No attempt is made to shrink the output string -- for example, inlined
PDL expressions all include explicit reshape() and typecast commands,
and uuencode() is notoriously inefficient.  So your data structures
will grow when you dump them.  Gzip will shrink the output by
about a factor of 10 for typical small data structures.

=head1 Bugs

Using rfits in an eval (which is how you get back dumped variables)
exercises a bug in perl or PDL that causes coredumps in pre-2002
versions of PDL if you include PDL::NiceSlice explicitly in your
.perldlrc.  It seems to work fine with late-model PDL distributions
and no explicit 'use PDL::NiceSlice'.  (late-model distributions seem
to use NiceSlice automagically).

It's still possible to break this code and cause it to dump core, for
the same reason that Data::Dumper crashes.  In particular, other
external-hook variables aren't recognized (for that a more universal
Dumper would be needed) and will still exercise the Data::Dumper crash.  
This is by choice:  (A) it's difficult to recognize which objects
are actually external, and (B) most everyday objects are quite safe.

There's currently no reference recursion detection, so a non-treelike
reference topology will cause Dumper to buzz forever.

=head1 Author, copyright, no warranty

Copyright 2002, Craig DeForest.

This code may be distributed under the same terms as Perl itself
(license available at http://ww.perl.org).  Copying, reverse
engineering, distribution, and modification are explicitly allowed so
long as this notice is preserved intact and modified versions are
clearly marked as such.

This package comes with NO WARRANTY.

=head1 HISTORY

=over 3 

=item * 1.0: initial release

=item * 1.1 (26-Feb-2002): Shorter form for short PDLs; more readability

=back

=head1 FUNCTIONS

=cut

package PDL::IO::Dumper;

BEGIN{
  use Exporter ();

  our $VERSION = 1.1;
  
  our @ISA = ( Exporter ) ;
  our @EXPORT_OK = qw( fdump sdump frestore );
  our @EXPORT = @EXPORT_OK;

  our %EXPORT_TAGS = ( );

 use PDL;
 use PDL::Exporter;
 use Data::Dumper;
 use Carp;
}

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

# Find the bless(...,'PDL') lines
  while($s =~ s/bless\( do\{\\\(my \$o \= (\d+)\)\}\, \'PDL\' \)/\$PDL_$1/) {
    $pdls{$1}=1;
  }

  # This next is broken into two parts to ensure $s is evaluated *after* the 
  # find_PDLs call (which modifies $s using the s/// operator).

  my($s2) =  "{my(\$VAR1);\n".&PDL::IO::Dumper::find_PDLs(\$s,@_)."\n\n";
  return $s2.$s."\n}";

#
}

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

I suggest using the extension '.pld' or (for non-broken OS's) '.pdld'
to distinguish Dumper files.  That way they're reminiscent of .pl
files for perl, while still looking a little different so you can pick
them out.  You can certainly feed a dump file straight into perl (for
syntax checking) but it won't do much for you, just build your data
structure and exit.

=cut
 
sub PDL::IO::Dumper::fdump { 
  my($struct,$file) = @_;
  if(!open(FDuMPFILE,">$file")) {
    Carp::cluck ("fdump: couldn't open '$file'\n");
     return undef;
  }
  print FDuMPFILE "####################\n## PDL::IO::Dumper dump file -- eval this in perl/PDL.\n\n";
  print FDuMPFILE sdump($struct);
  close FDuMPFILE;
  return $struct;
}

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

sub PDL::IO::Dumper::frestore {
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

=head2 PDL::IO::Dumper::big_PDL -- identify whether a PDL is ``big''

Internal routine takes a PDL and returns a boolean indicating whether
it's small enough for direct insertion into the dump string.  If 0, 
it can be inserted.  Larger numbers yield larger scopes of PDL.  
1 implies that it should be broken out but can be handled with a couple
of perl commands; 2 implies full uudecode treatment.

=cut

$PDL::IO::Dumper::small_thresh = 8;   # Smaller than this gets inlined
$PDL::IO::Dumper::med_thresh   = 200; # Smaller than this gets eval'ed
                                      # Any bigger gets uuencoded

sub PDL::IO::Dumper::big_PDL {
  my($a) = shift;
  
  return 0 
    if($a->nelem <= $PDL::IO::Dumper::small_thresh 
       && !defined $a->gethdr()
       );
  
  return 1
    if($a->nelem <= $PDL::IO::Dumper::med_thresh
       );

  return 2;
}

######################################################################

=head2 PDL::IO::Dumper::stringify_PDL -- turn a PDL into a 1-part perl expr
  
Internal routine that takes a PDL and returns a perl string that evals to the
PDL.  It should be used with care because it doesn't dump headers and 
it doesn't check number of elements.  The point here is that numbers are
dumped with the correct precision for their storage class.  Things we
don't know about get stringified element-by-element by their builtin class,
which is probably not a bad guess.

=cut

%PDL::IO::Dumper::stringify_formats = (
   "byte"=>"%d",
   "short"=>"%d",
   "long"=>"%d",
   "float"=>"%.6g",
   "double"=>"%.16g"
  );


sub PDL::IO::Dumper::stringify_PDL{
  my($pdl) = shift;
  
  if(!ref $pdl) {
    confess "PDL::IO::Dumper::stringify -- got a non-pdl value!\n";
    die;
  }

  ## Special case: empty PDL
  if($pdl->nelem == 0) {
    return "which(pdl(0))";
  }

  ## Normal case:  Figure out how to dump each number and dump them 
  ## in sequence as ASCII strings...

  my($pdlflat) = $pdl->flat;
  my($t) = $pdl->type;

  my($s);
  my(@s);
  my($dmp_elt);

  if(defined $PDL::IO::Dumper::stringify_formats{$t}) {
    $dmp_elt = eval "sub { sprintf '$PDL::IO::Dumper::stringify_formats{$t}',shift }";
  } else {
    if(!$PDL::IO::Dumper::stringify_warned) {
      cluck("PDL::IO::Dumper:  Warning, stringifying a '$t' PDL using default method\n\t(Will be silent after this)\n");
      $PDL::IO::Dumper::stringify_warned = 1;
    }
    $dmp_elt = sub { my($a) = shift; "$a"; };
  }
  $i = 0;

  my($i);
  for($i = 0; $i < $pdl->nelem; $i++) {
    push(@s, &{$dmp_elt}( $pdlflat->(($i)) )  );
  }

 
  ## Assemble all the strings and bracket with a pdl() call.
  
  $s = ($PDL::IO::Dumper::stringify_formats{$t}?$t:'pdl').
       "(" . join(   "," , @s  ) .   ")".
       (($_->getndims > 1) && ("->reshape(" . join(",",$pdl->dims) . ")"));

  return $s;
}


######################################################################

=head2 PDL::IO::Dumper::dump_PDL -- generate 1- or 2-part expr for a PDL

Internal routine that produces commands defining a PDL.  You supply
(<PDL>, <name>) and get back two strings: a prepended command string and an
expr that evaluates to the final PDL.  PDL is the PDL you want to dump.  
<inline> is a flag whether dump_PDL is being called inline or before
the inline dump string (0 for before; 1 for in).  <name> is the
name of the variable to be assigned (for medium and large PDLs,
which are defined before the dump string and assigned unique IDs).

=cut

sub PDL::IO::Dumper::dump_PDL {
  local($_) = shift;
  my($pdlid) = @_;
  my(@out);

  my($style) = &PDL::IO::Dumper::big_PDL($_);

  if($style==0) {
    @out = ("", "( ". &PDL::IO::Dumper::stringify_PDL($_). " )");
  }

  else {
    my(@s);

    ## midsized case
    if($style==1){
      @s = ("my(\$$pdlid) = (",
	    &PDL::IO::Dumper::stringify_PDL($_),
	    ");\n");
    }

    ## huge case
    else { 
      
      ##
      ## Write FITS file, uuencode it, snarf it up, and clean up /tmp
      ##
      my($fname) = "/tmp/$$.fits";
      wfits($_,$fname);
      open(FITSFILE,"uuencode $fname $fname |");
      my(@uulines) = <FITSFILE>;
      unlink $fname;
      
      ## 
      ## Generate commands to uudecode the FITS file and resnarf it
      ##
      @s = ("open DuMPERFILE,'|uudecode'; print DuMPERFILE <<'blat'\n",
	    @uulines,
	    "blat\n;\n",
	    "close DuMPERFILE;\n",
	    "my(\$$pdlid) = rfits('$fname'); unlink '$fname';\n",
	    "\$$pdlid->hdrcpy(".$_->hdrcpy().");\n"
	    );

      ##
      ## Unfortunately, FITS format mangles headers (and gives us one
      ## even if we don't want it).  Delete the FITS header if we don't
      ## want one.
      ## 
      if(!defined ($_->gethdr())) {
	push(@s,"\$$pdlid->sethdr(undef);\n");
      }
    }


    ## 
    ## Generate commands to reconstitute the header
    ## information in the PDL -- common to midsized and huge case.
    ## 
#    print "hdr: ",$_->gethdr()," keys:",scalar(keys(%{$_->gethdr()})),"\n";

    if( (defined ($_->gethdr())) && scalar(keys(%{$_->gethdr()}))) {
      push(@s,"\$$pdlid->sethdr( eval <<'FooBar${pdlid}'\n",
	   &PDL::IO::Dumper::sdump($_->gethdr()),
	   "\nFooBar${pdlid}\n);\n",
	   "\$$pdlid->hdrcpy(".$_->hdrcpy().");\n"
	   );
    }
    
    @out = (join("",@s), undef);
  }

  return @out;
}
  
######################################################################

=head2 PDL::IO::Dumper::find_PDLs -- walk a data structure and dump PDLs

Walks the original data structure and generates appropriate exprs
for each PDL.  The exprs are inserted into the Data::Dumper output
string.  You shouldn't call this unless you know what you're doing.
(see sdump, above).

=cut

sub PDL::IO::Dumper::find_PDLs {
  local($_);
  my($out)="";
  my($sp) = shift;

  findpdl:foreach $_(@_) {
    next findpdl unless ref($_);

    if(UNIVERSAL::isa($_,'ARRAY')) {
      my($a);
      foreach $a(@{$_}) {
	$out .= find_PDLs($sp,$a);
      }
    } 
    elsif(UNIVERSAL::isa($_,'HASH')) {
      my($a);
      foreach $a(values %{$_}) {
	$out .= find_PDLs($sp,$a)
	}
    } elsif(UNIVERSAL::isa($_,'PDL')) {

      # In addition to straight PDLs, 
      # this gets subclasses of PDL, but NOT magic-hash subclasses of
      # PDL (because they'd be gotten by the previous clause).
      # So if you subclass PDL but your actual data structure is still
      # just a straight PDL (and not a hash with PDL field), you end up here.
      #

      my($pdlid) = "PDL_".$$_;
      my(@strings) = &PDL::IO::Dumper::dump_PDL($_,$pdlid);
      
      $out .= $strings[0];
      $$sp =~ s/\$$pdlid/$strings[1]/g if(defined($strings[1]));  
    }
    elsif(UNIVERSAL::isa($_,'SCALAR')) {
      # This gets other kinds of refs -- PDLs have already been gotten.
      # Naked PDLs are themselves SCALARs, so the SCALAR case has to come 
      # last to let the PDL case run.
      $out .= find_PDLs( $sp, ${$_} );
    }
  
  }
  return $out;
}
      
1;
