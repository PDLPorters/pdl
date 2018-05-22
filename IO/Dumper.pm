=head1 NAME

PDL::IO::Dumper -- data dumping for structs with PDLs

=head1 DESCRIPTION

This package allows you cleanly to save and restore complex data structures
which include PDLs, as ASCII strings and/or transportable ASCII files.  It
exports four functions into your namespace: sdump, fdump, frestore, and
deep_copy.

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
and included in the dump string. (You have to have access to either 
uuencode(1) or the CPAN module Convert::UU for this to work).

No attempt is made to shrink the output string -- for example, inlined
PDL expressions all include explicit reshape() and typecast commands,
and uuencoding expands stuff by a factor of about 1.5.  So your data
structures will grow when you dump them. 

=head1 Bugs

It's still possible to break this code and cause it to dump core, for
the same reason that Data::Dumper crashes.  In particular, other
external-hook variables aren't recognized (for that a more universal
Dumper would be needed) and will still exercise the Data::Dumper crash.  
This is by choice:  (A) it's difficult to recognize which objects
are actually external, and (B) most everyday objects are quite safe.

Another shortfall of Data::Dumper is that it doesn't recognize tied objects.
This might be a Good Thing or a Bad Thing depending on your point of view, 
but it means that PDL::IO::Dumper includes a kludge to handle the tied
Astro::FITS::Header objects associated with FITS headers (see the rfits 
documentation in PDL::IO::Misc for details).

There's currently no reference recursion detection, so a non-treelike
reference topology will cause Dumper to buzz forever.  That will
likely be fixed in a future version.  Meanwhile a warning message finds
likely cases.


=head1 Author, copyright, no warranty

Copyright 2002, Craig DeForest.

This code may be distributed under the same terms as Perl itself
(license available at L<http://ww.perl.org>).  Copying, reverse
engineering, distribution, and modification are explicitly allowed so
long as this notice is preserved intact and modified versions are
clearly marked as such.

This package comes with NO WARRANTY.

=head1 HISTORY

=over 3 

=item * 1.0: initial release

=item * 1.1 (26-Feb-2002): Shorter form for short PDLs; more readability

=item * 1.2 (28-Feb-2002): Added deep_copy() -- exported convenience function
  for "eval sdump"

=item * 1.3 (15-May-2002): Added checking for tied objects in gethdr()
  [workaround for hole in Data::Dumper]

=item * 1.4 (15-Jan-2003): Added support for Convert::UU as well as
  command-line uu{en|de}code

=back

=head1 FUNCTIONS

=cut

# use PDL::NiceSlice;

package PDL::IO::Dumper;
use File::Temp;


BEGIN{
  use Exporter ();

  package PDL::IO::Dumper;

  $PDL::IO::Dumper::VERSION = '1.3.2';
  
  @PDL::IO::Dumper::ISA = ( Exporter ) ;
  @PDL::IO::Dumper::EXPORT_OK = qw( fdump sdump frestore deep_copy);
  @PDL::IO::Dumper::EXPORT = @EXPORT_OK;
  %PDL::IO::Dumper::EXPORT_TAGS = ( Func=>[@EXPORT_OK]);

  eval "use Convert::UU;";
  $PDL::IO::Dumper::convert_ok = !$@;

  my $checkprog = sub {
      my($prog) = $_[0];
      my $pathsep = $^O =~ /win32/i ? ';' : ':';
      my $exe = $^O =~ /win32/i ? '.exe' : '';
      for(split $pathsep,$ENV{PATH}){return 1 if -x "$_/$prog$exe"}
      return 0;
  };
  # make sure not to use uuencode/uudecode
  # on MSWin32 systems (it doesn't work)
  # Force Convert::UU for BSD systems to see if that fixes uudecode problem
  if (($^O !~ /(MSWin32|bsd)$/) or ($^O eq 'gnukfreebsd')) {
     $PDL::IO::Dumper::uudecode_ok = &$checkprog('uudecode') and &$checkprog('uuencode') and ($^O !~ /MSWin32/);
  }

  use PDL;
  use PDL::Exporter;
  use PDL::Config;
  use Data::Dumper 2.121;
  use Carp;
  use IO::File;
}

######################################################################

=head2 sdump

=for ref

Dump a data structure to a string.

=for usage

  use PDL::IO::Dumper;
  $s = sdump(<VAR>);
  ...
  <VAR> = eval $s;

=for description

sdump dumps a single complex data structure into a string.  You restore
the data structure by eval-ing the string.  Since eval is a builtin, no
convenience routine exists to use it.

=cut

sub PDL::IO::Dumper::sdump {
# Make an initial dump...
  my($s) = Data::Dumper->Dump([@_]);
  my(%pdls);
# Find the bless(...,'PDL') lines
  while($s =~ s/bless\( do\{\\\(my \$o \= '?(-?\d+)'?\)\}\, \'PDL\' \)/sprintf('$PDL_%u',$1)/e) {
    $pdls{$1}++;
  }

## Check for duplicates -- a weak proxy for recursion...
  my($v);
  my($dups);
  foreach $v(keys %pdls) {
    $dups++ if($pdls{$v} >1);
  }
  print STDERR "Warning: duplicated PDL ref.  If sdump hangs, you have a circular reference.\n"  if($dups);

  # This next is broken into two parts to ensure $s is evaluated *after* the 
  # find_PDLs call (which modifies $s using the s/// operator).

  my($s2) =  "{my(\$VAR1);\n".&PDL::IO::Dumper::find_PDLs(\$s,@_)."\n\n";
  return $s2.$s."\n}";

#
}

######################################################################

=head2 fdump

=for ref

Dump a data structure to a file

=for usage

  use PDL::IO::Dumper;
  fdump(<VAR>,$filename);
  ...
  <VAR> = frestore($filename);

=for description

fdump dumps a single complex data structure to a file.  You restore the
data structure by eval-ing the perl code put in the file.  A convenience
routine (frestore) exists to do it for you.

I suggest using the extension '.pld' or (for non-broken OS's) '.pdld'
to distinguish Dumper files.  That way they are reminiscent of .pl
files for perl, while still looking a little different so you can pick
them out.  You can certainly feed a dump file straight into perl (for
syntax checking) but it will not do much for you, just build your data
structure and exit.

=cut

sub PDL::IO::Dumper::fdump { 
  my($struct,$file) = @_;
  my $fh = IO::File->new( ">$file" );
  unless ( defined $fh ) {
      Carp::cluck ("fdump: couldn't open '$file'\n");
      return undef;
  }
  $fh->print( "####################\n## PDL::IO::Dumper dump file -- eval this in perl/PDL.\n\n" );
  $fh->print( sdump($struct) );
  $fh->close();
  return $struct;
}

######################################################################

=head2 frestore

=for ref

Restore a dumped file

=for usage

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

  my $fh = IO::File->new( "<$fname" );
  unless ( defined $fh ) {
    Carp::cluck("frestore:  couldn't open '$file'\n");
    return undef;
  }

  my($file) = join("",<$fh>);
  
  $fh->close;

  return eval $file;
}

######################################################################

=head2 deep_copy

=for ref

Convenience function copies a complete perl data structure by the
brute force method of "eval sdump".

=cut

sub PDL::IO::Dumper::deep_copy {
  return eval sdump @_;
}

######################################################################

=head2 PDL::IO::Dumper::big_PDL

=for ref

Identify whether a PDL is ``big'' [Internal routine]

Internal routine takes a PDL and returns a boolean indicating whether
it's small enough for direct insertion into the dump string.  If 0, 
it can be inserted.  Larger numbers yield larger scopes of PDL.  
1 implies that it should be broken out but can be handled with a couple
of perl commands; 2 implies full uudecode treatment.

PDLs with Astro::FITS::Header objects as headers are taken to be FITS
files and are always treated as huge, regardless of size.

=cut

$PDL::IO::Dumper::small_thresh = 8;   # Smaller than this gets inlined
$PDL::IO::Dumper::med_thresh   = 400; # Smaller than this gets eval'ed
                                      # Any bigger gets uuencoded

sub PDL::IO::Dumper::big_PDL {
  my($x) = shift;
  
  return 0 
    if($x->nelem <= $PDL::IO::Dumper::small_thresh
       && !(keys %{$x->hdr()})
       );
  
  return 1
    if($x->nelem <= $PDL::IO::Dumper::med_thresh
       && ( !( ( (tied %{$x->hdr()}) || '' ) =~ m/^Astro::FITS::Header\=/)  )
       );

  return 2;
}

######################################################################

=head2 PDL::IO::Dumper::stringify_PDL

=for ref

Turn a PDL into a 1-part perl expr [Internal routine]

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
      print STDERR "PDL::IO::Dumper:  Warning, stringifying a '$t' PDL using default method\n\t(Will be silent after this)\n";
      $PDL::IO::Dumper::stringify_warned = 1;
    }
    $dmp_elt = sub { my($x) = shift; "$x"; };
  }
  $i = 0;

  my($i);
  for($i = 0; $i < $pdl->nelem; $i++) {
    push(@s, &{$dmp_elt}( $pdlflat->slice("($i)") )  );
  }

 
  ## Assemble all the strings and bracket with a pdl() call.
  
  $s = ($PDL::IO::Dumper::stringify_formats{$t}?$t:'pdl').
       "(" . join(   "," , @s  ) .   ")".
       (($_->getndims > 1) && ("->reshape(" . join(",",$pdl->dims) . ")"));

  return $s;
}


######################################################################

=head2 PDL::IO::Dumper::uudecode_PDL

=for ref

Recover a PDL from a uuencoded string [Internal routine]

This routine encapsulates uudecoding of the dumped string for large piddles. 
It's separate to encapsulate the decision about which method of uudecoding
to try (both the built-in Convert::UU and the shell command uudecode(1) 
are supported).

=cut

# should we use OS/library-level routines for creating
# a temporary filename?
#
sub _make_tmpname () {
    # should we use File::Spec routines to create the file name?
    return File::Temp::tmpnam() . ".fits";
}

# For uudecode_PDL:
#
# uudecode on OS-X needs the -s option otherwise it strips off the
# path of the data file - which messes things up. We could change the
# logic so that we explicitly tell uudecode where to create the output
# file, except that this is also OS-dependent (-o <filename> on OS-X/linux,
# -p on solaris/OS-X to write to stdout, any others?),
# so we go this way for now as it is less-likely to break things
#
my $uudecode_string = "|uudecode";
$uudecode_string .= " -s" if (($^O =~ m/darwin|((free|open)bsd)|dragonfly/) and ($^O ne 'gnukfreebsd'));

sub PDL::IO::Dumper::uudecode_PDL {
    my $lines = shift;
    my $out;
    my $fname = _make_tmpname();
    if($PDL::IO::Dumper::uudecode_ok) {
        local $SIG{PIPE}= sub {}; # Prevent crashing if uudecode exits
	my $fh = IO::File->new( $uudecode_string );
	$lines =~ s/^[^\n]*\n/begin 664 $fname\n/o;
	$fh->print( $lines );
	$fh->close;
    }
    elsif($PDL::IO::Dumper::convert_ok) {
	my $fh = IO::File->new(">$fname");
	my $fits = Convert::UU::uudecode($lines);
	$fh->print( $fits );
	$fh->close();
    } else {
      barf("Need either uudecode(1) or Convert::UU to decode dumped PDL.\n");
  }

  $out = rfits($fname);
  unlink($fname);

  $out;
}
 
=head2 PDL::IO::Dumper::dump_PDL

=for ref

Generate 1- or 2-part expr for a PDL [Internal routine]

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
      ## Write FITS file, uuencode it, snarf it up, and clean up the
      ## temporary directory
      ##
      my $fname = _make_tmpname();
      wfits($_,$fname);
      my(@uulines);

      if($PDL::IO::Dumper::uudecode_ok) {
	  my $fh = IO::File->new( "uuencode $fname $fname |" );
	  @uulines = <$fh>;
	  $fh->close;
    } elsif($PDL::IO::Dumper::convert_ok) {
	# Convert::UU::uuencode does not accept IO::File handles
        # (at least in version 0.52 of the module)
	#
	open(FITSFILE,"<$fname");
	@uulines = ( Convert::UU::uuencode(*FITSFILE) );
    } else {
	barf("dump_PDL: Requires either uuencode or Convert:UU");
    }
      unlink $fname;
      
      ## 
      ## Generate commands to uudecode the FITS file and resnarf it
      ##
      @s = ("my(\$$pdlid) = PDL::IO::Dumper::uudecode_PDL(<<'DuMPERFILE'\n",
	    @uulines,
	    "\nDuMPERFILE\n);\n",
	    "\$$pdlid->hdrcpy(".$_->hdrcpy().");\n"
	    );

      ##
      ## Unfortunately, FITS format mangles headers (and gives us one
      ## even if we don't want it).  Delete the FITS header if we don't
      ## want one.  
      ##
      if( !scalar(keys %{$_->hdr()}) ) {
	push(@s,"\$$pdlid->sethdr(undef);\n");
      }
    }

    ## 
    ## Generate commands to reconstitute the header
    ## information in the PDL -- common to midsized and huge case.
    ##
    ## We normally want to reconstitute, because FITS headers mangle
    ## arbitrary hashes and we can reconsitute efficiently with a private 
    ## sdump().  The one known exception to this is when there's a FITS
    ## header object (Astro::FITS::Header) tied to the original 
    ## PDL's header.  Other types of tied object will get handled just
    ## like normal hashes.
    ##
    ## Ultimately, Data::Dumper will get fixed to handle tied objects, 
    ## and this kludge will go away.
    ## 

    if( scalar(keys %{$_->hdr()}) ) {
      if( ((tied %{$_->hdr()}) || '') =~ m/Astro::FITS::Header\=/) {
	push(@s,"# (Header restored from FITS file)\n");
      } else {
	push(@s,"\$$pdlid->sethdr( eval <<'EndOfHeader_${pdlid}'\n",
	     &PDL::IO::Dumper::sdump($_->hdr()),
	     "\nEndOfHeader_${pdlid}\n);\n",
	     "\$$pdlid->hdrcpy(".$_->hdrcpy().");\n"
	     );
      }
    }
    
    @out = (join("",@s), undef);
  }

  return @out;
}
  
######################################################################

=head2 PDL::IO::Dumper::find_PDLs

=for ref

Walk a data structure and dump PDLs [Internal routine]

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
      my($x);
      foreach $x(@{$_}) {
	$out .= find_PDLs($sp,$x);
      }
    } 
    elsif(UNIVERSAL::isa($_,'HASH')) {
      my($x);
      foreach $x(values %{$_}) {
	$out .= find_PDLs($sp,$x)
	}
    } elsif(UNIVERSAL::isa($_,'PDL')) {

      # In addition to straight PDLs, 
      # this gets subclasses of PDL, but NOT magic-hash subclasses of
      # PDL (because they'd be gotten by the previous clause).
      # So if you subclass PDL but your actual data structure is still
      # just a straight PDL (and not a hash with PDL field), you end up here.
      #

      my($pdlid) = sprintf('PDL_%u',$$_);
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
