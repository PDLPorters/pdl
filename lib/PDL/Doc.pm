# the filter for the PDL pod format (which is a valid general perl
# pod format but with special interpretation of some =for directives)

package PDL::PodParser;
use strict;
use warnings;
use PDL::Core '';
use Pod::Select;

our @ISA = qw(Pod::Select);

our %Title = ('Example' => 'Example',
	  'Ref'     => 'Reference',
	  'Sig'     => 'Signature',
	  'Opt'     => 'Options',
	  'Usage'   => 'Usage',
          'Bad'     => 'Bad value support',
	 );

sub new {
  my $class = shift;
  my $parser = $class->SUPER::new(@_);
  bless $parser,$class; # just in case

  $parser->select("METHODS|OPERATORS|CONSTRUCTORS|FUNCTIONS|NAME");
  $parser->{CURFUNC} = undef;
  $parser->{SYMHASH} = {};
  $parser->{INBLOCK} = 0;
  $parser->{Mode} = "";
  $parser->{verbose} = 0;
  $parser->{NAME} = 'UNKNOWN';
  return $parser;
}

sub command {
  my ($this,$cmd,$txt,$line_num,$pod_para) = @_;
  $this->{Parmode} = 'Body';

  if ($cmd eq 'head1') {
    $this->{Mode} = $txt;
    $this->{Parmode} = 'Body';
    $this->{Parmode} = 'NAME' if $txt =~ /NAME/;
  } elsif ($this->{Mode} =~ /NAME/) {
    # do nothing (was 'last' but that was probably a mistake)
  } elsif ($cmd eq 'head2') {
    return $this->SUPER::command($cmd,$txt,$line_num,$pod_para) if $txt =~ /^The\s/; # heuristic to deal with GSL::CDF descriptive =head2
    # A function can have multiple names (ex: zeros and zeroes),
    # so split at the commas
    my @funcs = split(',',$txt);
    # Remove parentheses (so myfunc and myfunc() both work)
    my @names = map {$1 if m/\s*([^\s\(]+)\s*/} @funcs;
    barf "error parsing function list '$txt'"
      unless $#funcs == $#names;
    # check for signatures
    my $sym = $this->{SYMHASH};
    for (@funcs) {
      $sym->{$1}->{Module} = $this->{NAME} if m/\s*([^\s(]+)\s*/;
      $sym->{$1}->{Sig} = $2  if m/\s*([^\s(]+)\s*\(\s*(.+)\s*\)\s*$/;
    }
    # make the first one the current function
    $sym->{$names[0]}->{Names} = join(',',@names) if $#names > 0;
    my $name = shift @names;
    # Make the other names cross-reference the first name
    $sym->{$_}->{Crossref} = $name for (@names);
    my $sig = $sym->{$name}->{Sig};
    # diagnostic output
    print "\nFunction '".join(',',($name,@names))."'\n" if $this->{verbose};
    print "\n\tSignature: $sig\n" if defined $sig && $this->{verbose};
    $this->{CURFUNC} = $name;
  } elsif ($cmd eq 'for') {
    $this->check_for_mode($txt,$pod_para) if $cmd eq 'for';
  }
  local $this->{Parmode} = 'Body';
  $this->SUPER::command($cmd,$txt,$line_num,$pod_para);
}

sub check_for_mode {
  my ($this,$txt,$pod_para) = @_;
  if ($txt =~ /^(sig|example|ref|opt|usage|bad|body)/i) {
    $this->{Parmode} = ucfirst lc $1;
    print "switched now to '$1' mode\n" if $this->{VERBOSE};
    print "\n\t$Title{$this->{Parmode}}\n"
      unless $this->{Parmode} =~ /Body/ || !$this->{verbose};
  }
}

sub textblock {
  my $this = shift;
  my $txt = shift;
  $this->checkmode($txt);
  local $this->{INBLOCK} = 1;
  $this->SUPER::textblock($txt,@_);
  $this->{Parmode} = 'Body'; # and reset parmode
}

sub checkmode {
  my ($this,$txt,$verbatim) = @_;
  if ($this->{Mode} =~ /NAME/ && $this->{Parmode} =~ /NAME/) {
    $this->{NAME} = $1 if $this->trim($txt) =~ /^\s*(\S+)\s*/;
    print "\nNAME\t$this->{NAME}\n" if $this->{verbose};
    $this->{Parmode} = 'Body';
    return;
  }
  unless ($this->{Parmode} =~ /Body/ || $this->{INBLOCK}) {
    my $func = $this->{CURFUNC};
    die "no function defined\n" unless defined $func;
    local $this->{INBLOCK} = 1; # can interpolate call textblock?
    my $itxt = $verbatim ? $txt : $this->interpolate($txt);
    $this->{SYMHASH}->{$func}->{$this->{Parmode}} .=
      $this->trim($itxt,$verbatim);
    my $cr = ($verbatim && $this->{Parmode} ne 'Sig') ? "\n" : "";
    my $out = "\n\t\t$cr".$this->trim($itxt,$verbatim);
    print "$out\n$cr" if $this->{verbose};
  }
  $this->{Parmode} = 'Body';
}

sub verbatim {
  my $this = shift;
  my $txt = shift;
  $this->checkmode($txt,1);
  $this->SUPER::verbatim($txt,@_);
}

# this needs improvement
# and any formatting information should be removed?
# it probably depends
sub trim {
  my ($this,$txt,$verbatim) = @_;
  my $ntxt = "";
  $txt =~ s/(signature|usage):\s*//i if $this->{Parmode} eq 'Sig' ||
			   $this->{Parmode} eq 'Usage';
  if ($this->{Parmode} eq 'Sig') {

    $txt =~ s/^\s*//;
    $txt =~ s/\s*$//;
    while( $txt =~ s/^\((.*)\)$/$1/ ) {}; # Strip BALANCED brackets

  }
  for (split "\n", $txt) {
    s/^\s*(.*)\s*$/$1/ unless $verbatim;
    $ntxt .= "$_\n" unless m/^\s*$/;
  }
  # $txt =~ s/^\s*(.*)\s*$/$1/;
  chomp $ntxt;
  return $ntxt;
}


=head1 NAME

PDL::Doc - support for PDL online documentation

=head1 SYNOPSIS

  use PDL::Doc;
  $onlinedc = PDL::Doc->new($docfile);
  @match = $onlinedc->search('m/slice|clump/');

=head1 DESCRIPTION

An implementation of online docs for PDL.

=head1 Using PDL documentation

PDL::Doc's main use is in the "help" (synonym "?") and "apropos"
(synonym "??") commands in the perldl shell.  PDL::Doc provides the
infrastrucure to index and access PDL's documentation through these
commands.  There is also an API for direct access to the documentation
database (see below).

The PDL doc system is built on Perl's pod (Plain Old Documentation),
included inline with each module. The PDL core modules are
automatically indexed when PDL is built and installed, and there is
provision for indexing external modules as well.

To include your module's pod into the Perl::Doc index, you should
follow the documentation conventions below.

=head1 PDL documentation conventions

For a package like PDL that has I<a lot> of functions it
is very desirable to have some form of online help to
make it easy for users to remind themselves of names,
calling conventions and typical usage of the multitude
of functions at their disposal. To make it straightforward
to extract the relevant information from the POD documentation
in source files that make up the PDL distribution
certain conventions have been adopted in formatting this
documentation.

The first convention says that all documentation for
PDL functions appears in the POD section introduced
by one of the following:

  =head1 FUNCTIONS
  =head1 OPERATORS
  =head1 METHODS
  =head1 CONSTRUCTORS

If you're documenting an object-oriented interface to a class
that your module defines, you should use METHODS and CONSTRUCTORS
as appropriate.  If you are simply adding functions to PDL,
use FUNCTIONS and OPERATORS as appropriate.

Individual functions or methods in these section are introduced by

  =head2 funcname

where signature is the argumentlist for a PP defined function as
explained in L<PDL::PP>. Generally, PDL documentation is in valid POD
format (see L<perlpod>) but uses the C<=for> directive in a
special way. The C<=for> directive is used to flag to the PDL Pod
parser that information is following that will be used to generate
online help.

The PDL Pod parser recognises the following C<=for> directives:

=over 5

=item Ref

indicates that the one line reference for this function follows,
e.g.,

   =for ref

   Returns an ndarray of lags to parent.

=item Sig

the signature for the current function follows, e.g.,

   =for sig

      Signature: (a(n), [o]b(), [t]tmp(n))

=item Usage

an indication of the possible calling conventions for the current
function, e.g.,

   =for usage

      wpic($pdl,$filename[,{ options... }])

=item Opt

lists options for the current function, e.g.,

   =for options

      CONVERTER  => 'ppmtogif',   # explicitly specify pbm converter
      FLAGS      => '-interlaced -transparent 0',  # flags for converter
      IFORM      => 'PGM',        # explicitly specify intermediate format
      XTRAFLAGS  => '-imagename iris', # additional flags to defaultflags
      FORMAT     => 'PCX',        # explicitly specify output image format
      COLOR      => 'bw',         # specify color conversion
      LUT        => $lut,         # use color table information


=item Example

gives examples of typical usage for the current function:

   =for example

       wpic $pdl, $file;
       $im->wpic('web.gif',{LUT => $lut});
       for (@images) {
         $_->wpic($name[0],{CONVERTER => 'ppmtogif'})
       }

=item Bad

provides information on how the function handles bad values. The
documentation under this directive should indicate if this function
accepts ndarrays with bad values and under what circumstances this function
might return ndarrays with bad values.

=back

The PDL podparser is implemented as a simple state machine. Any of
the above C<=for> statements switches the podparser into a state
where the following paragraph is accepted as information for the
respective field (C<Ref>, C<Usage>, C<Opt>, C<Example> or C<Bad>).
Only the text up to
the end of the current paragraph is accepted, for example:

  =for example

         ($x,$y) = $z->func(1,3);  # this is part of the accepted info
         $x = func($z,0,1);        # this as well

         $x = func($c,$d);         # but this isn't

To make the resulting pod documentation also easily digestible for the
existing pod filters (pod2man, pod2text, pod2html, etc) the actual
textblock of information must be separated from the C<=for> directive
by at least one blank line. Otherwise, the textblock will be lost in
the translation process when the "normal" podformatters are used. The
general idea behind this format is that it should be easy to extract
the information for online documentation, automatic generation of a
reference card, etc but at the same time the documentation should be
translated by the standard podformatters without loss of contents
(and without requiring any changes in the existing POD format).

The preceding explanations should be further explained by the
following example (extracted from PDL/IO/Misc/misc.pd):

   =head2 rcols()

   =for ref

   Read ASCII whitespaced cols from file into ndarrays efficiently.

   If no columns are specified all are assumed
   Will optionally only process lines matching a pattern.
   Can take file name or *HANDLE.

   =for usage

    Usage: ($x,$y,...) = rcols(*HANDLE|"filename", ["/pattern/",$col1, $col2,] ...)

   e.g.,

   =for example

     ($x,$y)    = rcols 'file1'
     ($x,$y,$z) = rcols 'file2', "/foo/",3,4
     $x = PDL->rcols 'file1';

   Note: currently quotes are required on the pattern.


which is translated by, e.g, the standard C<pod2text> converter into:

  rcols()

    Read ASCII whitespaced cols from file into ndarrays efficiently.

    If no columns are specified all are assumed Will optionally only
    process lines matching a pattern. Can take file name or *HANDLE.

      Usage: ($x,$y,...) = rcols(*HANDLE|"filename", ["/pattern/",$col1, $col2,] ...)

    e.g.,

      ($x,$y)    = rcols 'file1'
      ($x,$y,$z) = rcols 'file2', "/foo/",3,4
      $x = PDL->rcols 'file1';

    Note: currently quotes are required on the pattern.

It should be clear from the preceding example that readable output
can be obtained from this format using the standard converters and
the reader will hopefully get a feeling how they can easily intersperse
the special C<=for> directives with the normal POD documentation.

=head2 Which directives should be contained in the documentation

The module documentation should
start with the

  =head1 NAME

  PDL::Modulename -- do something with ndarrays

section (as anyway required by C<pod2man>) since the PDL podparser
extracts the name of the module this function belongs to from
that section.

Each function that is I<not> only for internal use by the module
should be documented, introduced with the C<=head2> directive
in the C<=head1 FUNCTIONS> section. The only field that every function
documented along these lines should have is the I<Ref> field preceding
a one line description of its intended functionality (suitable for
inclusion in a concise reference card). PP defined functions (see L<PDL::PP>)
should have a I<Sig> field stating their signature. To facilitate
maintenance of this documentation for such functions the 'Doc' field
has been introduced into the definition of C<pp_def> (see again L<PDL::PP>)
which will take care that name and signature of the so defined function
are documented in this way (for examples of this usage see, for example,
the PDL::Slices module, especially F<slices.pd> and the resulting
F<Slices.pm>). Similarly, the 'BadDoc' field provides a means of
specifying information on how the routine handles the presence of
bad values: this will be automatically created if
C<BadDoc> is not supplied, or set to C<undef>.

Furthermore, the documentation for each function should contain
at least one of the I<Usage> or I<Examples> fields. Depending on the
calling conventions for the function under consideration presence
of both fields may be warranted.

If a function has options that should be given as a hash reference in
the form

   {Option => Value, ...}

then the possible options (and aproppriate values) should be explained
in the textblock following the C<=for Opt> directive (see example above
and, e.g., PDL::IO::Pic).

It is well possible that some of these conventions appear to be clumsy
at times and the author is keen to hear of any suggestions for better
alternatives.

=cut

package PDL::Doc;
use strict;
use warnings;
use PDL::Core '';
use File::Basename;
use PDL::Doc::Config;
use File::Spec::Functions qw(file_name_is_absolute abs2rel rel2abs catdir catfile);
use Cwd (); # to help Debian packaging

=head1 INSTANCE METHODS

=head2 new

  $onlinedc = PDL::Doc->new('file.pdl',[more files]);

=cut

sub new {
  my ($type,@files) = @_;
  my $this = bless {},$type;
  $this->{File} = [@files];
  $this->{Scanned} = [];
  $this->{Outfile} = $files[0];
  return $this;
}

=head2 addfiles

add another file to the online database associated with this object.

=cut

sub addfiles {
  my ($this,@files) = @_;
  push @{$this->{File}}, @files;
}

=head2 outfile

set the name of the output file for this online db

=cut

sub outfile {
  my ($this,$file) = @_;
  $this->{Outfile} = $file if defined $file;
  return $this->{Outfile};
}

=head2 ensuredb

Make sure that the database is slurped in

=cut

sub ensuredb {
  my ($this) = @_;
  while (my $fi = pop @{$this->{File}}) {
    open my $fh, $fi or barf "can't open database $fi, scan docs first";
    binmode $fh;
    my ($plen,$txt);
    while (read $fh, $plen,2) {
      my ($len) = unpack "S", $plen;
      read $fh, $txt, $len;
      my ($sym, $module, @a) = split chr(0), $txt;
      push @a, "" if @a % 2; # Add null string at end if necessary -- solves bug with missing REF section.
      $this->{SYMS}{$sym}{$module} = { @a, Dbfile => $fi }; # keep the origin pdldoc.db path
    }
    push @{$this->{Scanned}}, $fi;
  }
  return $this->{SYMS};
}

=head2 savedb

save the database (i.e., the hash of PDL symbols) to the file associated
with this object.

=cut

sub savedb {
  my ($this) = @_;
  my $hash = $this->ensuredb;
  open my $fh, '>', $this->{Outfile} or barf "can't write to symdb $this->{Outfile}: $!";
  binmode $fh;
  while (my ($name,$mods_hash) = each %$hash) {
    next if 0 == scalar(%$mods_hash);
    while (my ($module,$val) = each %$mods_hash) {
      my $fi = $val->{File};
      $val->{File} = abs2rel($fi, dirname($this->{Outfile}))
        #store paths to *.pm files relative to pdldoc.db
        if file_name_is_absolute($fi) && -f $fi;
      delete $val->{Dbfile}; # no need to store Dbfile
      my $txt = join(chr(0),$name,$module,%$val);
      print $fh pack("S",length($txt)).$txt;
    }
  }
}


=head2 gethash

Return the PDL symhash (e.g. for custom search operations). To see what
it has stored in it in JSON format:

  perl -MPDL::Doc -MJSON::PP -e \
    'print encode_json +PDL::Doc->new(PDL::Doc::_find_inc([qw(PDL pdldoc.db)]))->gethash' |
    json_pp -json_opt pretty,canonical

The symhash is a multiply nested hash ref with the following structure:

 $symhash = {
     function_name => {
             module::name => {
                  Module => 'module::name',
                  Sig    => 'signature string',
                  Bad    => 'bad documentation string',
                  ...
                  },
             },
     function_name => {
             module::name => {
                  Module => 'module::name',
                  Sig    => 'signature string',
                  Bad    => 'bad documentation string',
                  ...
                  },
             },
 }

The three-layer structure is designed to allow the symhash (and the
underlying database) to handle functions that have the same name but
reside in different module namespaces.

The possible keys for each function/module entry include:

 Module   - module name
 Sig      - signature
 Crossref - the function name for the documentation, if it has multiple
            names (ex: the documentation for zeros is under zeroes)
 Names    - a comma-separated string of all the function's names
 Example  - example text (optional)
 Ref      - one-line reference string
 Opt      - options
 Usage    - short usage explanation
 Bad      - explanation of behavior when it encounters bad values

=cut

sub gethash { $_[0]->ensuredb }

=head2 search

Search a PDL symhash

=for usage

  $onldc->search($regex, $fields [, $sort])

Searching is by default case insensitive. Other flags can be
given by specifying the regexp in the form C<m/regex/ismx>
where C</> can be replaced with any other non-alphanumeric
character. $fields is an array reference for all hash fields
(or simply a string if you only want to search one field)
that should be matched against the regex. Valid fields are

  Name,    # name of the function
  Module,  # module the function belongs to
  Ref,     # the one-line reference description
  Example, # the example for this function
  Opt,     # options
  File,    # the path to the source file these docs have been extracted from

If you wish to have your results sorted by function name, pass a true
value for C<$sort>.

The results will be returned as an array of triplets in the form

 @results = (
  [funcname, module, {SYMHASH_ENTRY}],
  [funcname, module, {SYMHASH_ENTRY}],
  ...
 );

See the example at the end of the documentation to see how you might
use this.

=cut


sub search {
  my ($this,$pattern,$fields,$sort) = @_;
  $sort = 0 unless defined $sort;
  my $hash = $this->ensuredb;
  my @match = ();

  # Make a single scalar $fields work
  $fields = [$fields] if ref($fields) eq '';

  $pattern = $this->checkregex($pattern);

  while (my ($name,$mods_hash) = each %$hash) {
      while (my ($module,$val) = each %$mods_hash) {
	FIELD: for (@$fields) {
	    if ($_ eq 'Name' and $name =~ /$pattern/i
		or defined $val->{$_} and $val->{$_} =~ /$pattern/i) {
		$val = $hash->{$val->{Crossref}}->{$module} #we're going to assume that any Crossref'd documentation is also in this module
		if defined $val->{Crossref} && defined $hash->{$val->{Crossref}}->{$module};
		push @match, [$name,$module,$val];
		last FIELD;
	    }
	}
      }
  }
  @match = sort {$a->[0] cmp $b->[0]} @match if (@match && $sort);
  return @match;
}


# parse a regexp in the form
#   m/^[a-z]+/ismx
# where the pairs of '/' can be replaced by any other pair of matching
# characters
# if the expression doesn't start with 'm' followed by a nonalphanumeric
# character,  return as-is
sub checkregex {
  my ($this,$regex) = @_;
  return "(?i)$regex" unless $regex =~ /^m[^a-z,A-Z,0-9]/;
  my $sep = substr($regex,1,1);
  substr($regex,0,2) = '';
  $sep = '(?<!\\\\)\\'.$sep; # Avoid '\' before the separator

  my ($pattern,$mod) = split($sep,$regex,2);
  barf "unknown regex modifiers '$mod'" if $mod && $mod !~ /[imsx]+/;
  $pattern = "(?$mod)$pattern" if $mod;
  return $pattern;
}

=head2 scan

Scan a source file using the PDL podparser to extract information
for online documentation

=cut

sub scan {
  my ($this,$file,$verbose) = @_;
  barf "can't find file '$file'" unless -f $file;
  $file = Cwd::abs_path($file); # help Debian packaging
  $verbose = 0 unless defined $verbose;

  open my $infile, '<', $file;
  # XXXX convert to absolute path
  # my $outfile = '/tmp/'.basename($file).'.pod';
  open my $outfile, '>', \(my $outfile_text);

  # Handle RPM etc. case where we are building away from the final
  # location. Alright it's a hack - KGB
  my $file2 = $file;
  $file2 =~ s/^$ENV{BUILDROOTPREFIX}// if $ENV{BUILDROOTPREFIX};

  my $parser = PDL::PodParser->new;
  $parser->{verbose} = $verbose;
  eval { $parser->parse_from_filehandle($infile,$outfile) };
  warn "cannot parse '$file' ($@)" if $@ and $@ ne "no function defined\n";

  my $hash = $this->{SYMS} ||= {};
  my $n = 0;
  $_->{File} = $file2, $n++ for values %{ $parser->{SYMHASH} };
  while (my ($key,$val) = each %{ $parser->{SYMHASH} }) {
    #set up the 3-layer hash/database structure: $hash->{funcname}->{PDL::SomeModule} = $val
    if (defined($val->{Module})) {
	$hash->{$key}{$val->{Module}} = $val;
    } else {
	warn "no Module for $key in $file2\n";
    }
  }

  # KGB pass2 - scan for module name and function
  # alright I admit this is kludgy but it works
  # and one can now find modules with 'apropos'

  open $infile, '<', $file;
  $outfile_text = '';
  $parser = PDL::PodParser->new;
  $parser->select('NAME');
  eval { $parser->parse_from_filehandle($infile,$outfile) };
  warn "cannot parse '$file'" if $@;

  my @namelines = split("\n",$outfile_text);
  my ($name,$does);
  for (@namelines) {
     if (/^(PDL) (-) (.*)/ or  /^\s*(Inline::Pdlpp)\s*(-*)?\s*(.*)\s*$/ or /\s*(PDL::[\w:]*)\s*(-*)?\s*(.*)\s*$/) {
	$name = $1; $does = $3;
     }
     if (/^\s*([a-z][a-z0-9]*) (-+) (.*)/) { # lowercase shell script name
       $name = $1; $does = $3;
       ($name,$does) = (undef,undef) unless $does =~ /shell|script/i;
     }
   }
   $does = 'Hmmm ????' if $does and $does =~ /^\s*$/;
   my $type = ($file =~ /\.pod$/ ?
	       ($does =~ /shell|script/i && $name =~ /^[a-z][a-z0-9]*$/) ? 'Script:'
	       : 'Manual:'
	       : 'Module:');
   $hash->{$name}{$name} = {Ref=>"$type $does",File=>$file2} if $name and $name !~ /^\s*$/;
   return $n;
}

=head2 scantree

Scan whole directory trees for online documentation in
C<.pm> (module definition) and C<*.pod> (general
documentation) files (using the File::Find module).

=cut

sub scantree {
  my ($this,$dir,$verbose) = @_;
  $verbose = 0 unless defined $verbose;
  require File::Find;
  print "Scanning $dir ... \n\n";
  my $ntot = 0;
  my $sub = sub {
    return if $File::Find::name !~ /\.(?:pm|pod)$/;
    return if $File::Find::name =~ /(?:Index\.pod|PP\.pm)$/ or
      $File::Find::dir =~ m#/PP#;
    printf "%-20s", $_.'...';
    $ntot += my $n = $this->scan($File::Find::name,$verbose);
    print "\t$n functions\n";
  };
  File::Find::find($sub,$dir);
  print "\nfound $ntot functions\n";
  $ntot;
}


=head2 funcdocs

extract the complete documentation about a function from its
source file using the PDL::PodParser filter.

=cut

sub funcdocs {
  my ($this,$func,$module,$fout) = @_;
  my $hash = $this->ensuredb;
  barf "unknown function '$func'" unless defined($hash->{$func});
  barf "funcdocs now requires 3 arguments" if defined fileno $module;
  my $file = $hash->{$func}{$module}{File};
  my $dbf = $hash->{$func}{$module}{Dbfile};
  $file = Cwd::abs_path($file) if file_name_is_absolute($file);
  $dbf = Cwd::abs_path($dbf); # help Debian packaging
  $file = rel2abs($file, dirname($dbf))
    if !file_name_is_absolute($file) && $dbf;
  funcdocs_fromfile($func,$file,$fout);
}

=head1 FUNCTIONS

=cut

sub funcdocs_fromfile {
  my ($func,$file) = @_;
  barf "can't find file '$file'" unless -f $file;
  local $SIG{PIPE}= sub {}; # Prevent crashing if user exits the pager
  open my $in, '<', $file or barf "can't open file $file";
  my $out = $_[2];
  open $out, "| pod2text | $PDL::Doc::pager" if !defined $out;
  barf "can't open output handle" unless $out;
  getfuncdocs($func,$in,$out);
  print $out "Docs from $file\n\n";
}

sub extrdoc {
  my ($func,$file) = @_;
  open my $out, '>', \(my $out_text);
  funcdocs_fromfile($func,$file,$out);
  return $out_text;
}

sub getfuncdocs {
  my ($func,$in,$out) = @_;
  my $parser = Pod::Select->new;
#  $parser->select("\\(METHODS\\|OPERATORS\\|CONSTRUCTORS\\|FUNCTIONS\\|METHODS\\)/$func(\\(.*\\)*\\s*");
  foreach my $foo(qw/FUNCTIONS OPERATORS CONSTRUCTORS METHODS/) {
      seek $in,0,0;
      $parser->select("$foo/(.*,\\s+)*$func(\\(.*\\))*(\\s*|,\\s+.*)");
      $parser->parse_from_filehandle($in,$out);
  }
}


=head2 add_module

=for usage

 use PDL::Doc;
 PDL::Doc::add_module("PDL::Stats"); # add PDL::Stats, PDL::Stats::GLM, ...

=for ref

The C<add_module> function allows you to add POD from a particular Perl
module (and as of PDL 2.083, in fact all modules starting with that as
a prefix) that you've installed somewhere in C<@INC>. It searches for the
active PDL document database and the module's .pod and .pm files, and
scans and indexes the module(s) into the database.

C<add_module> is meant to be added to your module's Makefile as part of the
installation script. This is done automatically by
L<PDL::Core::Dev/pdlpp_postamble>, but if the top level of your
distribution is Perl modules (like L<PDL::LinearAlgebra>), then add a
C<postamble> manually in the F<Makefile.PL>:

  use PDL::Core::Dev;
  sub MY::postamble {
    my $oneliner = PDL::Core::Dev::_oneliner(qq{exit if \$ENV{DESTDIR}; use PDL::Doc; eval { PDL::Doc::add_module(shift); }});
    qq|\ninstall :: pure_install\n\t$oneliner \$(NAME)\n|;
  }

=cut

sub _find_inc {
  my ($what, $want_dir) = @_;
  my @ret;
  for my $dir (@INC) {
    my $ent = $want_dir ? catdir($dir, @$what) : catfile($dir, @$what);
    push @ret, $ent if $want_dir ? -d $ent : -f $ent;
  }
  @ret;
}

sub add_module {
  my ($module) = @_;
  my ($file) = _find_inc([qw(PDL pdldoc.db)], 0);
  die "Unable to find docs database - therefore not updating it.\n" if !defined $file;
  die "No write permission for $file - not updating docs database.\n"
    if !-w $file;
  print "Found docs database $file\n";
  my $pdldoc = PDL::Doc->new($file);
  my @pkg = my @mfile = split /::/, $module;
  my $mlast = pop @mfile;
  my @found = map _find_inc([@mfile, $mlast.$_]), qw(.pm .pod);
  die "Unable to find a .pm or .pod file in \@INC for module $module\n" if !@found;
  $pdldoc->ensuredb;
  my $n = 0;
  $n += $pdldoc->scan($_) for @found;
  print "Added @found, $n functions.\n";
  $n += $pdldoc->scantree($_) for _find_inc(\@pkg, 1);
  eval { $pdldoc->savedb; };
  warn $@ if $@;
  print "PDL docs database updated - total $n functions.\n";
}

=head1 PDL::DOC EXAMPLE

Here's an example of how you might use the PDL Doc database in your
own code.

 use PDL::Doc;
 # Find the pdl documentation
 my ($file) = _find_inc([qw(PDL pdldoc.db)], 0);
 die "Unable to find docs database!\n" unless defined $file;
 print "Found docs database $file\n";
 my $pdldoc = PDL::Doc->new($file);
 # Print the reference line for zeroes:
 print map{$_->{Ref}} values %{$pdldoc->gethash->{zeroes}};
 # Or, if you remember that zeroes is in PDL::Core:
 print $pdldoc->gethash->{zeroes}->{PDL::Core}->{Ref};

 # Get info for all the functions whose examples use zeroes
 my @entries = $pdldoc->search('zeroes','Example',1);

 # All the functions that use zeroes in their example:
 print "Functions that use 'zeroes' in their examples include:\n";
 foreach my $entry (@entries) {
     # Unpack the entry
     my ($func_name, $module, $sym_hash) = @$entry;
     print "$func_name\n";
 }
 print "\n";

 #Or, more concisely:
 print map "$_->[0]\n", @entries;

 # Let's look at the function 'mpdl'
 @entries = $pdldoc->search('mpdl', 'Name');
 # I know there's only one:
 my $entry = $entries[0];
 my ($func_name, undef, $sym_hash) = @$entry;
 print "mpdl info:\n";
 foreach my $key (sort keys %$sym_hash) {
     # Unpack the entry
     print "---$key---\n$sym_hash->{$key}\n";
 }

=head2 Finding Modules

How can you tell if you've gotten a module for one of your entries?
The Ref entry will begin with 'Module:' if it's a module. In code:

 # Prints:
 #  Module: fundamental PDL functionality and vectorization/broadcasting
 print $pdldoc->gethash->{'PDL::Core'}->{'PDL::Core'}->{Ref}, "\n"

=head1 BUGS

Quite a few shortcomings which will hopefully be fixed following
discussions on the pdl-devel mailing list.

=head1 AUTHOR

Copyright 1997 Christian Soeller E<lt>c.soeller@auckland.ac.nzE<gt>
and Karl Glazebrook E<lt>kgb@aaoepp.aao.gov.auE<gt>

Further contributions copyright 2010 David Mertens
E<lt>dcmertens.perl@gmail.comE<gt>

Documentation database restructuring 2019 Derek Lamb

All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=cut

1;
