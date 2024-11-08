=head1 NAME

PDL::Doc::Perldl - commands for accessing PDL doc database from 'perldl' shell

=head1 DESCRIPTION

This module provides a set of functions to
access the PDL documentation database, for use
from the I<perldl> or I<pdl2> shells as well as the
I<pdldoc> command-line program.

Autoload files are also matched, via a search of the PDLLIB autoloader
tree.  That behavior can be switched off with the variable 
C<$PERLDL::STRICT_DOCS> (true: don't search autoload tree; false: search
the autoload tree.)

In the interest of brevity, functions that print module names (at the moment
just L</apropos> and L</usage>) use some shorthand notation for module names.
Currently-implemented shorthands are

=over 3

=item * P:: (short for PDL::)

=item * P::G:: (short for PDL::Graphics::)

=back

To turn this feature off, set the variable $PERLDL::long_mod_names to a true value.
The feature is assumed to be on for the purposes of this documentation.

=head1 SYNOPSIS

 use PDL::Doc::Perldl; # Load all documentation functions

=head1 FUNCTIONS

=cut

package PDL::Doc::Perldl;

use Exporter;
use strict;
use warnings;

our @ISA = qw(Exporter);
our @EXPORT = qw( apropos aproposover usage help sig badinfo whatis );

use PDL::Doc;
use Pod::Select;
use Pod::PlainText;
use Term::ReadKey; #for GetTerminalSize
use Cwd; # to help Debian packaging

$PDL::onlinedoc = PDL::Doc->new(FindStdFile());

# Find std file

sub FindStdFile {
  my ($f) = PDL::Doc::_find_inc([qw(PDL pdldoc.db)], 0);
  warn("Unable to find PDL/pdldoc.db in ".join(":",@INC)."\n"), return if !defined $f;
  print "Found docs database $f\n" if $PDL::verbose;
  print "Type 'help' for online help\n" if $PDL::verbose;
  return $f;
}

# used to find out how wide the screen should be
# for printmatch() - really should check for a 
# sensible lower limit (for printmatch >~ 40
# would be my guess)
#
# taken from Pod::Text (v1.0203), then hacked to get it
# to work (at least on my solaris and linux
# machines)
#
sub screen_width() {
    return ( ( GetTerminalSize(\*STDOUT) )[0] // 72);
}

sub printmatch {
    my @match = @_;
    if (@match) {
	foreach my $t ( format_ref( @_ ) ) { print $t; }
    } else {
	print "no match\n\n";
    }
} # sub: print_match()

# given a long module name, return the (perhaps shortened) module name.

sub shortmod {
  my $module = shift;
  $module =~ s/::$//;
  unless ($PERLDL::long_mod_names && $PERLDL::long_mod_names){ # silence warn
      $module =~ s/^PDL::/P::/;
      $module =~ s/^P::Graphics::/P::G::/;
      #additional abbreviation substitutions go here
  }
  return $module;
}

# return a string containing a formated version of the Ref string
# for the given matches
#
sub format_ref {
  my @match = @_;
  my @text = ();

  #finding the max width before doing the printing means looping through @match an extra time; so be it.
  my @module_shorthands = map { shortmod($_->[1]) } @match;
  my $max_mod_length = -1;
  map {$max_mod_length = length if (length>$max_mod_length) } @module_shorthands;


  my $width = screen_width()-17-1-$max_mod_length;
  my $parser = Pod::PlainText->new( width => $width, indent => 0, sentence => 0 );

  for my $m (@match) {
    my $ref = $m->[2]{Ref} ||
      ( (defined $m->[2]{CustomFile})
        ? "[No ref avail. for `".$m->[2]{CustomFile}."']"
        : "[No reference available]"
     );

    my $name = $m->[0];
    my $module = shortmod($m->[1]);

    $ref = $parser->interpolate( $ref );
    $ref = $parser->reformat( $ref );

    # remove last new lines (so substitution doesn't append spaces at end of text)
    $ref =~ s/\n*$//;
    $ref =~ s/\n/"\n                ".' 'x($max_mod_length+2)/eg;
    $ref =~ s/^\s*//;
    if ( length($name) > 15 ) {
      push @text, sprintf "%s ...\n " . ' 'x15 . "%-*s  %s\n", $name, $max_mod_length, $module, $ref;
    } else {
      push @text, sprintf "%-15s %-*s  %s\n", $name, $max_mod_length, $module, $ref;
    }
  }
  return wantarray ? @text : $text[0];

} # sub: format_ref()

=head2 apropos

=for ref

Regex search PDL documentation database

=for usage

 apropos 'text'

=for example

 pdl> apropos 'pic'
 PDL::IO::Pic    P::IO::Pic  Module: image I/O for PDL
 grabpic3d       P::G::TriD  Grab a 3D image from the screen.
 rim             P::IO::Pic  Read images in most formats, with improved RGB handling.
 rpic            P::IO::Pic  Read images in many formats with automatic format detection.
 rpiccan         P::IO::Pic  Test which image formats can be read/written
 wim             P::IO::Pic  Write a pdl to an image file with selected type (or using filename extensions)
 wmpeg           P::IO::Pic  Write an image sequence (a (3,x,y,n) byte pdl) as an animation.
 wpic            P::IO::Pic  Write images in many formats with automatic format selection.
 wpiccan         P::IO::Pic  Test which image formats can be read/written

To find all the manuals that come with PDL, try

  apropos 'manual:'

and to get quick info about PDL modules say

  apropos 'module:'

You get more detailed info about a PDL function/module/manual
with the C<help> function

=cut

sub aproposover {
    die "Usage: aproposover \$funcname\n" unless $#_>-1;
    die "no online doc database" unless defined $PDL::onlinedoc;
    my $func = shift;
    $func =~ s:\/:\\\/:g;
    search_docs("m/$func/",['Name','Ref','Module'],1);

}

sub apropos  {
    die "Usage: apropos \$funcname\n" unless $#_>-1;
    die "no online doc database" unless defined $PDL::onlinedoc;
    my $func = shift;
    printmatch aproposover $func;
}

=head2 PDL::Doc::Perldl::search_docs

=for ref

Internal routine to search docs database and autoload files

=cut

sub search_docs {
    my ($func,$types,$sortflag,$exact) = @_;
    my @match;

    @match = $PDL::onlinedoc->search($func,$types,$sortflag);
    push(@match,find_autodoc( $func, $exact ) );

    @match;
}



=head2 PDL::Doc::Perldl::finddoc

=for ref 

Internal interface to the PDL documentation searcher

=cut

sub finddoc  {
    local $SIG{PIPE}= sub {}; # Prevent crashing if user exits the pager

    die 'Usage: doc $topic' unless $#_>-1;
    die "no online doc database" unless defined $PDL::onlinedoc;
    my $topic = shift;

    # See if it matches a PDL function name

    my $subfield = $1
      if( $topic =~ s/\[(\d*)\]$// ); #does it end with a number in square brackets?

    (my $t2 = $topic) =~ s/([^a-zA-Z0-9_])/\\$1/g;  #$t2 is a copy of $topic with escaped non-word characters

    my @match = search_docs("m/^(PDL::)?".$t2."\$/",['Name'],0) ; #matches: ^PDL::topic$ or ^topic$

    unless(@match) {
      
      print "No PDL docs for '$topic'. Using 'whatis'. (Try 'apropos $topic'?)\n\n";
      whatis($topic);
      return;

    }

    # print out the matches

    open my $out, "| pod2text | $PDL::Doc::pager";
    
    if($subfield) {
      if($subfield <= @match) {
	@match = ($match[$subfield-1]);
	$subfield = 0;
      } else {
	print $out "\n\n=head1 PDL HELP: Ignoring out-of-range selector $subfield\n\n=head1\n\n=head1 --------------------------------\n\n";
	$subfield = undef;
      }
    }
    
    my $num_pdl_pod_matches = scalar @match;
    my $pdl_pod_matchnum = 0;

    while (@match) {
       $pdl_pod_matchnum++;

       if (  @match > 1   and   !$subfield  and $pdl_pod_matchnum==1 ) {
          print $out "\n\n=head1 MULTIPLE MATCHES FOR HELP TOPIC '$topic':\n\n=head1\n\n=over 3\n\n";
          my $i=0;
          for my $m ( @match ) {
             printf $out "\n=item [%d]\t%-30s %s%s\n\n", ++$i, $m->[0], $m->[2]{Module} && "in ", $m->[2]{CustomFile} || $m->[2]{Module};
          }
          print $out "\n=back\n\n=head1\n\n To see item number \$n, use 'help ${topic}\[\$n\]'. \n\n=cut\n\n";
       }

       if (@match > 0 and $num_pdl_pod_matches > 1) {
          print $out "\n=head1 Displaying item $pdl_pod_matchnum:\n\n=head1 --------------------------------------\n\n=cut\n\n";
       }

       my $m = shift @match;

       my $Ref = $m->[2]{Ref};
       if ( $Ref && $Ref =~ /^(Module|Manual|Script): / ) {
	   # We've got a file name and we have to open it.  With the relocatable db, we have to reconstitute the absolute pathname.
	   my $relfile = $m->[2]{File};
	   my $absfile = undef;
	   my @scnd = @{$PDL::onlinedoc->{Scanned}};
	   for my $dbf (@scnd) {
	       $dbf = Cwd::abs_path($dbf); # help Debian packaging
	       $dbf =~ s:\/[^\/]*$::; # Trim file name off the end of the database file to get just the directory
	       $dbf .= "/$relfile";
	       $absfile = $dbf if( -e $dbf );
	   }
	   unless ($absfile) {
	       die "Documentation error: couldn't find absolute path to $relfile\n";
	   }
	   open my $in, "<", $absfile;
	   print $out join("",<$in>);
       } else {
          if(defined $m->[2]{CustomFile}) {

             my $parser= Pod::Select->new;
             print $out "=head1 Autoload file \"".$m->[2]{CustomFile}."\"\n\n";
             $parser->parse_from_file($m->[2]{CustomFile},$out);
             print $out "\n\n=head2 Docs from\n\n".$m->[2]{CustomFile}."\n\n";

          } else {

             print $out "=head1 Module ",$m->[2]{Module}, "\n\n";
#	     print STDERR "calling funcdocs(" . $m->[0] . ", " . $m->[1] . ")\n";
             $PDL::onlinedoc->funcdocs($m->[0],$m->[1],$out);

          }

       }
    }
  }


=head2 find_autodoc

=for ref

Internal routine that finds and returns documentation in the
PDL::AutoLoader path, if it exists.

You feed in a topic and it searches for the file "${topic}.pdl".  If
that exists, then the filename gets returned in a match structure
appropriate for the rest of finddoc.

=cut

# Yuck.  Sorry.  At least it works.  -CED

sub find_autodoc {
    my $topic = shift;
    my $exact = shift;
    my $matcher;
    # Fix up regexps and exact matches for the special case of 
    # searching the autoload dirs...
    if($exact) {
	$topic =~ s/\(\)$//;  # "func()" -> "func"
	$topic .= ".pdl" unless $topic =~ m/\.pdl$/;
    } else {

	$topic =~ s:([^\$])(.)$:$1\.\*\$$2:; # Include explicit ".*$" at end of
	                                   # vague matches -- so that we can
	                                   # make it a ".*\.pdl$" below.

	$topic =~ s:\$(.)$:\.pdl\$$1:; # Force ".pdl" at end of file match

	$matcher = eval "sub { ${topic}i && \$\_ };";  # Avoid multiple compiles
    }

    my @out;

    return unless(@main::PDLLIB);
    @main::PDLLIB_EXPANDED = PDL::AutoLoader::expand_path(@main::PDLLIB)
	unless(@main::PDLLIB_EXPANDED);
    
    for my $dir(@main::PDLLIB_EXPANDED) {
	if($exact) {
	    my $file = $dir . "/" . "$topic";
	    push(@out,
	          [$file, undef, {CustomFile => "$file", Module => "file '$file'"}]
		 )
		if(-e $file);
	} else {
	    opendir(FOO,$dir) || next;
	    my @dir = readdir(FOO);
	    closedir(FOO);
	    for my $file( grep( &$matcher, @dir ) ) {
		push(@out,
		     [$file, undef, {CustomFile => "$dir/$file", Module => "file '$dir/$file'"}]
		     );
	    }

	}
    }
    @out;
}


=head2 usage

=for ref

Prints usage information for a PDL function

=for usage

 Usage: usage 'func'

=for example

   pdl> usage 'inner'

   inner           P::Primitive  Inner product over one dimension

   Signature: inner(a(n); b(n); [o]c())



=cut

sub usage {
    die 'Usage: usage $funcname' unless $#_>-1;
    die "no online doc database" unless defined $PDL::onlinedoc;
    print usage_string(@_);
}
sub usage_string{
    my $func = shift;
    my $str = "";
    my @match = search_docs("m/^(PDL::)?$func\$|\:\:$func\$/",['Name']);
    my $count = @match;
    unless ($count) { $str = "\n  no match\n" }
    else {
	#this sorts by namespace depth by counting colons in the name.
	#PDL::Ufunc::max comes before PDL::GSL::RNG::max, for example.
	foreach my $m(sort { scalar(()=$a->[1]=~/\:/g) <=> scalar(()=$b->[1]=~/\:/g) } @match){
	    $str .= "\n" . format_ref( $m );
	    my ($name,$module,$hash) = @{$m};
	    #$str .= sprintf ( (' 'x16)."(Module %s)\n\n", $hash->{Module} );
	    $str.="\n";
	    die "No usage info found for $func\n" if (!defined $hash->{Example} && !defined $hash->{Sig} &&
						      !defined $hash->{Usage});
	    $str .= "  Signature: $name($hash->{Sig})\n\n" if defined $hash->{Sig};
	    for (['Usage','Usage'],['Opt','Options'],['Example','Example']) {
		$str .= "  $_->[1]:\n".&allindent($hash->{$_->[0]},10)."\n" if defined $hash->{$_->[0]};
	    }
	    $str .= '='x20 unless 1==$count--;
	}
    }
    return $str;
}

=head2 sig

=for ref

prints signature of PDL function

=for usage

 sig 'func'

The signature is the normal dimensionality of the
function's arguments.  Calling with different dimensions
doesn't break -- it causes broadcasting.  See L<PDL::PP> and L<PDL::Broadcasting> for details.

=for example

  pdl> sig 'outer'
    Signature: outer(a(n); b(m); [o]c(n,m))

=cut

sub sig {
	die "Usage: sig \$funcname\n" unless $#_>-1;
	die "no online doc database" unless defined $PDL::onlinedoc;
	my $func = shift;
	my @match = search_docs("m/^(PDL::)?$func\$|\:\:$func\$/",['Name']);
	my $count = @match;
	unless (@match) { print "\n  no match\n" } else {
	    foreach my $m(sort { scalar(()=$a->[1]=~/\:/g) <=> scalar(()=$b->[1]=~/\:/g) } @match){
		my ($name,$module,$hash) = @{$m};
		die "No signature info found for $func\n" if !defined $hash->{Sig};
		print "  Signature: $name($hash->{Sig})\n" if defined $hash->{Sig};
		print '='x20 unless 1==$count--;
	    }
	}
}

sub allindent {
	my ($txt,$n) = @_;
	my ($ntxt,$tspc) = ($txt,' 'x8);
	$ntxt =~ s/^\s*$//mg;
	$ntxt =~ s/\t/$tspc/g;
	my $minspc = length $txt;
	for (split '\n', $txt) { if (/^(\s*)/)
          { $minspc = length $1 if length $1 < $minspc } }
	$n -= $minspc;
	$tspc = ' 'x abs($n);
	$ntxt =~ s/^/$tspc/mg if $n > 0;
	return $ntxt;
}


=head2 whatis

=for ref

Describe a perl and/or PDL variable or expression.  Useful for
determining the type of an expression, identifying the keys in a hash
or a data structure, or examining WTF an unknown object is.

=for usage

 Usage: whatis $var
        whatis <expression>

=cut

sub whatis {
  my $topic;

  if(@_ > 1) {
    whatis_r('',0,[@_]);
  } else {
    whatis_r('',0,shift);
  }
}

$PDL::Doc::Perldl::max_strlen = 55;
$PDL::Doc::Perldl::max_arraylen = 1;
$PDL::Doc::Perldl::max_keylen = 8;
$PDL::Doc::Perldl::array_indent=5;
$PDL::Doc::Perldl::hash_indent=3;

sub whatis_r {
  my $prefix = shift;
  my $indent = shift;
  my $x = shift;
  
  unless(defined $x) {
    print $prefix,"<undef>\n";
    return;
  }

  unless(ref $x) {
    print "${prefix}'".
      substr($x,0,$PDL::Doc::Perldl::max_strlen).
      "'".((length $x > $PDL::Doc::Perldl::max_strlen) && '...').
      "\n";
    return;
  }

  if(ref $x eq 'ARRAY') {
    print "${prefix}Array (".scalar(@$x)." elements):\n";

    my($el);
    for $el(0..$#$x) {
      my $pre = sprintf("%s  %2d: "," "x$indent,$el);
      whatis_r($pre,$indent + $PDL::Doc::Perldl::array_indent, $x->[$el]);
      last if($el == $PDL::Doc::Perldl::max_arraylen);
    } 
    printf "%s   ... \n"," " x $indent
      if($#$x > $PDL::Doc::Perldl::max_arraylen);

    return;
  }
      
  if(ref $x eq 'HASH') {
    print "${prefix}Hash (".scalar(keys %$x)." elements)\n";
    my $key;
    for $key(sort keys %$x) {
      my $pre = " " x $indent .
	        " $key: " . 
		(" "x($PDL::Doc::Perldl::max_keylen - length($key))) ;

      whatis_r($pre,$indent + $PDL::Doc::Perldl::hash_indent, $x->{$key});
    }
    return;
  }

  if(ref $x eq 'CODE') {
    print "${prefix}Perl CODE ref\n";
    return;
  }

  if(ref $x eq 'SCALAR' || ref $x eq 'REF') {
    whatis_r($prefix." Ref -> ",$indent+8,$$x);
    return;
  }

  if(UNIVERSAL::can($x,'px')) {
    my $y;
    local $PDL::debug = 1;

    $y = ( (UNIVERSAL::isa($x,'PDL') && $x->nelem < 5 && $x->ndims < 2)
	   ? 
	   ": $x" :
	   ": *****"
	   );

    $x->px($prefix.(ref $x)." %7T (%D) ".$y);

  } else {

    print "${prefix}Object: ".ref($x)."\n";

  }
}

=head2 help

=for ref

print documentation about a PDL function or module or show a PDL manual

In the case of multiple matches, the first command found is printed out,
and the remaining commands listed, along with the names of their modules.


=for usage

 Usage: help 'func'

=for example

 pdl> help 'PDL::Tutorials' # show the guide to PDL tutorials
 pdl> help 'PDL::Slices'    # show the docs in the PDL::Slices module
 pdl> help 'slice'          # show docs on the 'slice' function

=cut

sub help {
  if ($#_>-1) {
      require PDL::Dbg;
      my $topic = shift;
      if (PDL::Core::blessed($topic) && $topic->can('px')) {
	  local $PDL::debug = 1;
	  $topic->px('This variable is');
      } else {
	  $topic = 'PDL::Doc::Perldl' if $topic =~ /^\s*help\s*$/i;
	  if ($topic =~ /^\s*vars\s*$/i) {
	      PDL->px((caller)[0]);
	  } else {
	      finddoc($topic);
	  }
      }
  } else {
	print <<'EOH';

The following commands support online help in the perldl shell:

 help 'thing'   -- print docs on 'thing' (func, module, manual, autoload-file)
 help vars      -- print information about all current ndarrays

 whatis <expr>  -- Describe the type and structure of an expression or ndarray.
 apropos 'word' -- search for keywords/function names 
 usage          -- print usage information for a given PDL function
 sig            -- print signature of PDL function
 badinfo        -- information on the support for bad values

 ('?' is an alias for 'help';  '??' is an alias for 'apropos'.)

Quick start:
  apropos 'manual:' -- Find all the manual documents
  apropos 'module:' -- Quick summary of all PDL modules
  help 'help'       -- details about PDL help system
  help 'perldl'     -- help about this shell

EOH
  }
}

=head2 badinfo

=for ref

provides information on the bad-value support of a function

And has a horrible name.

=for usage

 badinfo 'func'

=for example

  pdl> badinfo 'inner'
  Bad value support for inner (in module PDL::Primitive)
      If "a() * b()" contains only bad data, "c()" is set bad. Otherwise "c()"
      will have its bad flag cleared, as it will not contain any bad values.


=cut

# need to get this to format the output - want a format_bad()
# subroutine that's like - but much simpler - than format_ref()
#
sub badinfo {
    my $func = shift;
    die "Usage: badinfo \$funcname\n" unless defined $func;

    die "no online doc database" unless defined $PDL::onlinedoc;

    local $SIG{PIPE}= sub {}; # Prevent crashing if user exits the pager

    my @match = search_docs("m/^(PDL::)?$func\$|\:\:$func\$/",['Name']);
    my $count = @match;
    if ( $count ) {
	my ($pagerstr, $noinfostr) = ('', '');
	foreach my $m(@match) {
	    my ($name,$module,$hash) = @{$m};
	    my $info = $hash->{Bad};
	    if ( defined $info ) {
		$name=~s/^(.*)\:\:(\w*)$/$2/;

		$pagerstr .= "=head1 Bad value support for $name (in module $module)\n\n$info\n";
	    } else {
		$noinfostr .= "\n  No information on bad-value support found for $func (in module $module)\n";
	    }
	}
	if ($pagerstr){
	    open my $out, "| pod2text | $PDL::Doc::pager";
	    print $out $pagerstr, $noinfostr;
	} else {
	    print $noinfostr;
	}
    } else {
	print "\n  no match\n";
    }
} # sub: badinfo()

1; # OK
