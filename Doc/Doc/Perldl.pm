=head1 NAME

PDL::Doc::Perldl - commands for accessing PDL doc database from 'perldl' shell

=head1 DESCRIPTION

This module provides a simple set of functions to
access the PDL documentation of database, for use
from the I<perldl> shell and the I<pdldoc> command-line
program.

Currently, multiple matches are not handled very well.

=head1 SYNOPSIS

 use PDL::Doc::Perldl; # Load all documenation functions

=head1 FUNCTIONS

=cut

package PDL::Doc::Perldl;

use Exporter;
use strict;
use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter);

@EXPORT = qw( apropos aproposover usage help sig badinfo );

use PDL::Doc;
use IO::File;
use Pod::Text;

$PDL::onlinedoc = undef;
$PDL::onlinedoc = new PDL::Doc (FindStdFile());

use PDL::Config;
my $bvalflag = $PDL::Config{WITH_BADVAL} || 0;

# pod commands are stripped from the ref string before printing.
# How we do this depends on the version of Pod::Text installed.
#
# I'm guessing the difference in behaviour is between versions
# 1 and 2 of Pod::Text (it's certainly true for 
# version 1.0203 (perl5.005_03) and 2.03 (perl 5.6.0))
#
# version 1:
#  we use a private routine from Pod::Text
#  (prepare_for_output) in printmatch() in order
#  to strip away pod directives from the ref
#  string
#
# version 2: (Thanks to Tim Jenness)
#  create an object and use the interpol() method
#

# Find std file

sub FindStdFile {
  my ($d,$f);
  for $d (@INC) {
      $f = $d."/PDL/pdldoc.db";
      if (-f $f) {
         print "Found docs database $f\n" if $PDL::verbose;
	 print "Type 'help' for online help\n" if $PDL::verbose;
         return $f;
      }
  }
  warn "Unable to find PDL/pdldoc.db in ".join(":",@INC)."\n";
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
    return $ENV{COLUMNS}
       || (($ENV{TERMCAP} =~ /co#(\d+)/) and $1)
       || ($^O ne 'MSWin32' and $^O ne 'dos' and 
	   (`stty -a 2>/dev/null` =~ /columns\s*=?\s*(\d+)/) and $1)
       || 72;
}

# the $^W assignment stops Pod::Text::fill() from 
# generating "Use of uninitialised values" errors
#
sub printmatch {
    my @match = @_;
    if (@match) {
	foreach my $t ( format_ref( @_ ) ) { print $t; }
    } else {
	print "no match\n\n";
    }
} # sub: print_match()

# return a string containing a formated version of the Ref string
# for the given matches
#
sub format_ref {
    my @match = @_;
    my @text = ();

    # XXX this is NASTY
    my $width = screen_width()-17;
    if ( $Pod::Text::VERSION < 2 ) {
	$Pod::Text::indent = 0;
	$Pod::Text::SCREEN = $width;
	local $^W = 0;
	for my $m (@match) { 
	    $_ = $m->[1]->{Ref} || "[No reference available]";
	  Pod::Text::prepare_for_output(); # adds a '\n' to $_
	    $_ = Pod::Text::fill $_; # try and get `nice' wrapping 
	    s/\n*$//; # remove last new lines (so substitution doesn't append spaces at end of text)
	    s/\n/\n                /g;
	    my $name = $m->[0];
	    if ( length($name) > 15 ) { 
	        push @text, sprintf "%s ...\n                %s\n", $name, $_; 
	    } else {
		push @text, sprintf "%-15s %s\n", $name, $_; 
	    }
	}
    } else {
	my $parser = new Pod::Text( width => $width, indent => 0, sentence => 0 );
	
	for my $m (@match) { 
	    my $ref = $m->[1]->{Ref} || "[No reference available]";
	    $ref = $parser->interpolate( $ref );
	    $ref = $parser->reformat( $ref );
	    
	    # remove last new lines (so substitution doesn't append spaces at end of text)
	    $ref =~ s/\n*$//; 
	    $ref =~ s/\n/\n                /g;

	    my $name = $m->[0];
	    if ( length($name) > 15 ) { 
		push @text, sprintf "%s ...\n                %s\n", $name, $ref; 
	    } else {
		push @text, sprintf "%-15s %s\n", $name, $ref; 
	    }
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

 perldl> apropos 'pic'
 rpic            Read images in many formats with automatic format detection.
 rpiccan         Test which image formats can be read/written
 wmpeg           Write an image sequence ((x,y,n) piddle) as an MPEG animation.
 wpic            Write images in many formats with automatic format selection.
 wpiccan         Test which image formats can be read/written

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
    return $PDL::onlinedoc->search($func,['Name','Ref','Module'],1);
}

sub apropos  {
    die "Usage: apropos \$funcname\n" unless $#_>-1;
    die "no online doc database" unless defined $PDL::onlinedoc;
    my $func = shift;
    printmatch aproposover $func;
}

sub finddoc  {
    die 'Usage: doc $topic' unless $#_>-1;
    die "no online doc database" unless defined $PDL::onlinedoc;
    my $topic = shift;

    # See if it matches a PDL function name
    my @match = $PDL::onlinedoc->search("m/^(PDL::)?$topic\$/",['Name']);

    die "Unable to find PDL docs on $topic\n"
	if $#match == -1;

    # print out the matches
    # - do not like this solution when have multiple matches
    #   but looping through each match didn't seem right either
    my $m = shift @match;
    my $Ref = $m->[1]{Ref};
    if ( $Ref =~ /^(Module|Manual|Script): / ) {
	system("pod2text $m->[1]{File} | $PDL::Doc::pager");
    } else {
	my $out = IO::File->new( "| pod2text | $PDL::Doc::pager" );
	print $out "=head1 Module\n\n",$m->[1]{Module}, "\n\n";
	$PDL::onlinedoc->funcdocs($m->[0],$out);
    }
    if ( $#match > -1 ) {
	print "\nFound other matches for $topic:\n";
	foreach my $m ( @match ) {
	    printf "  %-30s in %s\n", $m->[0], $m->[1]{Module};
	}
    }
}

=head2 usage

=for ref

Prints usage information for a PDL function

=for usage

 Usage: usage 'func'

=for example

   perldl> usage 'inner'

   inner           inner prodcuct over one dimension
                   (Module PDL::Primitive)

   Signature: inner(a(n); b(n); [o]c(); )


=cut

sub usage {
    die 'Usage: usage $funcname' unless $#_>-1;
    die "no online doc database" unless defined $PDL::onlinedoc;
    print usage_string(@_);
}
sub usage_string{
    my $func = shift;
    my $str = "";
    my @match = $PDL::onlinedoc->search("m/^(PDL::)?$func\$/",['Name']);
    unless (@match) { print "\n  no match\n" } 
    else {
	$str .= "\n" . format_ref( $match[0] );
	my ($name,$hash) = @{$match[0]};
	$str .= sprintf ( (' 'x16)."(Module %s)\n\n", $hash->{Module} );
	die "No usage info found for $func\n"
	    if !defined $hash->{Example} && !defined $hash->{Sig} &&
		!defined $hash->{Usage};
	$str .= "  Signature: $name($hash->{Sig})\n\n" if defined $hash->{Sig};
	for (['Usage','Usage'],['Opt','Options'],['Example','Example']) {
	    $str .= "  $_->[1]:\n\n".&allindent($hash->{$_->[0]},10)."\n\n"
		if defined $hash->{$_->[0]};
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
functions arguments. Calling with different dimensions
causes 'threading' - see C<PDL::PP> for more details.

=for example

  perldl> sig 'outer'
    Signature: outer(a(n); b(m); [o]c(n,m); )


=cut

sub sig {
	die "Usage: sig \$funcname\n" unless $#_>-1;
	die "no online doc database" unless defined $PDL::onlinedoc;
	my $func = shift;
	my @match = $PDL::onlinedoc->search("m/^(PDL::)?$func\$/",['Name']);
	unless (@match) { print "\n  no match\n" } else {
         my ($name,$hash) = @{$match[0]};
	 die "No signature info found for $func\n"
            if !defined $hash->{Sig};
         print "  Signature: $name($hash->{Sig})\n" if defined $hash->{Sig};
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


=head2 help

=for ref

print documentation about a PDL function or module or show a PDL manual

In the case of multiple matches, the first command found is printed out,
and the remaining commands listed, along with the names of their modules.

=for usage

 Usage: help 'func'

=for example

 perldl> help 'PDL::Slices'   # show the docs in the PDL::Slices module
 perldl> help 'PDL::Intro'    # show the PDL::Intro manual
 perldl> help 'slice'         # show docs on the 'slice' function

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

The following four commands support online help in the perldl shell:

  help            -- print this text
  help 'thing'    -- print the docs on 'thing' (can be function/module/manual)
  help $a         -- print information about $a (if it's a piddle)
  help vars       -- print information about all current piddles
  apropos 'word'  -- search for keywords/function names in the list of
                     documented PDL functions
  ?		  -- alias for 'help'
  ??		  -- alias for 'apropos'
  usage           -- print usage information for a given PDL function
  sig             -- print signature of PDL function
EOH

print "  badinfo         -- information on the support for bad values\n"
   if $bvalflag;

print <<'EOH';

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

=cut

# need to get this to format the output - want a format_bad()
# subroutine that's like - but much simpler - than format_ref()
#
sub badinfo {
    my $func = shift;
    die "Usage: badinfo \$funcname\n" unless defined $func;

    die "PDL has not been compiled with support for bad values.\n" .
	"Recompile with WITH_BADVAL set to 1 in config file!.\n"
	    unless $bvalflag;

    die "no online doc database" unless defined $PDL::onlinedoc;

    my @match = $PDL::onlinedoc->search("m/^(PDL::)?$func\$/",['Name']);
    if ( @match ) {
	my ($name,$hash) = @{$match[0]};
	my $info = $hash->{Bad};

	if ( defined $info ) {
	    my $out = new IO::File "| pod2text | $PDL::Doc::pager";
	    print $out "=head1 Bad value support for $name\n\n$info\n";
	} else {
	    print "\n  No information on bad-value support found for $func\n";
	}
    } else {
	print "\n  no match\n";
    }
} # sub: badinfo()

1; # OK
