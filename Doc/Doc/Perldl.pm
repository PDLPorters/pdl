=head1 NAME

PDL::Doc::Perldl - commands for accessing PDL doc database from 'perldl' shell

=head1 DESCRIPTION

This module provides a simple set of functions to
access the PDL documentation of database, for use
from the 'perldl' shell.

=head1 SYNOPSIS

 use PDL::Doc::Perldl; # Load all documenation functions

=head1 FUNCTIONS

=cut

package PDL::Doc::Perldl;

use Exporter;
use strict;
use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter);

@EXPORT = qw( apropos usage help sig );

use PDL::Doc;
use IO::File;

$PDL::onlinedoc = undef;
$PDL::onlinedoc = new PDL::Doc (FindStdFile());

# Find std file

sub FindStdFile {
  my ($d,$f);
  for $d (@INC) {
      $f = $d."/PDL/pdldoc.db";
      if (-f $f) {
         print "Found docs database $f\n"; # if $PDL::debug;
	 print "Type 'help' for online help\n"; # if $PDL::debug;
         return $f;
      }
  }
  warn "Unable to find PDL/pdldoc.db in ".join(":",@INC)."\n";
}

sub printmatch {
  my @match = @_;
  unless (@match) {
    print "no match\n\n";
  } else {
    for (@match) { printf "%-15s %s\n", $_->[0], $_->[1]->{Ref}}
  }
}

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

sub apropos  {
	die "Usage: apropos \$funcname\n" unless $#_>-1;
	die "no online doc database" unless defined $PDL::onlinedoc;
	my $func = shift;
	my @match = $PDL::onlinedoc->search($func,['Name','Ref','Module'],1);
	printmatch @match;
}


sub finddoc  {
	die 'Usage: doc $topic' unless $#_>-1;
	die "no online doc database" unless defined $PDL::onlinedoc;
	my $topic = shift;

	# See if it matches a PDL function name

	my @match = $PDL::onlinedoc->search("m/^(PDL::)?$topic\$/",['Name']);
	if (@match) {
	   my $Ref = $match[0]->[1]->{Ref};
	   if ( $Ref =~ /^Module: / || $Ref =~ /^Manual: /) {
	       system("pod2text $match[0]->[1]->{File} | $PDL::Doc::pager");
	       return;
	   }
	   my $out = new IO::File "| pod2text | $PDL::Doc::pager";
	   print $out "=head1 Module\n\n",$match[0]->[1]->{Module}, "\n\n";
	   $PDL::onlinedoc->funcdocs($match[0]->[0],$out);
	}
	else {
	  die "Unable to find PDL docs on $topic\n";
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
    unless (@match) { print "\n  no match\n" } else {
     my ($name,$hash) = @{$match[0]};
     $str .= sprintf "\n%-15s %s \n".(' 'x16)."(Module %s)\n\n", $name,
    	    $hash->{'Ref'}, $hash->{'Module'};
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
	  if ($topic =~ /^\s*perldl\s*$/i) {
	      system("pod2text $0 | $PDL::Doc::pager\n");
	  } elsif ($topic =~ /^\s*vars\s*$/i) {
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

  Quick start:

  apropos 'manual:' -- Find all the manual documents
  apropos 'module:' -- Quick summary of all PDL modules
  help 'help'       -- details about PDL help system
  help 'perldl'     -- help about this shell

EOH
  }
}

1; # OK
