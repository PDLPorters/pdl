=head1 NAME

PDL::Dbg - functions to support debugging of PDL scripts

=head1 SYNOPSIS

       use PDL;
       use PDL::Dbg;

       $c = $a->slice("5:10,2:30")->px->diagonal(3,4);
       PDL->px;

=head1 DESCRIPTION

This packages implements a couple of functions that should come in
handy when debugging your PDL scripts. They make a lot of sense while
you're doing rapid prototyping of new PDL code, let's say inside the
perldl shell.

=cut

#' fool emacs

package PDL::Dbg;

# used by info
$PDL::Dbg::Title = "Type   Dimension       Flow  State          Mem";
$PDL::Dbg::Infostr = "%6T %-15D  %3F   %-5S  %12M";

package PDL;

=head1 FUNCTIONS

=head2 px

=for ref

Print info about a piddle (or all known piddles)

=for example

    perldl> PDL->px
    perldl> $b += $a->clump(2)->px('clumptest')->sumover
    perldl> $a->px('%C (%A) Type: %T')

This function prints some information about piddles. It can be invoked
as a class method (e.g. C<PDL-E<gt>px> ) or as an instance method (e.g.
C<$pdl-E<gt>px($arg)>). If

=over 2

=item invoked as a class method

it prints info about all piddles found in the current package
(I<excluding> C<my> variables). This comes in quite handy when you are
not quite sure which pdls you have already defined, what data they
hold , etc. C<px> is supposed to support inheritance and prints info
about all symbols for which an C<isa($class)> is true. An optional
string argument is interpreted as the package name for which to print
symbols:

  perldl> PDL->px('PDL::Mypack')

The default package is that of the caller.

=item invoked as an instance method

it prints info about that particular piddle if C<$PDL::debug> is
true and returns the pdl object upon completion. It accepts an
optional string argument that is simply prepended to the default info
if it doesn't contain a C<%> character. If, however, the argument
contains a C<%> then the string is passed to the C<info> method to
control the format of the printed information. This can be used to
achieve customized output from C<px>. See the documentation of
C<PDL::info> for further details.

=back

The output of px will be determined by the default formatting string
that is passed to the C<info> method (unless you pass a string
containing C<%> to px when invoking as an instance method, see
above). This default string is stored in C<$PDL::Dbg::Infostr> and the
default output format can be accordingly changed by setting this
variable.  If you do this you should also change the default title
string that the class method branch prints at the top of the listing
to match your new format string. The default title is stored in the
variable C<$PDL::Dbg::Title>.

For historical reasons C<vars> is an alias for C<px>.

=cut

sub px {
  my $arg = shift;
  my $str="";

  if (ref($arg)) {
    return $arg unless $PDL::debug;
    my $info = $arg->info($#_ > -1 ? ($_[0] =~ /%/ ?
			 $_[0] : "$_[0] $PDL::Dbg::Infostr") :
			  $PDL::Dbg::Infostr);
    print "$info\n";
    return $arg;
  }

  # we have been called as a class method
  my $package = $#_ > -1 ? shift : caller;
  my $classname = $arg;
  # find the correct package
  $package .= "::" unless $package =~ /::$/;
  *stab = *{"main::"};
  while ($package =~ /(\w+?::)/g){
    *stab = $ {stab}{$1};
  }
  print "$classname variables in package $package\n\n";
  my $title = "Name         $PDL::Dbg::Title\n";
  print $title;
  print '-'x(length($title)+3)."\n";
  my ($pdl,$npdls,$key,$val,$info) = ((),0,"","","");
  while (($key,$val) = each(%stab)) {
    $pdl = ${"$package$key"};
    # print info for all objects derived from this class
    if (isa($pdl,$classname)) {
      $npdls++;
      $info = $pdl->info($PDL::Dbg::Infostr);
      printf "\$%-11s %s %s\n",$key,$info,(ref($pdl) eq $classname ? '' :
					 ref($pdl));
      # also print classname for derived classes
    }
  }
  print "no $classname objects in package $package\n"
       unless $npdls;
  return $arg;
}

=head2 vars

=for ref

Alias for C<px>

=cut

# make vars an alias
# I hope this works with inheritance
*vars = \&px;

1; # return success

=head1 BUGS

There are probably some. Please report if you find any. Bug reports
should be sent to the PDL mailing list perldl@jachw.hawaii.edu.

=head1 AUTHOR

Copyright(C) 1997 Christian Soeller (c.soeller@auckland.ac.nz).
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


