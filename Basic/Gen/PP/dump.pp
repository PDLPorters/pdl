# These are suspended for now...

# use blib; # For Types.pm
# require './PP.pm';

open PP, "PP.pm" or die "can't open PP.pm";
$str = join '',<PP>;
$str =~ m|\@PDL::PP::EXPORT\s*=\s*qw/([^/]*)/|s;
$str = $1; # Get the contents of the qw//


$pm = '
=head1 NAME

PDL::PP::Dump -- dump pp_xxx calls to stdout

=head1 SYNOPSIS

   perl -MPDL::PP::Dump Basic/Ops/ops.pd

=head1 DESCRIPTION

The most basic PP script debugger thinkable.

=head1 AUTHOR

Christian Soeller <csoelle@sghms.ac.uk> .

=cut

package PDL::PP::Dump;

use Exporter;
@ISA = Exporter;

@EXPORT = qw('.$str.q|);

for (@EXPORT) {
  if ($_ !~ /pp_def/) {
    my $def = "sub $_ { print '".$_.q/('.(join ',',map("'$_'",@_)).");\n" }/;
#    print "defining =>\n$def\n";
    eval($def);
  }
}

sub pp_def {
   my($name,%hash) = @_;

   print "pp_def('$name',\n";
     foreach (keys(%hash)) {
       if ($_ =~ /(Generic)*Types/) {
        print "$_ => [" . join(',',@{$hash{$_}}) . "]\n";
       } else {
        print "$_ =>\n'".$hash{$_}."',\n";
       }
     }
  print ");\n";
}

1;
|;

print $pm;

