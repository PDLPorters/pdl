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

Christian Soeller <c.soeller@auckland.ac.nz> .

=cut

package PDL::PP::Dump;

use Exporter;
@ISA = Exporter;

@EXPORT = qw('.$str.q|);

my $typecheck =0;

sub import {
	my ($pack,$arg) = @_;
	$typecheck =1 if defined $arg && $arg =~ /^typecheck$/i;
        @_ = ($pack);
        goto &Exporter::import;
}
	
sub printargs {
  my $name = shift;
  print "$name(";
  print join ',',map("'$_'",@_);
  print ");\n";
}

for (@EXPORT) {
  if ($_ !~ /pp_def/) {
    my $def = "sub $_ { printargs($_,\@_) unless \$typecheck }";
    # print "defining =>\n$def\n";
    eval($def);
  }
}

sub pp_def {
   my($name,%hash) = @_;
   use PDL::Types ':All';

   if ($typecheck) {
    my @alltypes = ppdefs; my $jointypes = join '',@alltypes;
    my $types = exists $hash{GenericTypes} ? $hash{GenericTypes} : [@alltypes];
    for my $key (qw/Code BackCode/) {
      if (exists $hash{$key}) {
         while ($hash{$key} =~ s/\$T([a-zA-Z]+)\s*\(([^)]*)\)//) {
           my ($mactypes,$alternatives) = ($1,$2);
           # print "type macro ($mactypes) in $name\n";
	   my @mactypes = split '', $mactypes;
	   print "$name has extra types in macro: $mactypes vs $jointypes\n"
	     unless $mactypes =~ /^\s*[$jointypes]+\s*$/;
	   for my $gt (@$types) {
             print "$name has no Macro for generic type $gt (has $mactypes)"
	      unless grep {$gt eq $_} @mactypes;
	   }
         }
      }
    }   
   } else {
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
}

1;
|;

print $pm;

