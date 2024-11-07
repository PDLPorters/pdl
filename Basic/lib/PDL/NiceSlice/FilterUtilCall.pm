# This original Filter::Util::Call-based
# PDL::NiceSlice engine.
#
use strict;
use warnings;
use Filter::Util::Call;

{
no warnings 'redefine';
sub PDL::NiceSlice::FilterUtilCall::make_filter {
  my ($class,$file,$offset) = @_;
  my $terminator = terminator_regexp($class);
  sub {
    my ($status, $off, $end);
    my $count = 0;
    my $data = "";
        while ($status = filter_read()) {
                return $status if $status < 0;
                if (defined($terminator) && m/$terminator/) {
                        $off=1;
                        last;
                }
                if (m/^\s*(__END__|__DATA__)\s*$/) {
                  $end=$1; $off = 1;
                  last;
                }
                $data .= $_;
                $count++;
                $_ = "";
        }
        $_ = $data;
        $_ = findslice $_ unless $status < 0; # the actual filter
        $_ .= "no $class;\n" if $off;
        $_ .= "$end\n" if $end;
        return $count;
  };
}
}

sub import {
    my ($class) = @_;
    my ($file,$offset) = (caller)[1,2];  # for error reporting
    ## Parse class name into a regexp suitable for filtration
    filter_add(PDL::NiceSlice::FilterUtilCall::make_filter($class, $file, $offset+1));
}

sub unimport {
  filter_del();
}

1;
