# This original Filter::Util::Call-based
# PDL::NiceSlice engine.
#
use Filter::Util::Call;

##############################
# If you mess with the import filter, please also change the pdlpp importer 
# just above this comment!  They both do similar things, but one to an eval string
# and one to an import file.
#   --CED 5-Nov-2007
#
sub import {
    my ($class) = @_;
    ($file,$offset) = (caller)[1,2];  # for error reporting
    $offset++;
    
    ## Parse class name into a regexp suitable for filtration
    my $terminator = terminator_regexp($class);

    filter_add(
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
		}
	);
}

sub unimport {
  filter_del();
}

1;
