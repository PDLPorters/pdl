# Scripts to test PDL memory handling for leaks.

# Replace DPERL by the name of your debugging perl (built with MSTATS)
# executable.

$DPERL = "/usr/bin/dperl";

sub memtest;

memtest "JUSTPERL",<<'END';
{
	my $a = "sljslfsjeflsejflisejflseijfljesfjsefs" x 10;
	my $b = $a . "foo";
}
END

memtest "ALLOCONE",<<'END';
	my $a = zeroes(100,100);
END

memtest "ADDSAME",<<'END';
	my $a = zeroes(100,100);
	my $b = $a + $a;
END

memtest "ADDONE",<<'END';
	my $a = zeroes(100,100);
	my $b = $a + 1;
END

memtest "ADDONE+AT",<<'END';
	my $a = zeroes(50,50);
	my $b = $a + 1;
	my $c = $b->at(5,5);
END

sub memtest {
	my($name,$scr) = @_;
	my @res;
	for(1,51,101) {
	my $res;
		print "$name ROUND $_\n";
	open FILE, ">tmpscript";
	print FILE "BEGIN{print `pwd`;};\$|=1; use blib '../..'; use PDL; 
	  for(\$i = 0; \$i < $_; \$i++) { $scr } print \"FINISHED\\n\";
	";
	close FILE;
	$ENV{PERL_DEBUG_MSTATS}=2;
	open(PIPE,"dperl tmpscript 2>&1 |")
		or die "Couldn't open pipe";
	{ local $_; while(<PIPE>) { $res .= $_ }; }
	close PIPE;
#	print "RESULT: $res ENDRESULT\n";;
	$res =~ /FINISHED/ or die "Couldn't run script!";
	push @res,$res;
	}
	my $tres = join '', 
	map {
		/(Memory allocation statistics after execu.*Total sb[^\n]*$)/s
		 or die "Output $_ doesn't match pattern\n";
		my $str = $1;
		$str =~ /\n([^\n]*used[^\n]*\n)/m
		 or die "Output $str doesn't match pattern2\n";
		$1;
	} @res;
	print "-----------------------\nRES $name:\n$tres\n";
}

