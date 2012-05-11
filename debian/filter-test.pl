#!/usr/bin/perl
use strict;
use warnings;
my $hot=0;
my $last_enter_dir;
while (<>) {
	if (/^make\[\d+\]: Entering directory/) {
		$last_enter_dir=$_;
		$hot=0;
		next;
	}
	if (/\"test_harness\(/) {
		$hot=1;
		print $last_enter_dir;
	}
	if ($hot) {
		print;
		if (/^Result:/) {
			$hot=0;
			next;
		}
	}
}
