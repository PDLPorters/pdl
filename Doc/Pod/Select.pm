#############################################################################
# Select.pm -- function to select portions of pod docs
#
# Based on Tom Christiansen's pod2text() function
# (with extensive modifications).
#
# Copyright (C) 1996 Tom Christiansen. All rights reserved.
# This file is part of "PodParser". PodParser is free software;
# you can redistribute it and/or modify it under the same terms
# as Perl itself.
#############################################################################

package PDL::Pod::Select;

$VERSION = 1.00;   ## Current version of this package
require  5.002;    ## requires Perl version 5.002 or later

=head1 NAME

podselect - function to extract selected sections of pod documentation

=head1 SYNOPSIS

    use PDL::Pod::Select;
    podselect (@filelist);
    podselect ({OUTPUT => "tmp.out"}, @filelist):
    podselect ({SELECT => ["NAME|SYNOPSIS", "OPTIONS"]}, @filelist):
    podselect ({OUTPUT => ">&STDERR", SELECT => ["DESCRIPTION"]}, "-");

=head1 DESCRIPTION

B<podselect()> is a function which will extract specified sections of
pod documentation from an input stream. This ability is already provided
in the B<PDL::Pod::Parser> module. Subclasses of B<PDL::Pod::Parser> that wish to
take advantage of this feature do I<not> need to derive from
B<PDL::Pod::Select>. B<PDL::Pod::Select> merely provides a single function named
B<podselect()> which provides this capability in function form (as
opposed to object form) for extracting the raw pod docs.

=cut

#############################################################################

use Exporter ();
use PDL::Pod::Parser;
@ISA = qw(Exporter);
@EXPORT = qw(&podselect);

use strict;
use diagnostics;
use Carp;

sub version {
    no strict;
    return  $VERSION;
}


=head2 podselect(\%options, @filelist)

B<podselect> will print the raw (untranslated) pod documentation of all
pod sections in the given input files specified by C<@filelist>
according to the given options.

If any argument to B<podselect> is a reference to a hash
(associative array) then the values with the following keys are
processed as follows:

=over 4

=item C<OUTPUT>

A string corresponding to the desired output file (or ">&STDOUT"
or ">&STDERR"). The default is to use standard output.

=item C<SELECT>

A reference to an array of sections specifications (as described in
L<PDL::Pod::Parser/"SECTION SPECIFICATIONS">) which indicate the desired set of pod
sections and subsections to be selected from input. If no section
specifications are given, then all sections of pod documentation are
used.

=back

All other arguments should correspond to the names of input files
containing pod documentation. A file name of "-" or "<&STDIN" will
be interpeted to mean standard input (which is the default if no
filenames are given).

=cut

sub podselect {
    my(@argv) = @_;
    my (@sections, $output);
    my $pod_parser = new PDL::Pod::Parser;
    my $num_inputs = 0;
    local($_);
    for (@argv) {
        if (ref($_)) {
            next unless (ref($_) eq 'HASH');
            $output = $_->{OUTPUT}  if (defined $_->{OUTPUT});
            if ((defined $_->{SELECT}) && (ref($_->{SELECT}) eq 'ARRAY')) {
                $pod_parser->select(@{$_->{SELECT}});
            }
        }
        else {
            $pod_parser->parse_from_file($_, $output);
            ++$num_inputs;
        }
    }
    $pod_parser->parse_from_file("-")  unless ($num_inputs > 0);
}

=head1 SEE ALSO

L<PDL::Pod::Parser>

=head1 AUTHOR

Brad Appleton E<lt>Brad_Appleton-GBDA001@email.mot.comE<gt>

Based on code for B<pod2text> written by
Tom Christiansen E<lt>tchrist@mox.perl.comE<gt>

=cut

1;
