#############################################################################
# Parser.pm -- package which defines a base class for parsing pod docs.
#
# Based on Tom Christiansen's Pod::Text module
# (with extensive modifications).
#
# Copyright (C) 1996 Tom Christiansen. All rights reserved.
# This file is part of "PodParser". PodParser is free software;
# you can redistribute it and/or modify it under the same terms
# as Perl itself.
#############################################################################

package PDL::Pod::Parser;

$VERSION = 1.00;   ## Current version of this package
require  5.002;    ## requires Perl version 5.002 or later

=head1 NAME

PDL::Pod::Parser - base class for creating pod filters and translators

=head1 SYNOPSIS

    use PDL::Pod::Parser;
    package PDL::MyParser;
    @ISA = qw(PDL::Pod::Parser);

    sub new {
        ## constructor code ...
    }

    ## implementation of appropriate subclass methods ...

    package main;
    $parser = new PDL::MyParser;
    @ARGV = ('-')  unless (@ARGV > 0);
    for (@ARGV) {
        $parser->parse_from_file($_);
    }

=head1 DESCRIPTION

B<PDL::Pod::Parser> is an abstract base class for implementing filters and/or
translators to parse pod documentation into other formats. It handles
most of the difficulty of parsing the pod sections in a file and leaves
it to the subclasses to override various methods to provide the actual
translation. The other thing that B<PDL::Pod::Parser> provides is the ability
to process only selected sections of pod documentation from the input.

=head2 SECTION SPECIFICATIONS

Certain methods and functions provided by B<PDL::Pod::Parser> may be given
one or more "section specifications" to restrict the text processed to
only the desired set of sections and their corresponding subsections.  A
section specification is a string containing one or more Perl-style
regular expressions separated by forward slashes ("/").  If you need to
use a forward slash literally within a section title you can escape it
with a backslash ("\/").

The formal syntax of a section specification is:

=over 4

=item
I<head1-title-regexp>/I<head2-title-regexp>/...

=back

Any omitted or empty regular expressions will default to ".*".
Please note that each regular expression given is implicitly
anchored by adding "^" and "$" to the beginning and end.  Also, if a
given regular expression starts with a "!" character, then the
expression is negated (so C<!foo> would match anything I<except>
C<foo>).

Some example section specifications follow.

=over 4

=item
Match the C<NAME> and C<SYNOPSIS> sections and all of their subsections:

C<NAME|SYNOPSIS>

=item
Match only the C<Question> and C<Answer> subsections of the C<DESCRIPTION>
section:

C<DESCRIPTION/Question|Answer>

=item
Match the C<Comments> subsection of I<all> sections:

C</Comments>

=item
Match all subsections of C<DESCRIPTION> I<except> for C<Comments>:

C<DESCRIPTION/!Comments>

=item
Match the C<DESCRIPTION> section but do I<not> match any of its subsections:

C<DESCRIPTION/!.+>

=item
Match all top level sections but none of their subsections:

C</!.+>

=back

=cut

#############################################################################

use strict;
use Carp;

## Maximum number of heading levels supported for '=headN' directives
$PDL::Pod::Parser::MAX_HEAD_LEVEL = 3;

##---------------------------------------------------------------------------

=head1 FUNCTIONS

B<PDL::Pod::Parser> provides the following functions (please note that these
are functions and I<not> methods, they do not take an object reference
as an implicit first parameter):

=cut

##---------------------------------
## Function definitions begin here
##---------------------------------

=head2 version()

Return the current version of this package.

=cut

sub version {
    no strict;
    return  $VERSION;
}

##---------------------------------------------------------------------------

=head1 INSTANCE METHODS

B<PDL::Pod::Parser> provides several methods, some of which should be
overridden by subclasses.  They are as follows:

=cut

##-------------------------------
## Method definitions begin here
##-------------------------------

=head2 new()

This is the the constructor for the base class. You should only use it
if you want to create an instance of a B<PDL::Pod::Parser> instead of one of
its subclasses. The constructor for this class and all of its subclasses
should return a blessed reference to an associative array (hash).

=cut

sub new {
    my $this = shift;
    my $class = ref($this) || $this;
    my %params = @_;
    my $self = {%params};
    bless $self, $class;
    $self->initialize();
    return $self;
}

=head2 initialize()

This method performs any necessary base class initialization.
It takes no arguments (other than the object instance of course).
If subclasses override this method then they I<must> be sure to
invoke the superclass' B<initialize()> method.

=cut

sub initialize {
    my $self = shift;
    return;
}

=head2 select($section_spec1, $section_spec2, ...)

This is the method that is used to select the particular sections and
subsections of pod documentation that are to be printed and/or
processed. If the I<first> I<argument> is the string "+", then the
remaining section specifications are I<added> to the current list of
selections; otherwise the given section specifications will I<replace>
the current list of selections.

Each of the C<$section_spec> arguments should be a section
specification as described in L<"SECTION SPECIFICATIONS">.  The section
specifications are parsed by this method and the resulting regular
expressions are stored in the array referenced by
C<$self-E<gt>{SELECTED}> (please see the description of this member
variable in L<"INSTANCE DATA">).

This method should I<not> normally be overridden by subclasses.

=cut

sub select {
    my $self = shift;
    my @sections = @_;
    my $add = ($sections[0] eq "+") ? shift(@sections) : "";
    ## reset the set of sections to use
    unless (@sections > 0) {
        undef $self->{SELECTED}  unless ($add);
        return;
    }
    $self->{SELECTED} = []  unless ($add  &&  defined $self->{SELECTED});
    local($_);
    my ($spec, $errors);
    my (@re, $i, $negated, %exprs);
    ## Parse each spec
    for $spec (@sections) {
        $_ = $spec;
        s|\\\\|\001|go;  ## handle escaped backward slashes
        s|\\/|\002|go;   ## handle escaped forward slashes
        ## Extract the regexps for the heading titles
        @re = split('/', $_, $PDL::Pod::Parser::MAX_HEAD_LEVEL);
        ## Modify the regexps as needed and check their syntax
        for ($i = 0; $i < $PDL::Pod::Parser::MAX_HEAD_LEVEL; ++$i) {
            $re[$i]  = '.*'  if ((! defined $re[$i]) || $re[$i] eq "");
            $re[$i] .= '.+'  if ($re[$i] eq '!');
            ## Put back any escape sequences we "handled"
            $re[$i] =~ s|\001|\\\\|go;
            $re[$i] =~ s|\002|\\/|go;
            ## Check for negation
            $negated = ($re[$i] =~ /^\!/o);
            $re[$i] = $'  if ($negated);
            ## Check regexp syntax
            eval "/$re[$i]/";
            if ($@) {
                ++$errors;
                carp "Invalid regular expression /$re[$i]/ in \"$spec\": $@\n";
            }
            else {
                ## Add the forward and rear anchors (and put the negator back)
                $re[$i] = '^' . $re[$i]  unless ($re[$i] =~ /^\^/o);
                $re[$i] = $re[$i] . '$'  unless ($re[$i] =~ /\$$/o);
                $re[$i] = '!' . $re[$i]  if ($negated);
            }
        }
        if ($errors) {
            carp "Ignoring section spec \"$spec\"!\n";
        }
        else {
            ## Store them in our sections array
            push(@{$self->{SELECTED}}, [ @re ]);
        }
    }
}

=head2 want_section($head1_title, $head2_title, ...)

Returns a value of true if the given section and subsection titles match
any of the section specifications passed to the B<select()> method (or
if no section specifications were given). Returns a value of false
otherwise. If C<$headN_title> is ommitted then it defaults to the current
C<headN> section title in the input.

This method should I<not> normally be overridden by subclasses.

=cut

sub want_section {
    my $self = shift;
    my (@heads) = @_;
    ## Return true if no restrictions were explicitly specified
    return  1  unless ((defined $self->{SELECTED})
                       && (@{$self->{SELECTED}} > 0));

    ## default any unspecified sections to the current one
    my $i;
    for ($i = 0; $i < $PDL::Pod::Parser::MAX_HEAD_LEVEL; ++$i) {
        $heads[$i] = $self->{HEADINGS}->[$i]  unless (defined $heads[$i]);
    }

    ## Look for a match against the specified section expressions
    my ($sec_spec, $regexp, $negated, $match);
    for $sec_spec (@{$self->{SELECTED}}) {
        $match = 1;
        for ($i = 0; $i < $PDL::Pod::Parser::MAX_HEAD_LEVEL; ++$i) {
            $regexp  = $sec_spec->[$i];
            $negated = ($regexp =~ /^\!/o);
            $regexp  = $'  if ($negated);
            $match  &= ($negated ? ($heads[$i] !~ /${regexp}/)
                                 : ($heads[$i] =~ /${regexp}/));
            last unless ($match);
        }
        return  1  if ($match);
    }
    return  0;  ## no match
}

=head2 begin_input()

This method is invoked by B<parse_from_filehandle()> immediately I<before>
processing input from a filehandle. The base class implementation does
nothing but subclasses may override it to perform any per-file
intializations.

=cut

sub begin_input {
    my $self = shift;
    #----------------------------------------------------
    # Subclasses may wish to make use of some of the
    # commented-out code below for initializing pragmas
    #----------------------------------------------------
    # $self->{PRAGMAS} = {
    #     FILL     => 'on',
    #     STYLE    => 'plain',
    #     INDENT   => 0,
    # };
    # ## Initialize all PREVIOUS_XXX pragma values
    # my ($name, $value);
    # for (($name, $value) = each %{$self->{PRAGMAS}}) {
    #     $self->{PRAGMAS}->{"PREVIOUS_${name}"} = $value;
    # }
    #----------------------------------------------------
    return;
}

=head2 end_input()

This method is invoked by B<parse_from_filehandle()> immediately I<after>
processing input from a filehandle. The base class implementation does
nothing but subclasses may override it to perform any per-file
cleanup actions.

=cut

sub end_input {
    my $self = shift;
    return;
}

=head2 preprocess_line($text)

This methods should be overridden by subclasses that wish to perform any
kind of preprocessing for each I<line> of input (I<before> it has been
determined whether or not it is part of a pod paragraph). The parameter
C<$text> is the input line and the value returned should correspond to
the new text to use in its place. If the empty string or an undefined
value is returned then no further process will be performed for this
line. If desired, this method can call the B<parse_paragraph()> method
directly with any preprocessed text and return an empty string (to
indicate that no further processing is needed).

Please note that the B<preprocess_line()> method is invoked I<before>
the B<preprocess_paragraph()> method. After all (possibly preprocessed)
lines in a paragraph have been assembled together and it has been
determined that the paragraph is part of the pod documentation from one
of the selected sections, then B<preprocess_paragraph()> is invoked.

The base class implementation of this method returns the given text.

=cut

sub preprocess_line($text) {
    my $self = shift;
    my $text = shift;
    return  $text;
}

=head2 preprocess_paragraph($text)

This method should be overridden by subclasses that wish to perform any
kind of preprocessing for each block (paragraph) of pod documentation
that appears in the input stream.  The parameter C<$text> is the pod
paragraph from the input file and the value returned should correspond
to the new text to use in its place.  If the empty string is returned or
an undefined value is returned, then the given C<$text> is ignored (not
processed).

This method is invoked by B<parse_paragraph()>. After it returns,
B<parse_paragraph()> examines the current cutting state (which is
stored in C<$self-E<gt>{CUTTING}>). If it evaluates to false then input text
(including the given C<$text>) is cut (not processed) until the next pod
directive is encountered.

Please note that the B<preprocess_line()> method is invoked I<before>
the B<preprocess_paragraph()> method. After all (possibly preprocessed)
lines in a paragraph have been assembled together and it has been
determined that the paragraph is part of the pod documentation from one
of the selected sections, then B<preprocess_paragraph()> is invoked.

The base class implementation of this method returns the given text.

=cut

sub preprocess_paragraph {
    my $self = shift;
    my $text = shift;
    return  $text;
}

=head2 parse_pragmas($cmd, $text, $sep)

This method is called when an C<=pod> directive is encountered. When
such a pod directive is seen in the input, this method is called and is
passed the command name C<$cmd> (which should be "pod") and the
remainder of the text paragraph C<$text> which appeared immediately
after the command name. If desired, the text which separated the C<=pod>
directive from its corresponding text may be found in C<$sep>.  Each
word in C<$text> is examined to see if it is a pragma specification.
Pragma specifications are of the form C<pragma_name=pragma_value>.

Unless the given object is an instance of the B<PDL::Pod::Parser> class, the
base class implementation of this method will invoke the B<pragma()> method for
each pragma specification in C<$text>.  I<If and only if> the given
object I<is> an instance of the B<PDL::Pod::Parser> class, the base class
version of this method will simply reproduce the C<=pod> command exactly
as it appeared in the input.

Derived classes should I<not> usually need to reimplement this method.

=cut

sub parse_pragmas {
    my $self = shift;
    my $cmd  = shift;
    my $text = shift;
    my $sep  = shift;
    $cmd  = ''  unless (defined $cmd);
    $text = ''  unless (defined $text);
    $sep  = ' ' unless (defined $sep);
    local($_);
    if (ref($self) eq 'PDL::Pod::Parser') {
        ## If and only if this is an instance of the base class, then
        ## dump the '=pod' paragraph exactly as it appeared in the input.
        $self->print("=${cmd}${sep}${text}");
    }
    else {
        ## otherwise invoke the pragma command for each word in $text
        my @pragmas = split(" ", $text);
        for (@pragmas) {
            $self->pragma(lc $`, $')  if (/=/o);
        }
    }
}

=head2 pragma($pragma_name, $pragma_value)

This method is invoked for each pragma encountered inside an C<=pod>
paragraph (see the description of the B<parse_pragmas()> method). The
pragma name is passed in C<$pragma_name> (which should always be
lowercase) and the corresponding value is C<$pragma_value>.

The base class implementation of this method does nothing.  Derived
class implementations of this method should be able to recognize at
least the following pragmas and take any necessary actions when they are
encountered:

=over 4

=item B<fill=value>

The argument I<value> should be one of C<on>, C<off>, or C<previous>.
Specifies that "filling-mode" should set to 1, 0, or its previous value
(respectively). If I<value> is omitted then the default is C<on>.
Derived classes may use this to decide whether or not to perform any
filling (wrapping) of subsequent text.

=item B<style=value>

The argument I<value> should be one of C<bold>, C<italic>, C<code>,
C<plain>, or C<previous>. Specifies that the current default paragraph
font should be set to C<bold>, C<italic>, C<code>, the empty string C<>,
or its previous value (respectively).  If I<value> is omitted then the
default is C<plain>.  Derived classes may use this to determine the
default font style to use for subsequent text.

=item B<indent=value>

The argument I<value> should be an integer value (with an optional
sign).  Specifies that the current indentation level should be reset to
the given value. If a plus (minus) sign precedes the number then the
indentation level should be incremented (decremented) by the given
number. If only a plus or minus sign is given (without a number) then
the current indentation level is incremented or decremented by some
default amount (to be determined by subclasses).

=back

The value returned will be 1 if the pragma name was recognized and 0 if
it wasnt (in which case the pragma was ignored).

Derived classes should override this method if they wish to implement
any pragmas. The base class implementation of this method does nothing
but it does contain some commented-out code which subclasses may want
to make use of when implementing pragmas.

=cut

sub pragma {
    my $self  = shift;
    ## convert remaining args to lowercase
    my $name  = lc shift;
    my $value = lc shift;
    my $rc = 1;
    local($_);
    #----------------------------------------------------
    # Subclasses may wish to make use of some of the
    # commented-out code below for processing pragmas
    #----------------------------------------------------
    # my ($abbrev, %abbrev_table);
    # if ($name eq 'fill') {
    #     %abbrev_table = ('on' => 'on',
    #                      'of' => 'off',
    #                      'p'  => 'previous');
    #     $value = 'on' unless ((defined $value) && ($value ne ''));
    #     return  $rc  unless ($value =~ /^(on|of|p)/io);
    #     $abbrev = $1;
    #     $value = $abbrev_table{$abbrev};
    #     if ($value eq 'previous') {
    #         $self->{PRAGMAS}->{FILL} = $self->{PRAGMAS}->{PREVIOUS_FILL};
    #     }
    #     else {
    #         $self->{PRAGMAS}->{PREVIOUS_FILL} = $self->{PRAGMAS}->{FILL};
    #         $self->{PRAGMAS}->{FILL} = $value;
    #     }
    # }
    # elsif ($name eq 'style') {
    #     %abbrev_table = ('b'  => 'bold',
    #                      'i'  => 'italic',
    #                      'c'  => 'code',
    #                      'pl' => 'plain',
    #                      'pr' => 'previous');
    #     $value = 'plain' unless ((defined $value) && ($value ne ''));
    #     return  $rc  unless ($value =~ /^(b|i|c|pl|pr)/io);
    #     $abbrev = $1;
    #     $value = $abbrev_table{$abbrev};
    #     if ($value eq 'previous') {
    #         $self->{PRAGMAS}->{STYLE} = $self->{PRAGMAS}->{PREVIOUS_STYLE};
    #     }
    #     else {
    #         $self->{PRAGMAS}->{PREVIOUS_STYLE} = $self->{PRAGMAS}->{STYLE};
    #         $self->{PRAGMAS}->{STYLE} = $value;
    #     }
    # }
    # elsif ($name eq 'indent') {
    #     return $rc unless ((defined $value) && ($value =~ /^([-+]?)(\d*)$/o));
    #     my ($sign, $number) = ($1, $2);
    #     $value .= "4"  unless ((defined $number) && ($number ne ''));
    #     $self->{PRAGMAS}->{PREVIOUS_INDENT} = $self->{PRAGMAS}->{INDENT};
    #     if ($sign) {
    #         $self->{PRAGMAS}->{INDENT} += (0 + $value);
    #     }
    #     else {
    #         $self->{PRAGMAS}->{INDENT} = $value;
    #     }
    # }
    # else {
    #     $rc = 0;
    # }
    #----------------------------------------------------
    return $rc;
}

=head2 command($cmd, $text, $sep)

This method should be overridden by subclasses to take the appropriate
action when a pod command paragraph (denoted by a line beginning with
"=") is encountered.  When such a pod directive is seen in the input,
this method is called and is passed the command name C<$cmd> and the
remainder of the text paragraph C<$text> which appears immediately after
the command name. If desired, the text which separated the command from
its corresponding text may be found in C<$sep>.  Note that this method
is I<not> called for C<=pod> paragraphs.

The base class implementation of this method simply prints the raw pod
command to the output filehandle and then invokes the B<textblock()>
method, passing it the C<$text> parameter.

=cut

sub command {
    my $self = shift;
    my $cmd  = shift;
    my $text = shift;
    my $sep  = shift;
    $cmd  = ''  unless (defined $cmd);
    $text = ''  unless (defined $text);
    $sep  = ' ' unless (defined $sep);
    $self->print("=${cmd}${sep}");
    $self->textblock($text);
}

=head2 verbatim($text)

This method may be overridden by subclasses to take the appropriate
action when a block of verbatim text is encountered. It is passed the
text block C<$text> as a parameter.

The base class implementation of this method simply prints the textblock
(unmodified) to the output filehandle.

=cut

sub verbatim {
    my $self = shift;
    my $text = shift;
    $self->print($text);
}

=head2 textblock($text)

This method may be overridden by subclasses to take the appropriate
action when a normal block of pod text is encountered (although the base
class method will usually do what you want). It is passed the text block
C<$text> as a parameter.

In order to process interior sequences, subclasses implementations of
this method will probably want invoke the B<interpolate()> method,
passing it the text block C<$text> as a parameter and then perform any
desired processing upon the returned result.

The base class implementation of this method simply prints the text block
as it occurred in the input stream).

=cut

sub textblock {
    my $self  = shift;
    my $text  = shift;
    local($_) = $self->interpolate($text);
    $self->print($_);
}

=head2 interior_sequence($seq_cmd, $seq_arg)

This method should be overridden by subclasses to take the appropriate
action when an interior sequence is encountered. An interior sequence is
an embedded command within a block of text which appears as a command
name (usually a single uppercase character) followed immediately by
a string of text which is enclosed in angle brackets. This method is
passed the sequence command C<$seq_cmd> and the corresponding text
$seq_arg and is invoked by the B<interpolate()> method for each
interior sequence that occurs in the string that it is passed.
It should return the desired text string to be used in place of
the interior sequence.

Subclass implementationss of this method may wish to examine the
the array referenced by C<$self-E<gt>{SEQUENCES}> which is a
stack of all the interior sequences that are currently being
processed (they may be nested). The current interior sequence
(the one given by C<$seq_cmdE<lt>$seq_argE<gt>>) should always
be at the top of this stack.

The base class implementation of the B<interior_sequence()> method simply
returns the raw text of the of the interior sequence (as it occurred in
the input) to the output filehandle.

=cut

sub interior_sequence {
    my $self = shift;
    my $seq_cmd = shift;
    my $seq_arg = shift;
    return  "${seq_cmd}<${seq_arg}>";
}

=head2 interpolate($text, $end_re)

This method will translate all text (including any embedded interior
sequences) in the given text string C<$text> and return the
interpolated result.  If a second argument is given, then it is taken to
be a regular expression that indicates when to quit interpolating the
string.  Upon return, the C<$text> parameter will have been modified to
contain only the un-processed portion of the given string (which will
I<not> contain any text matched by C<$end_re>).

This method should probably I<not> be overridden by subclasses.
It should be noted that this method invokes itself recursively
to handle any nested interior sequences.

=cut

sub interpolate {
    my $self = shift;
    my ($text, $end_re) = @_;
    $text   = ''   unless (defined $text);
    $end_re = '$'  unless ((defined $end_re) && ($end_re ne ''));
    local($_)  = $text;
    my $result = '';
    my ($seq_cmd, $seq_arg, $end) = ('', '', undef);
    while (($_ ne '') && /([A-Z])<|($end_re)/) {
        $result .= $`;  ## Append text before the match to the result
        $_ = $';        ## Only text after the match remains to be processed
        ## See if we matched an interior sequence or an end-expression
        ($seq_cmd, $end) = ($1, $2);
        last if (defined $end);  ## Saw the end - quit loop here
        ## At this point we have found an interior sequence,
        ## we need to obtain its argument
        push(@{$self->{SEQUENCES}}, $seq_cmd);
        $seq_arg = $self->interpolate($_, '>');
        ## Now process the interior sequence
        $result .= $self->interior_sequence($seq_cmd, $seq_arg);
        pop(@{$self->{SEQUENCES}});
    }
    ## Handle whatever is left if we didnt match the ending regexp
    unless ((defined $end) && ($end_re ne '$')) {
        $result .= $_;
        $_ = '';
    }
    ## Modify the input parameter to consume the text that was
    ## processed so far.
    $_[0] = $_;
    ## Return the processed-text
    return  $result;
}

=head2 parse_paragraph($text)

This method takes the text of a pod paragraph to be processed and
invokes the appropriate method (one of B<command()>, B<verbatim()>,
or B<textblock()>).

This method does I<not> usually need to be overridden by subclasses.

=cut

sub parse_paragraph {
    my $self = shift;
    local $_ = shift;
    my ($para);
    my ($cmd, $arg, $sep);
    ## Keep track of current paragraph number
    ++$self->{PARAGRAPH}  if ((defined $_)  &&  ($_ ne ''));
    ## Ignore up until next pod directive if we are cutting
    if ($self->{CUTTING}) {
        return  1  unless /^=/o;
        $self->{CUTTING} = 0;
    }
    ## Keep track of current sections and their titles
    my ($level, $title);
    if (/^=((?:sub)*)(?:head(?:ing)?|sec(?:tion)?)(\d*)\s+(.*)\s*$/o) {
        ## This is a section heading command
        ($level, $title) = ($2, $3);
        $level = 1 + (length($1) / 3)  if (($level eq '') || ($1 ne ''));
        ## Reset the current section heading at this level
        $self->{HEADINGS}->[$level - 1] = $title;
        ## Reset subsection headings of this one to empty
        my $i;
        for ($i = $level; $i < $PDL::Pod::Parser::MAX_HEAD_LEVEL; ++$i) {
            $self->{HEADINGS}->[$i] = '';
        }
    }
    ## Ignore this block if it isnt in one of the selected sections
    $self->{CUTTING} = 1   unless ($self->want_section());
    return  1  if ($self->{CUTTING});

    ## Perform any desired preprocessing
    $para = $self->preprocess_paragraph($_);
    next unless ((defined $para) && ($para ne ""));
    $_ = $para;
    return  1  if ($self->{CUTTING});

    ## Look for one of the three types of paragraphs
    if (s/^=//o) {
        ## Looks like a command paragraph
        ($cmd, $sep, $_) = /\s+/o ? ($`, $&, $') : ($_, '', '');
        if ($cmd eq 'cut') {
            $self->{CUTTING} = 1;
        }
        elsif ($cmd eq 'pod') {
            ## This is a pod pragma paragraph
            $self->parse_pragmas($cmd, $_, $sep);
        }
        else {
            ## Some other command
            $self->command($cmd, $_, $sep);
        }
    }
    elsif (/^\s+/o) {
        ## Indented text - must be a verbatim paragraph
        $self->verbatim($_);
    }
    else {
        ## Looks like an ordinary block of text
        $self->textblock($_);
    }
    return  1;
}

=head2 parse_from_filehandle($infilehandle, $outfilehandle)

This method takes a glob to a filehandle (which is assumed to already be
opened for reading) and reads the entire input stream looking for blocks
(paragraphs) of pod documentation to be processed. For each block of pod
documentation encountered it will call the B<parse_paragraph()> method.

If a second argument is given then it should be a filehandle glob where
output should be sent (otherwise the default output filehandle is
C<STDOUT>). If no first argument is given the default input filehandle
C<STDIN> is used.

The input filehandle that is currently in use is stored in the member
variable whose key is "INPUT" (e.g. C<$self-E<gt>{INPUT}>).

The output filehandle that is currently in use is stored in the member
variable whose key is "OUTPUT" (e.g. C<$self-E<gt>{OUTPUT}>).

Input is read line-by-line and assembled into paragraphs (which are
separated by lines containing nothing but whitespace). The current line
number is stored in the member variable whose key is "LINE" (e.g.
C<$self-E<gt>{LINE}>) and the current paragraph number is stored in the
member variable whose key is "PARAGRAPH" (e.g.  C<$self-E<gt>{PARAGRAPH}>).

This method does I<not> usually need to be overridden by subclasses.

=cut

sub parse_from_filehandle {
    my $self = shift;
    my $infilehandle = shift;
    my $outfilehandle = shift;
    local($_);

    $infilehandle  = \*STDIN   unless (defined $infilehandle);
    $outfilehandle = \*STDOUT  unless (defined $outfilehandle);

    ## Initialize stuff for this input stream
    $self->{INPUT}     = $infilehandle;
    $self->{OUTPUT}    = $outfilehandle;
    $self->{CUTTING}   = 1;   ## Keep track of when we are cutting
    $self->{SEQUENCES} = [];  ## Keep track of nested interior sequences
    ## Initialize section heading titles
    {
        $self->{HEADINGS}   = [];
        my $i;
        for ($i = 0; $i < $PDL::Pod::Parser::MAX_HEAD_LEVEL; ++$i) {
            $self->{HEADINGS}->[$i] = '';
        }
    }

    $self->begin_input();

    my $paragraph = '';
    while (<$infilehandle>) {
        ++$self->{LINE};  ## Keep track of current line number
        $_ = $self->preprocess_line($_);
        next  unless ((defined $_)  &&  ($_ ne ''));
        if (! /^\s*$/o) {
            ## Append this line to the current paragraph
            $paragraph .= $_;
            next;
        }
        ## This line is blank line and ends the current paragraph
        next  if ($paragraph eq '');
        $paragraph .= "\n";
        $self->parse_paragraph($paragraph);
        $paragraph = '';
    }
    ## Dont forget about the last paragraph in the file
    $self->parse_paragraph($paragraph)  unless ($paragraph eq '');

    $self->end_input();
}

=head2 parse_from_file($filename, $outfile)

This method takes a filename and does the following:

=over 4

=item *

opens the input and output files for reading
(creating the appropriate filehandles)

=item *

invokes the B<parse_from_filehandle()> method passing it the
corresponding input and output filehandles.

=item *

closes the input and output files.

=back

If the special input filename "-" or "<&STDIN" is given then the STDIN
filehandle is used for input (and no open or close is performed).  If no
input filename is specified then "-" is implied.  If a reference is
passed instead of a filename then it is assumed to be a glob-style
reference to a filehandle.


If a second argument is given then it should be the name of the desired
output file.  If the special output filename "-" or ">&STDOUT" is given
then the STDOUT filehandle is used for output (and no open or close is
performed). If the special output filename ">&STDERR" is given then the
STDERR filehandle is used for output (and no open or close is
performed).  If no output filename is specified then "-" is implied.
If a reference is passed instead of a filename then it is assumed to
be a glob-style reference to a filehandle.

The name of the input file that is currently being read is stored in the
member variable whose key is "INFILE" (e.g. C<$self-E<gt>{INFILE}>).

The name of the output file that is currently being written is stored in
the member variable whose key is "OUTFILE" (e.g.  C<$self-E<gt>{OUTFILE}>).

This method does I<not> usually need to be overridden by subclasses.

=cut

sub parse_from_file {
    my $self = shift;
    my $infile = shift;
    my $outfile = shift;
    my ($in_fh,  $in_fh_name)  = (undef, undef);
    my ($out_fh, $out_fh_name) = (undef, undef);

    $infile  = '-'  unless ((defined $infile)  && ($infile ne ''));
    $outfile = '-'  unless ((defined $outfile) && ($outfile ne ''));

    if (($infile  eq '-') || ($infile =~ /^<&(STDIN|0)$/o)) {
        $in_fh = \*STDIN;
        $self->{INFILE} = "<standard input>";
    }
    elsif (ref $infile) {
        $in_fh = $infile;
        $self->{INFILE} = ${$infile};;
    }
    else {
        $self->{INFILE} = $in_fh_name = $infile;
        $in_fh_name =~ s/\W/_/g;
        no strict "refs";
        open($in_fh_name, "<$infile") ||
             croak "Can't open $infile for reading: $!\n";
        $in_fh = \*$in_fh_name;
    }

    if (($outfile  eq '-') || ($outfile =~ /^>&?(STDOUT|1)$/o)) {
        $out_fh  = \*STDOUT;
        $self->{OUTFILE} = "<standard output>";
    }
    elsif ($outfile =~ /^>&(STDERR|2)$/o) {
        $out_fh  = \*STDERR;
        $self->{OUTFILE} = "<standard error>";
    }
    elsif (ref $outfile) {
        $out_fh = $outfile;
        $self->{OUTFILE} = ${$outfile};;
    }
    else {
        $self->{OUTFILE} = $out_fh_name = $outfile;
        $out_fh_name =~ s/\W/_/g;
        no strict "refs";
        open($out_fh_name, ">$outfile") ||
             croak "Can't open $outfile for writing: $!\n";
        $out_fh = \*$out_fh_name;
    }

    $self->parse_from_filehandle($in_fh, $out_fh);

    if (defined $in_fh_name) {
        close($in_fh) || croak "Can't close $infile after reading: $!\n";
    }
    if (defined $out_fh_name) {
        close($out_fh) || croak "Can't close $outfile after writing: $!\n";
    }
}

sub print {
  my $self = shift;
  if (ref($self->{OUTPUT}) eq 'GLOB') {
    my $out_fh = $self->{OUTPUT};
    print $out_fh @_;
  } else {
    $self->{OUTPUT}->print(@_);
  }
}

##---------------------------------------------------------------------------

=head1  INSTANCE DATA

B<PDL::Pod::Parser> uses the following data members for each of its
instances (where C<$self> is a reference to such an instance):

=head2 $self->{INPUT}

The current input filehandle.

=head2 $self->{OUTPUT}

The current output filehandle.

=head2 $self->{INFILE}

The name of the current input file.

=head2 $self->{OUTFILE}

The name of the current output file.

=head2 $self->{LINE}

The current line number from the input stream.

=head2 $self->{PARAGRAPH}

The current paragraph number from the input stream (which includes input
paragraphs that are I<not> part of the pod documentation).

=head2 $self->{HEADINGS}

A reference to an array of the current section heading titles for each
heading level (note that the first heading level title is at index 0).

=head2 $self->{SELECTED}

A reference to an array of references to arrays. Each subarray is a list
of anchored regular expressions (preceded by a "!" if the regexp is to be
negated). The index of the expression in the subarray should correspond
to the index of the heading title in B<$self-E<gt>{HEADINGS}> that it is
to be matched against.

=head2 $self->{CUTTING}

A boolean-valued scalar which evaluates to true if text from the
input file is currently being "cut".

=head2 $self->{SEQUENCES}

An array reference to the stack of interior sequence commands that are
currently in the middle of being processed.

=head1 NOTES

To create a pod translator to translate pod documentation to some other
format, you usually only need to create a subclass of B<PDL::Pod::Parser>
which overrides the base class implementation for the following methods:

=over 4

=item *

B<pragma()>

=item *

B<command()>

=item *

B<verbatim()>

=item *

B<textblock()>

=item *

B<interior_sequence()>

=back

You may also want to implement the B<begin_input()> and B<end_input()>
methods for your subclass (to perform any needed per-file intialization
or cleanup).

If you need to perform any preprocesssing of input before it is parsed
you may want to implement one or both of the B<preprocess_line()> and/or
B<preprocess_paragraph()> methods.

Also, don't forget to make sure your subclass constructor invokes the
base class' B<initialize()> method.

Sometimes it may be necessary to make more than one pass over the input
files. This isn't a problem as long as none of the input files correspond
to C<STDIN>. You can override either the B<parse_from_filehandle()>
method or the B<parse_from_file()> method to make the first pass yourself
to collect all the information you need and then invoke the base class
method to do the rest of the standard processing.

Feel free to add any member data fields you need to keep track of things
like current font, indentation, horizontal or vertical position, or
whatever else you like.

For the most part, the B<PDL::Pod::Parser> base class should be able to
do most of the input parsing for you and leave you free to worry about
how to intepret the commands and translate the result.

=head1 AUTHOR

Brad Appleton E<lt>Brad_Appleton-GBDA001@email.mot.comE<gt>

Based on code for B<Pod::Text> written by
Tom Christiansen E<lt>tchrist@mox.perl.comE<gt>

=cut

1;
