
package PDL::Options;

=head1 NAME

PDL::Options - simplifies option passing by hash in PerlDL

=head1 SYNOPSIS

  use PDL::Options;

  %hash = parse( \%defaults, \%user_options);

  use PDL::Options ();

  $opt = new PDL::Options;
  $opt = new PDL::Options ( \%defaults );

  $opt->defaults ( \%defaults );
  $opt->synonyms ( { 'COLOR' => 'COLOUR' } );

  $hashref = $opt->defaults;

  $opt->options ( \%user_options );

  $hashref = $opt->options;

  $opt->incremental(1);
  $opt->full_options(0);

=head1 DESCRIPTION

Object to simplify option passing for PerlDL subroutines.
Allows you to merge a user defined options with defaults.
A simplified (non-OO) interface is provided.

=cut

use strict;
use Carp;

use vars qw/$VERSION %EXPORT_TAGS %DEF_SYNS @ISA/;

require Exporter;

# difference to 0.91 is that added CENTRE/CENTER as default
# synonymns (patch by Diab Jerius [ #469110 ])
$VERSION = '0.92';

@ISA = qw(Exporter);

%EXPORT_TAGS = (
		'Func' => [qw/
			   parse iparse ifhref
			   /]
	       );

Exporter::export_tags('Func');

# List of default synonyms
%DEF_SYNS = (
	     COLOR  => 'COLOUR',
	     COLOUR => 'COLOR',
	     CENTER => 'CENTRE',
	     CENTRE => 'CENTER',
	    );

my $default = {
	       WarnOnMissing => 1,
	       FullOptions => 1,
	       DEBUG => 0,
	      };

=head1 Utility functions

=head2 ifhref

  parse({Ext => 'TIF', ifhref($opt)});

just return the argument if it is a hashref otherwise return
an empty hashref. Useful in conjunction with parse to return
just the default values if argument is not a hash ref

=head1 NON-OO INTERFACE

A simplified non-object oriented interface is provided.
These routines are exported into the callers namespace by default.

=over 4

=item parse( \%defaults, \%user_options)

This will parse user options by using the defaults.  The following
settings are used for parsing: The options are case-sensitive, a
default synonym table is consulted (see L</Default Synonyms>),
minimum-matching is turned on, and translation of values is not performed.

A hash (not hash reference) containing the processed options is returned.

  %options = parse( { LINE => 1, COLOUR => 'red'}, { COLOR => 'blue'});

=item iparse( \%defaults, \%user_options)

Same as C<parse> but matching is case insensitive

=cut

sub ifhref {
  my ($href) = @_;
  return defined $href && ref $href eq 'HASH' ? $href : {};
}

sub parse { return _parse(1,@_) }
sub iparse { return _parse(0,@_) }

sub _parse {

   croak 'Usage: parse( \%defaults, \%user )' if scalar(@_) != 3;

   my $casechk = shift;
   my $defaults = shift;
   croak ("First argument is not a hash reference")
      unless ref($defaults) eq "HASH";

   my $user = shift;
   croak ("Second argument is not a hash reference")
      unless ref($user) eq "HASH";

   # Create new object
   my $opt = new PDL::Options ( $defaults );

   # Set up default behaviour
   $opt->minmatch(1);
   $opt->casesens($casechk);
   $opt->synonyms( \%DEF_SYNS );

   # Process the options
   my $optref = $opt->options( $user );

   return %$optref;
}


=back

=head2 Default Synonyms

The following default synonyms are available in the non-OO interface:

  COLOR  => COLOUR
  COLOUR => COLOR
  CENTER => CENTRE
  CENTRE => CENTER

=head1 METHODS

The following methods are available to PDL::Options objects.

=over 4

=item new()

Constructor. Creates the object. With an optional argument can also
set the default options.

=cut

sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $opt = {};

  # Set up object structure
  $opt->{DEFAULTS} = {};   # Default options
  $opt->{CURRENT}  = {};   # Current options
  $opt->{CurrKeys} = [];   # list of selected keys if full_options(0)
  $opt->{SYNONYMS} = {};   # List of synonyms
  $opt->{INC}      = 0;    # Flag to decide whether we are incremental on cur
  $opt->{CaseSens} = 0;    # Are options case sensitive
  $opt->{MinMatch} = 1;    # Minimum matching on keys
  $opt->{Translation} = {};# Translation from eg 'RED' to 1
  $opt->{AutoTranslate}= 1;# Automatically translate options when processing
  $opt->{MinMatchTrans} = 0; # Min matching during translation
  $opt->{CaseSensTrans} = 0; # Case sensitive during translation
  # Return full options list
  $opt->{FullOptions} = $default->{FullOptions};
  # Whether to warn for options that are invalid or not
  $opt->{WarnOnMissing}= $default->{WarnOnMissing};
  $opt->{DEBUG}    = $default->{DEBUG};    # Turn on debug messages

  # Bless into class
  bless ( $opt, $class);

  # If we were passed arguments, pass to defaults method
  if (@_) { $opt->defaults( @_ ); }

  return $opt;
}

=item extend (\%options)

This will copy the existing options object and extend it with the
requested extra options.

=cut

sub extend {

  my ($self, $opt)=@_;

  my $class = ref($self);
  my $h = {%{$self}};
  croak ("Argument is not reference to hash!\n") unless ref($opt) eq 'HASH';
  #
  # The next step is to perform a deep copy of the hash
  # references since we might want to change these without
  # changing the originals.
  #
  $h->{SYNONYMS}={%{$self->{SYNONYMS}}};
  $h->{Translation}={%{$self->{Translation}}};
  $h->{CurrKeys}=[@{$self->{CurrKeys}}];
  #
  # Create the extended option list.
  #
  my %all_options = (%{$opt}, %{$self->{DEFAULTS}});

  # Bless it
  bless ($h, $class);

  # And parse the default options
  $h->defaults(\%all_options);

  return $h;

}

# =item change_defaults (\%options)

# This will merge the options given with the defaults hash and hence change
# the default hash. This is not normally a good idea, but in certain dynamic
# situations you might want to adjust a default parameter for future calls
# to the routine.

# =cut

# sub change_defaults {

#   my $self=shift;

#   my $arg = shift;
#   croak("Argument is not a hash reference!\n") unless ref($arg) eq 'HASH';

#   my $defs = $self->defaults($arg);

#   $self->defaults($)


# }


=item defaults( \%defaults )

Method to set or return the current defaults. The argument should be
a reference to a hash. The hash reference is returned if no arguments
are supplied.

The current values are reset whenever the defaults are changed.

=cut

sub defaults {
  my $self = shift;

  if (@_) {
    my $arg = shift;
    croak("Argument is not a hash reference") unless ref($arg) eq "HASH";
    $self->{DEFAULTS} = $arg;

    # Reset the current state (making sure that I disconnect the
    # hashes
    my %hash = %$arg;
    $self->curr_full(\%hash);

  }

  # Decouple the hash to protect it from being modified outside the
  # object
  my %hash = %{$self->{DEFAULTS}};
  return \%hash;

}

=item add_synonym (\%synonyms)

Method to add another synonym to an option set
The argument should be a reference to a hash.

=cut

sub add_synonym {
  my $self=shift;
  return unless @_;
  my $arg = shift;
  croak("Synonym argument is not a hash reference") unless ref($arg) eq "HASH";

  foreach (keys %$arg) {
    $self->{SYNONYMS}{$_}=$arg->{$_};
  }
  my %hash = %{$self->{SYNONYMS}};
  return \%hash;

}

=item add_translation (\%translation)

Method to add another translation rule to an option set.
The argument should be a reference to a hash.

=cut


sub add_translation {
  my $self = shift;
  return unless @_;
  my $arg = shift;
  croak("Translation argument is not a hash reference") unless ref($arg) eq 'HASH';

  foreach (keys %$arg) {
    $self->{Translation}{$_}=$arg->{$_};
  }
  my %hash = %{$self->{Translation}};

  return \%hash;

}

=item synonyms( \%synonyms )

Method to set or return the current synonyms. The argument should be
a reference to a hash. The hash reference is returned if no arguments
are supplied.

This allows you to provide alternate keywords (such as allowing
'COLOR' as an option when your defaults uses 'COLOUR').

=cut

sub synonyms {
  my $self = shift;

  if (@_) {
    my $arg = shift;
    croak("Argument is not a hash reference") unless ref($arg) eq "HASH";
    $self->{SYNONYMS} = $arg;
  }

  # Decouple the hash to protect it from being modified outside the
  # object
  my %hash = %{$self->{SYNONYMS}};
  return \%hash;

}


=item current

Returns the current state of the options. This is returned
as a hash reference (although it is not a reference to the
actual hash stored in the object). If full_options() is true
the full options hash is returned, if full_options() is false
only the modified options are returned (as set by the last call
to options()).

=cut

sub current {
  my $self = shift;

  if ($self->full_options) {
    return $self->curr_full;
  } else {
    my @keys = $self->curr_keys;
    my %hash = ();
    my $curr = $self->curr_full;

    foreach my $key (@keys) {
      $hash{$key} = $$curr{$key} if exists $$curr{$key};
    }
    return \%hash;
  }
}

=item clear_current

This routine clears the 'state' of the C<PDL::Options> object so that
the next call to current will return an empty list

=cut

sub clear_current {
  my $self = shift;
  @{$self->{CurrKeys}}=();
}


# Method to set the 'mini' state of the object
# This is just a list of the keys in %defaults that were selected
# by the user. current() returns the hash with these keys if
# called with full_options(0).
# Not publicising this

sub curr_keys {
  my $self = shift;
  if (@_) { @{$self->{CurrKeys}} = @_; }
  return @{$self->{CurrKeys}};
}

# Method to set the full state of the object
# Not publicising this

sub curr_full {
  my $self = shift;

  if (@_) {
    my $arg = shift;
    croak("Argument is not a hash reference") unless ref($arg) eq "HASH";
    $self->{CURRENT} = $arg;
  }

  # Decouple the hash
  my %hash = %{$self->{CURRENT}};
  return \%hash;

}


=item translation

Provide translation of options to more specific values that are
recognised by the program. This allows, for example, the automatic
translation of the string 'red' to '#ff0000'.

This method can be used to setup the dictionary and is hash reference
with the following structure:

    OPTIONA => {
	        'string1' => decode1,
                'string2' => decode2
		},
    OPTIONB => {
                's4' => decodeb1,
	       }
    etc....

Where OPTION? corresponds to the top level option name as stored in
the defaults array (eg LINECOLOR) and the anonymous hashes provide
the translation from string1 ('red') to decode1 ('#ff0000').

An options string will be translated automatically during the main options()
processing if autotrans() is set to true. Else translation can be
initiated by the user using the translate() method.

=cut

sub translation {
  my $self = shift;

  if (@_) {
    my $arg = shift;
    croak("Argument is not a hash reference") unless ref($arg) eq "HASH";
    $self->{Translation} = $arg;
  }

  # Decouple the hash to protect it from being modified outside the
  # object
  my %hash = %{$self->{Translation}};
  return \%hash;

}


=item incremental

Specifies whether the user defined options will be treated as additions
to the current state of the object (1) or modifications to the default
values only (0).

Can be used to set or return this value.
Default is false.

=cut

sub incremental {
  my $self = shift;
  if (@_) { $self->{INC} = shift; }
  return $self->{INC};
}

=item full_options

Governs whether a complete set of options is returned (ie defaults
+ expanded user options), true, or if just the expanded user
options are returned, false (ie the values specified by the user).

This can be useful when you are only interested in the changes to
the options rather than knowing the full state. (For example, if
defaults contains keys for COLOUR and LINESTYLE and the user supplied
a key of COL, you may simply be interested in the modification to
COLOUR rather than the state of LINESTYLE and COLOUR.)

Default is true.

=cut

sub full_options {
  my $self = shift;
  if (@_) { $self->{FullOptions} = shift; }
  return $self->{FullOptions};

}

=item casesens

Specifies whether the user defined options will be processed independent
of case (0) or not (1). Default is to be case insensitive.

Can be used to set or return this value.

=cut

sub casesens {
  my $self = shift;
  if (@_) { $self->{CaseSens} = shift; }
  return $self->{CaseSens};
}

=item minmatch

Specifies whether the user defined options will be minimum matched
with the defaults (1) or whether the user defined options should match
the default keys exactly. Defaults is true (1).

If a particular key matches exactly (within the constraints imposed
bby case sensitivity) this key will always be taken as correct even
if others are similar. For example COL would match COL and COLOUR but
this implementation will always return COL in this case (note that
for CO it will return both COL and COLOUR and pick one at random.

Can be used to set or return this value.

=cut

sub minmatch {
  my $self = shift;
  if (@_) { $self->{MinMatch} = shift; }
  return $self->{MinMatch};
}


=item autotrans

Specifies whether the user defined options will be processed via
the translate() method immediately following the main options
parsing. Default is to autotranslate (1).

Can be used to set or return this value.

=cut

sub autotrans {
  my $self = shift;
  if (@_) { $self->{AutoTranslate} = shift; }
  return $self->{AutoTranslate};
}


=item casesenstrans

Specifies whether the keys in the options hash will be matched insensitive
of case (0) during translation() or not (1). Default is to be case insensitive.

Can be used to set or return this value.

=cut

sub casesenstrans {
  my $self = shift;
  if (@_) { $self->{CaseSensTrans} = shift; }
  return $self->{CaseSensTrans};
}

=item minmatchtrans

Specifies whether the keys in the options hash  will be minimum matched
during translation(). Default is false (0).

If a particular key matches exactly (within the constraints imposed
bby case sensitivity) this key will always be taken as correct even
if others are similar. For example COL would match COL and COLOUR but
this implementation will always return COL in this case (note that
for CO it will return both COL and COLOUR and pick one at random.

Can be used to set or return this value.

=cut

sub minmatchtrans {
  my $self = shift;
  if (@_) { $self->{MinMatchTrans} = shift; }
  return $self->{MinMatchTrans};
}


=item warnonmissing

Turn on or off the warning message printed when an options is not in
the options hash. This can be convenient when a user passes a set of
options that has to be parsed by several different option objects down
the line.

=cut

sub warnonmissing {
  my $self = shift;
  if (ref $self) {
    if (@_) { $self->{WarnOnMissing}=shift;}
    return $self->{WarnOnMissing};
  } else {
    $default->{WarnOnMissing} = shift if @_;
    return $default->{WarnOnMissing};
  }
}


=item debug

Turn on or off debug messages. Default is off (0).
Can be used to set or return this value.

=cut

sub debug {
  my $self = shift;
  if (ref $self) {
    if (@_) { $self->{DEBUG} = shift; }
    return $self->{DEBUG};
  } else {
    $default->{DEBUG} = shift if @_;
    return $default->{DEBUG};
  }
}


=item options

Takes a set of user-defined options (as a reference to a hash)
and merges them with the current state (or the defaults; depends
on the state of incremental()).

The user-supplied keys will be compared with the defaults.
Case sensitivity and minimum matching can be configured using
the mimatch() and casesens() methods.

A warning is raised if keys present in the user options are not
present in the defaults unless warnonmissing is set.

A reference to a hash containing the merged options is returned.

  $merged = $opt->options( { COL => 'red', Width => 1});

The state of the object can be retrieved after this by using the
current() method or by using the options() method with no arguments.
If full_options() is true, all options are returned (options plus
overrides), if full_options() is false then only the modified
options are returned.

Synonyms are supported if they have been configured via the synonyms()
method.

=cut

sub options {

  my $self = shift;

  # If there is an argument do something clever
  if (@_) {

    # check that the arg is a hash
    my $arg = shift;
    croak("Argument is not a hash reference") unless ref($arg) eq "HASH";

    # Turn the options into a real hash
    my %user = %$arg;

    # Now read in the base options
    my $base;
    if ($self->incremental) {
      $base = $self->curr_full;
    } else {
      $base = $self->defaults;
    }

    # Turn into a real hash for convenience
    my %base = %$base;

    # Store a list of all the expanded user keys
    my @list = ();

    # Read in synonyms
    my %syn = %{$self->synonyms};

    # Now go through the keys in the user hash and compare with
    # the defaults
    foreach my $userkey (sort keys %user) {

      # Check for matches in the default set
      my @matched = $self->compare_with_list(0, $userkey, keys %base);

      # If we had no matches, check the synonyms list
      if ($#matched == -1) {
	@matched = $self->compare_with_list(0, $userkey, keys %syn);

	# If we have matched then convert the key to the actual
	# value stored in the object
	for (my $i =0; $i <= $#matched; $i++) {
	  $matched[$i] = $syn{$matched[$i]};
	}
      }

      # At this point we have matched the userkey to a key in the
      # defaults list (or if not say so)
      if ($#matched == -1) {
	print "Warning: $userkey is not a valid option\n" if $self->{WarnOnMissing};
      } else {
	if ( $#matched > 0 ) {
	  print "Warning: Multiple matches for option $userkey\n";
	  print "Warning: Could be any of the following:\n";
	  print join("\n",@matched) . "\n";
	  print "Accepting the first match ($matched[0])\n";
	}
	# Modify the value in %base and keep track of a separate
        # array containing only the matched keys
	$base{$matched[0]} = $user{$userkey};
	push(@list, $matched[0]);
	print "Matched: $userkey for $matched[0]\n" if $self->debug;
      }
    }

    # Finished matching so set this as the current state of the
    # object
    $self->curr_keys(@list);
    $self->curr_full(\%base);

    # Now process the values via the provided translation
    # if required. Note that the current design means that
    # We have to run this after we have set the current state.
    # Otherwise the translation() method would not work directly
    # and we would have to provide a public version and a private one.
    # Note that translate updates the current state of the object
    # So we don't need to catch the return value
    $self->translate if $self->autotrans;

  }

  # Current state should now be in current.
  # Simply return it
  return $self->current;

}

=item translate

Translate the current option values (eg those set via the options()
method) using the provided translation().

This method updates the current state of the object and returns the
updated options hash as a reference.

    $ref = $opt->translate;

=cut

sub translate {
  my $self = shift;

  my %trans = %{$self->translation};
  my %opt   = %{$self->curr_full}; # Process all options

  # Now need to go through each of the keys
  # and if the corresponding key exists in the translation
  # hash we need to check that a valid translation exists
  foreach my $key ( keys %opt ) {
    if (exists $trans{$key}) {
      # Okay so a translation might exist
      # Now compare keys in the hash in the hash
      my %subhash = %{$trans{$key}};

      my @matched =
	$self->compare_with_list(1, $opt{$key}, keys %subhash);

      # At this point we have matched the userkey to a key in the
      # dictionary. If there is no translation dont say anything
      # since it may be a 'REAL' answer (ie 1 instead of 'red')

      if ($#matched > -1) {
	if ( $#matched > 0 ) {
	  print "Warning: Multiple matches for $opt{$key} in option $key\n";
	  print "Warning: Could be any of the following:\n";
	  print join("\n",@matched) . "\n";
	  print "Accepting the first match ($matched[0])\n";

	}
	# Modify the value in the options set
	print "Translation: $opt{$key} translated to $subhash{$matched[0]}\n"
	  if $self->debug;
	$opt{$key} = $subhash{$matched[0]};

      }

    }

  }

  # Update the current state
  return $self->curr_full( \%opt );

}

# Private method to compare a key with a list of keys.
# The object controls whether case-sensitivity of minimum matching
# are required
# Arguments: flag to determine whether I am matchin options or translations
#                this is needed since both methods are configurable with
#                regards to minimum matching and case sensitivity.
#                0 - use $self->minmatch and $self->casesens
#                1 - use $self->minmatchtrans and $self->casesenstrans
#            $key: Key to be compared
#            @keys: List of keys
# Returns: Array of all keys that match $key taking into account the
#          object state.
#
# There must be a more compact way of doing this

sub compare_with_list {
    my $self = shift;

    my $flag = shift;
    my $key = shift;
    my @list = @_;

    my @result = ();

    my ($casesens, $minmatch);
    if ($flag == 0) {
	$casesens = $self->casesens;
	$minmatch = $self->minmatch;
    } else {
	$casesens = $self->casesenstrans;
	$minmatch = $self->minmatchtrans;
    }

    # Do matches

    # Case Sensitive
    if ($casesens) {

	# Always start with the exact match before proceding to minimum
	# match.
	# We want to make sure that we will always match on the
	# exact match even if alternatives exist (eg COL will always
	# match just COL if the keys are COL and COLOUR)
	# Case insensitive
	@result = grep { /^$key$/ } @list;

	# Proceed to minimum match if we detected nothing
	# Minumum match/ Case sensitive
	if ($#result == -1 && $minmatch) {

	    @result = grep { /^$key/ } @list;

	}

    } else {

	# We want to make sure that we will always match on the
	# exact match even if alternatives exist (eg COL will always
	# match just COL if the keys are COL and COLOUR)
	# First do the exact match (case insensitive)
      {
	local $^W = undef; # To silence warnings about uninitialised values
	@result =  grep { /^$key$/i } @list;
      }
	# If this match came up with something then we will use it
	# Else we will try a minimum match (assuming flag is true)

	# Minumum match/ Case insensitive
	if ($#result == -1 && $minmatch) {

	    @result = grep { /^$key/i } @list;

	}
    }
    return @result;
}




=back

=head1 EXAMPLE

Two examples are shown. The first uses the simplified interface and
the second uses the object-oriented interface.

=head1 Non-OO

   use PDL::Options (':Func');

   %options = parse( {
		   LINE => 1,
		   COLOUR => 'red',
		  },
		  {
		   COLOR => 'blue'
		  }
		);

This will return a hash containg

    %options = (
                 LINE => 1,
                 COLOUR => 'blue'
               )


=head1 Object oriented

The following example will try to show the main points:

   use PDL::Options ();

   # Create new object and supply defaults
   $opt = new PDL::Options(   { Colour => 'red',
	   		        LineStyle => 'dashed',
			        LineWidth => 1
			      }
			   );

   # Create synonyms
   $opt->synonyms( { Color => 'Colour' } );

   # Create translation dictionary
   $opt->translation( { Colour => {
                         'blue' => '#0000ff',
			 'red'  => '#ff0000',
			 'green'=> '#00ff00'
				},
	  	        LineStyle => {
			 'solid' => 1,
			 'dashed' => 2,
			 'dotted' => 3
			 }
		      }
		    );

   # Generate and parse test hash
   $options = $opt->options( { Color => 'green',
			       lines => 'solid',
			      }
			   );

When this code is run, $options will be the reference to a hash
containing the following:

   Colour => '#00ff00',
   LineStyle => 1,
   LineWidth => 1

If full_options() was set to false (0), $options would be a reference
to a hash containing:

   Colour => '#00ff00',
   LineStyle => 1

Minimum matching and case insensitivity can be configured for both
the initial parsing and for the subsequent translating. The translation
can be turned off if not desired.

Currently synonyms are not available for the translation although this
could be added quite simply.

=head1 AUTHOR

Copyright (C) Tim Jenness 1998 (t.jenness@jach.hawaii.edu).  All
rights reserved. There is no warranty. You are allowed to redistribute
this software / documentation under certain conditions. For details,
see the file COPYING in the PDL distribution. If this file is
separated from the PDL distribution, the copyright notice should be
included in the file.

=cut


1;

