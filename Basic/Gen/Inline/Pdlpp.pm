package Inline::Pdlpp;

use strict;
use warnings;

use Config;
use Data::Dumper;
use Carp;
use Cwd qw(cwd abs_path);
use PDL::Core::Dev;

$Inline::Pdlpp::VERSION = '0.4';
use base qw(Inline::C);

#==============================================================================
# Register this module as an Inline language support module
#==============================================================================
sub register {
    return {
	    language => 'Pdlpp',
	    aliases => ['pdlpp','PDLPP'],
	    type => 'compiled',
	    suffix => $Config{dlext},
	   };
}

# handle BLESS, INTERNAL, NOISY - pass everything else up to Inline::C
sub validate {
    my $o = shift;
    $o->{ILSM} ||= {};
    $o->{ILSM}{XS} ||= {};
    # Shouldn't use internal linking for Inline stuff, normally
    $o->{ILSM}{INTERNAL} = 0 unless defined $o->{ILSM}{INTERNAL};
    $o->{ILSM}{MAKEFILE} ||= {};
    if (not $o->UNTAINT) {
      my $w = abs_path(PDL::Core::Dev::whereami_any());
      $o->{ILSM}{MAKEFILE}{INC} = qq{"-I$w/Core"};
    }
    $o->{ILSM}{AUTO_INCLUDE} ||= ' '; # not '' as Inline::C does ||=
    my @pass_along;
    while (@_) {
	my ($key, $value) = (shift, shift);
	if ($key eq 'INTERNAL' or
	    $key eq 'BLESS'
	   ) {
	    $o->{ILSM}{$key} = $value;
	    next;
	}
	if ($key eq 'NOISY') {
            $o->{CONFIG}{BUILD_NOISY} = $value;
	    next;
	}
	push @pass_along, $key, $value;
    }
    $o->SUPER::validate(@pass_along);
}

sub add_list { goto &Inline::C::add_list }
sub add_string { goto &Inline::C::add_string }
sub add_text { goto &Inline::C::add_text }

#==============================================================================
# Parse and compile C code
#==============================================================================
sub build {
    my $o = shift;
    # $o->parse; # no parsing in pdlpp
    $o->get_maps; # get the typemaps
    $o->write_PD;
    # $o->write_Inline_headers; # shouldn't need this one either
    $o->write_Makefile_PL;
    $o->compile;
}

#==============================================================================
# Return a small report about the C code..
#==============================================================================
sub info {
    my $o = shift;
    my $txt = <<END;
The following PP code was generated (caution, can be long)...

*** start PP file ****

END
    return $txt . $o->pd_generate . "\n*** end PP file ****\n";
}

sub config {
    my $o = shift;
}

#==============================================================================
# Write the PDL::PP code into a PD file
#==============================================================================
sub write_PD {
    my $o = shift;
    my $modfname = $o->{API}{modfname};
    my $module = $o->{API}{module};
    $o->mkpath($o->{API}{build_dir});
    open my $fh, ">", "$o->{API}{build_dir}/$modfname.pd" or croak $!;
    print $fh $o->pd_generate;
    close $fh;
}

#==============================================================================
# Generate the PDL::PP code (piece together a few snippets)
#==============================================================================
sub pd_generate {
    my $o = shift;
    return join "\n", ($o->pd_includes,
		       $o->pd_code,
		       $o->pd_boot,
		       $o->pd_bless,
		       $o->pd_done,
		      );
}

sub pd_includes {
    my $o = shift;
    return << "END";
pp_addhdr << 'EOH';
$o->{ILSM}{AUTO_INCLUDE}
EOH

END
}

sub pd_code {
    my $o = shift;
    return $o->{API}{code};
}

sub pd_boot {
    my $o = shift;
    if (defined $o->{ILSM}{XS}{BOOT} and
	$o->{ILSM}{XS}{BOOT}) {
	return <<END;
pp_add_boot << 'EOB';
$o->{ILSM}{XS}{BOOT}
EOB

END
    }
    return '';
}


sub pd_bless {
    my $o = shift;
    if (defined $o->{ILSM}{BLESS} and
	$o->{ILSM}{BLESS}) {
	return <<END;
pp_bless $o->{ILSM}{BLESS};
END
    }
    return '';
}


sub pd_done {
  return <<END;
pp_done();
END
}

sub get_maps {
    my $o = shift;
    $o->SUPER::get_maps;
    my $w = abs_path(PDL::Core::Dev::whereami_any());
    push @{$o->{ILSM}{MAKEFILE}{TYPEMAPS}}, "$w/Core/typemap.pdl";
}

#==============================================================================
# Generate the Makefile.PL
#==============================================================================
sub write_Makefile_PL {
    my $o = shift;
    my ($modfname,$module,$pkg) = @{$o->{API}}{qw(modfname module pkg)};
    my $coredev_suffix = $o->{ILSM}{INTERNAL} ? '_int' : '';
    my @pack = [ "$modfname.pd", $modfname, $module ];
    my $stdargs_func = $o->{ILSM}{INTERNAL}
        ? \&pdlpp_stdargs_int : \&pdlpp_stdargs;
    my %hash = $stdargs_func->(@pack);
    delete $hash{VERSION_FROM};
    my %options = (
        %hash,
        VERSION => $o->{API}{version} || "0.00",
        %{$o->{ILSM}{MAKEFILE}},
        NAME => $o->{API}{module},
        INSTALLSITEARCH => $o->{API}{install_lib},
        INSTALLDIRS => 'site',
        INSTALLSITELIB => $o->{API}{install_lib},
        MAN3PODS => {},
        PM => {},
    );
    open my $fh, ">", "$o->{API}{build_dir}/Makefile.PL" or croak;
    print $fh <<END;
use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;
my \@pack = [ "$modfname.pd", "$modfname", "$module" ];
my %options = %\{
END
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 1;
    print $fh Data::Dumper::Dumper(\ %options);
    print $fh <<END;
\};
WriteMakefile(%options);
sub MY::postamble { pdlpp_postamble$coredev_suffix(\@pack); }
END
    close $fh;
}

#==============================================================================
# Run the build process.
#==============================================================================
sub compile {
    my $o = shift;
    # grep is because on Windows, Cwd::abs_path blows up on non-exist dir
    local $ENV{PERL5LIB} = join $Config{path_sep}, map abs_path($_), grep -e, @INC
        unless defined $ENV{PERL5LIB};
    $o->SUPER::compile;
}
sub fix_make { } # our Makefile.PL doesn't need this

1;

__END__

=head1 NAME

Inline::Pdlpp - Write PDL Subroutines inline with PDL::PP

=head1 DESCRIPTION

C<Inline::Pdlpp> is a module that allows you to write PDL subroutines
in the PDL::PP style. The big benefit compared to plain C<PDL::PP> is
that you can write these definitions inline in any old perl script
(without the normal hassle of creating Makefiles, building, etc).
Since version 0.30 the Inline module supports multiple programming
languages and each language has its own support module. This document
describes how to use Inline with PDL::PP (or rather, it will once
these docs are complete C<;)>.

For more information on Inline in general, see L<Inline>.

Some example scripts demonstrating C<Inline::Pdlpp> usage can be
found in the F<Example/InlinePdlpp> directory.


C<Inline::Pdlpp> is a subclass of L<Inline::C>. Most Kudos goes to Brian I.

=head1 Usage

You never actually use C<Inline::Pdlpp> directly. It is just a support module
for using C<Inline.pm> with C<PDL::PP>. So the usage is always:

    use Inline Pdlpp => ...;

or

    bind Inline Pdlpp => ...;

=head1 Examples

Pending availability of full docs a few quick examples
that illustrate typical usage.

=head2 A simple example

   # example script inlpp.pl
   use PDL; # must be called before (!) 'use Inline Pdlpp' calls

   use Inline Pdlpp; # the actual code is in the __Pdlpp__ block below

   $x = sequence 10;
   print $x->inc,"\n";
   print $x->inc->dummy(1,10)->tcumul,"\n";

   __DATA__

   __Pdlpp__

   pp_def('inc',
	  Pars => 'i();[o] o()',
	  Code => '$o() = $i() + 1;',
	 );

   pp_def('tcumul',
	  Pars => 'in(n);[o] mul()',
	  Code => '$mul() = 1;
		   loop(n) %{
		     $mul() *= $in();
		   %}',
   );
   # end example script

If you call this script it should generate output similar to this:

   prompt> perl inlpp.pl
   Inline running PDL::PP version 2.2...
   [1 2 3 4 5 6 7 8 9 10]
   [3628800 3628800 3628800 3628800 3628800 3628800 3628800 3628800 3628800 3628800]

Usage of C<Inline::Pdlpp> in general is similar to C<Inline::C>.
In the absence of full docs for C<Inline::Pdlpp> you might want to compare
L<Inline::C>.

=head2 Code that uses external libraries, etc

The script below is somewhat more complicated in that it uses code
from an external library (here from Numerical Recipes). All the
relevant information regarding include files, libraries and boot
code is specified in a config call to C<Inline>. For more experienced
Perl hackers it might be helpful to know that the format is
similar to that used with L<ExtUtils::MakeMaker|ExtUtils::MakeMaker>. The
keywords are largely equivalent to those used with C<Inline::C>. Please
see below for further details on the usage of C<INC>,
C<LIBS>, C<AUTO_INCLUDE> and C<BOOT>.

   use PDL; # this must be called before (!) 'use Inline Pdlpp' calls

   use Inline Pdlpp => Config =>
     INC => "-I$ENV{HOME}/include",
     LIBS => "-L$ENV{HOME}/lib -lnr -lm",
     # code to be included in the generated XS
     AUTO_INCLUDE => <<'EOINC',
   #include <math.h>
   #include "nr.h"    /* for poidev */
   #include "nrutil.h"  /* for err_handler */

   static void nr_barf(char *err_txt)
   {
     fprintf(stderr,"Now calling croak...\n");
     croak("NR runtime error: %s",err_txt);
   }
   EOINC
   # install our error handler when loading the Inline::Pdlpp code
   BOOT => 'set_nr_err_handler(nr_barf);';

   use Inline Pdlpp; # the actual code is in the __Pdlpp__ block below

   $x = zeroes(10) + 30;;
   print $x->poidev(5),"\n";

   __DATA__

   __Pdlpp__

   pp_def('poidev',
	   Pars => 'xm(); [o] pd()',
	   GenericTypes => [L,F,D],
	   OtherPars => 'long idum',
	   Code => '$pd() = poidev((float) $xm(), &$COMP(idum));',
   );


=head1 Pdlpp Configuration Options

For information on how to specify Inline configuration options, see
L<Inline>. This section describes each of the configuration options
available for Pdlpp. Most of the options correspond either to MakeMaker or
XS options of the same name. See L<ExtUtils::MakeMaker> and L<perlxs>.

=head2 AUTO_INCLUDE

Specifies extra statements to automatically included. They will be
added onto the defaults. A newline char will be automatically added.
Does essentially the same as a call to C<pp_addhdr>. For short
bits of code C<AUTO_INCLUDE> is probably syntactically nicer.

    use Inline Pdlpp => Config => AUTO_INCLUDE => '#include "yourheader.h"';

=head2 BLESS

Same as C<pp_bless> command. Specifies the package (i.e. class)
to which your new I<pp_def>ed methods will be added. Defaults
to C<PDL> if omitted.

    use Inline Pdlpp => Config => BLESS => 'PDL::Complex';

=head2 BOOT

Specifies C code to be executed in the XS BOOT section. Corresponds to
the XS parameter. Does the same as the C<pp_add_boot> command. Often used
to execute code only once at load time of the module, e.g. a library
initialization call.

=head2 CC

Specify which compiler to use.

=head2 CCFLAGS

Specify extra compiler flags.

=head2 INC

Specifies an include path to use. Corresponds to the MakeMaker parameter.

    use Inline Pdlpp => Config => INC => '-I/inc/path';

=head2 LD

Specify which linker to use.

=head2 LDDLFLAGS

Specify which linker flags to use.

NOTE: These flags will completely override the existing flags, instead
of just adding to them. So if you need to use those too, you must
respecify them here.

=head2 LIBS

Specifies external libraries that should be linked into your
code. Corresponds to the MakeMaker parameter.

    use Inline Pdlpp => Config => LIBS => '-lyourlib';

or

    use Inline Pdlpp => Config => LIBS => '-L/your/path -lyourlib';

=head2 MAKE

Specify the name of the 'make' utility to use.

=head2 MYEXTLIB

Specifies a user compiled object that should be linked in. Corresponds
to the MakeMaker parameter.

    use Inline Pdlpp => Config => MYEXTLIB => '/your/path/yourmodule.so';

=head2 OPTIMIZE

This controls the MakeMaker OPTIMIZE setting. By setting this value to
'-g', you can turn on debugging support for your Inline
extensions. This will allow you to be able to set breakpoints in your
C code using a debugger like gdb.

=head2 TYPEMAPS

Specifies extra typemap files to use. Corresponds to the MakeMaker parameter.

    use Inline Pdlpp => Config => TYPEMAPS => '/your/path/typemap';

=head2 NOISY

Show the output of any compilations going on behind the scenes. Turns
on C<BUILD_NOISY> in L<Inline::C>.

=head1 BUGS

=head2 C<do>ing inline scripts

Beware that there is a problem when you use
the __DATA__ keyword style of Inline definition and
want to C<do> your script containing inlined code. For example

   # myscript.pl contains inlined code
   # in the __DATA__ section
   perl -e 'do "myscript.pl";'
 One or more DATA sections were not processed by Inline.

According to Brian Ingerson (of Inline fame) the workaround is
to include an C<Inline-E<gt>init> call in your script, e.g.

  use PDL;
  use Inline Pdlpp;
  Inline->init;

  # perl code

  __DATA__
  __Pdlpp__

  # pp code

=head2 C<PDL::NiceSlice> and C<Inline::Pdlpp>

There is currently an undesired interaction between
L<PDL::NiceSlice|PDL::NiceSlice> and C<Inline::Pdlpp>.
Since PP code generally contains expressions
of the type C<$var()> (to access piddles, etc)
L<PDL::NiceSlice|PDL::NiceSlice> recognizes those incorrectly as
slice expressions and does its substitutions. For the moment
(until hopefully the parser can deal with that) it is best to
explicitly switch L<PDL::NiceSlice|PDL::NiceSlice> off before
the section of inlined Pdlpp code. For example:

  use PDL::NiceSlice;
  use Inline::Pdlpp;

  $x = sequence 10;
  $x(0:3)++;
  $x->inc;

  no PDL::NiceSlice;

  __DATA__

  __C__

  ppdef (...); # your full pp definition here

=head1 ACKNOWLEDGEMENTS

Brian Ingerson for creating the Inline infrastructure.

=head1 AUTHOR

Christian Soeller <soellermail@excite.com>

=head1 SEE ALSO

L<PDL>

L<PDL::PP>

L<Inline>

L<Inline::C>

=head1 COPYRIGHT

Copyright (c) 2001. Christian Soeller. All rights reserved.

This program is free software; you can redistribute it and/or
modify it under the same terms as PDL itself.

See http://pdl.perl.org

=cut
