package Inline::Pdlpp;

use strict;
require Inline;

use Config;
use Data::Dumper;
use Carp;
use Cwd qw(cwd abs_path);
require PDL::Core::Dev;

$Inline::Pdlpp::VERSION = '0.2';
@Inline::Pdlpp::ISA = qw(Inline);

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

#==============================================================================
# Validate the Pdlpp config options
#==============================================================================
sub usage_validate {
    my $key = shift;
    return <<END;
The value of config option '$key' must be a string or an array ref

END
}

sub validate {
    my $o = shift;

    $o->{ILSM} ||= {};
    $o->{ILSM}{XS} ||= {};
    # having internal on shouldn't matter
    $o->{ILSM}{INTERNAL} = 1 unless defined $o->{ILSM}{INTERNAL};
    $o->{ILSM}{MAKEFILE} ||= {};
    if (not $o->UNTAINT) {
      my $w = abs_path(PDL::Core::Dev::whereami_any());
      $o->{ILSM}{MAKEFILE}{INC} = "-I$w/Core";
    }
    $o->{ILSM}{AUTO_INCLUDE} ||= '';
    $o->{ILSM}{PPFLAGS} ||= '';
    while (@_) {
	my ($key, $value) = (shift, shift);
	if ($key eq 'MAKE' or
	    $key eq 'INTERNAL' or
	    $key eq 'BLESS' or
	    $key eq 'NOISY'
	   ) {
	    $o->{ILSM}{$key} = $value;
	    next;
	}
	if ($key eq 'CC' or
	    $key eq 'LD') {
	    $o->{ILSM}{MAKEFILE}{$key} = $value;
	    next;
	}
	if ($key eq 'LIBS') {
	    $o->add_list($o->{ILSM}{MAKEFILE}, $key, $value, []);
	    next;
	}
	if ($key eq 'INC' or
	    $key eq 'MYEXTLIB' or
	    $key eq 'OPTIMIZE' or
	    $key eq 'CCFLAGS' or
	    $key eq 'LDDLFLAGS') {
	    $o->add_string($o->{ILSM}{MAKEFILE}, $key, $value, '');
	    next;
	}
	if ($key eq 'TYPEMAPS') {
	    croak "TYPEMAPS file '$value' not found"
	      unless -f $value;
	    my ($path, $file) = ($value =~ m|^(.*)[/\\](.*)$|) ?
	      ($1, $2) : ('.', $value);
	    $value = abs_path($path) . '/' . $file;
	    $o->add_list($o->{ILSM}{MAKEFILE}, $key, $value, []);
	    next;
	}
	if ($key eq 'AUTO_INCLUDE' or
            $key eq 'PPFLAGS') {
	    $o->add_text($o->{ILSM}, $key, $value, '');
	    next;
	}
	if ($key eq 'BOOT') {
	    $o->add_text($o->{ILSM}{XS}, $key, $value, '');
	    next;
	}
	my $class = ref $o; # handles subclasses correctly.
	croak "'$key' is not a valid config option for $class\n";
    }
}

sub add_list {
    my $o = shift;
    my ($ref, $key, $value, $default) = @_;
    $value = [$value] unless ref $value eq 'ARRAY';
    for (@$value) {
	if (defined $_) {
	    push @{$ref->{$key}}, $_;
	}
	else {
	    $ref->{$key} = $default;
	}
    }
}

sub add_string {
    my $o = shift;
    my ($ref, $key, $value, $default) = @_;
    $value = [$value] unless ref $value;
    croak usage_validate($key) unless ref($value) eq 'ARRAY';
    for (@$value) {
	if (defined $_) {
	    $ref->{$key} .= ' ' . $_;
	}
	else {
	    $ref->{$key} = $default;
	}
    }
}

sub add_text {
    my $o = shift;
    my ($ref, $key, $value, $default) = @_;
    $value = [$value] unless ref $value;
    croak usage_validate($key) unless ref($value) eq 'ARRAY';
    for (@$value) {
	if (defined $_) {
	    chomp;
	    $ref->{$key} .= $_ . "\n";
	}
	else {
	    $ref->{$key} = $default;
	}
    }
}

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

#     return <<END;
#No information is currently generated when using inline pdlpp.
#
#END

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
    open PD, "> $o->{API}{build_dir}/$modfname.pd"
      or croak $!;
    print PD $o->pd_generate;
    close PD;
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
    if (defined $o->{ILSM}{XS}{BLESS} and
	$o->{ILSM}{XS}{BLESS}) {
	return <<END;
pp_bless $o->{ILSM}{XS}{BLESS};
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

    my $typemap = '';
    $typemap = "$Config::Config{installprivlib}/ExtUtils/typemap"
      if -f "$Config::Config{installprivlib}/ExtUtils/typemap";
    $typemap = "$Config::Config{privlibexp}/ExtUtils/typemap"
      if (not $typemap and -f "$Config::Config{privlibexp}/ExtUtils/typemap");
    warn "Can't find the default system typemap file"
      if (not $typemap and $^W);

    unshift(@{$o->{ILSM}{MAKEFILE}{TYPEMAPS}}, $typemap) if $typemap;

    if (not $o->UNTAINT) {
	require FindBin;
	if (-f "$FindBin::Bin/typemap") {
	    push @{$o->{ILSM}{MAKEFILE}{TYPEMAPS}}, "$FindBin::Bin/typemap";
	}
    }

    my $w = abs_path(PDL::Core::Dev::whereami_any());
    push @{$o->{ILSM}{MAKEFILE}{TYPEMAPS}}, "$w/Core/typemap.pdl";
}

#==============================================================================
# Generate the Makefile.PL
#==============================================================================
sub write_Makefile_PL {
    my $o = shift;
    $o->{ILSM}{xsubppargs} = '';
    for (@{$o->{ILSM}{MAKEFILE}{TYPEMAPS}}) {
	$o->{ILSM}{xsubppargs} .= "-typemap $_ ";
    }

    my %options = (
		   VERSION => $o->{API}{version} || '0.00',
		   %{$o->{ILSM}{MAKEFILE}},
		   NAME => $o->{API}{module},
		  );
    
    open MF, "> $o->{API}{build_dir}/Makefile.PL"
      or croak;
    
    print MF <<END;
use ExtUtils::MakeMaker;
my %options = %\{       
END

    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 1;
    print MF Data::Dumper::Dumper(\ %options);

    print MF <<END;
\};
WriteMakefile(\%options);

# Remove the Makefile dependency. Causes problems on a few systems.
sub MY::makefile { '' }
END
    close MF;
}

#==============================================================================
# Run the build process.
#==============================================================================
sub compile {
    my ($o, $perl, $make, $cmd, $cwd);
    $o = shift;
    $o->compile_pd; # generate the xs file
    my ($module, $modpname, $modfname, $build_dir, $install_lib) = 
      @{$o->{API}}{qw(module modpname modfname build_dir install_lib)};

    -f ($perl = $Config::Config{perlpath})
      or croak "Can't locate your perl binary";
    $make = $o->{ILSM}{MAKE} || $Config::Config{make}
      or croak "Can't locate your make binary";
    $cwd = &cwd;
    ($cwd) = $cwd =~ /(.*)/ if $o->UNTAINT;
    my $noisy = $o->{ILSM}{NOISY} ? '| tee' : '';
    for $cmd ("$perl Makefile.PL $noisy > out.Makefile_PL 2>&1",
	      \ &fix_make,   # Fix Makefile problems
	      "$make $noisy > out.make 2>&1",
	      "$make pure_install $noisy > out.make_install 2>&1",
	     ) {
	if (ref $cmd) {
	    $o->$cmd();
	}
	else {
	    ($cmd) = $cmd =~ /(.*)/ if $o->UNTAINT;
	    chdir $build_dir;
	    system($cmd) and do {
#		$o->error_copy;
                chdir $cwd; # back to original dir
		croak <<END;

A problem was encountered while attempting to compile and install your Inline
$o->{API}{language} code. The command that failed was:
  $cmd

The build directory was:
$build_dir

To debug the problem, cd to the build directory, and inspect the output files.

END
	    };
	    chdir $cwd;
	}
    }

    if ($o->{API}{cleanup}) {
	$o->rmpath($o->{API}{directory} . '/build/', $modpname);
	unlink "$install_lib/auto/$modpname/.packlist";
	unlink "$install_lib/auto/$modpname/$modfname.bs";
	unlink "$install_lib/auto/$modpname/$modfname.exp"; #MSWin32 VC++
	unlink "$install_lib/auto/$modpname/$modfname.lib"; #MSWin32 VC++
    }
}

# compile the pd file into xs using PDL::PP
sub compile_pd {
  my $o = shift;
  my ($perl,$inc,$cwd);
  my ($modfname,$module,$pkg,$bdir) =
    @{$o->{API}}{qw(modfname module pkg build_dir)};

  -f ($perl = $Config::Config{perlpath})
    or croak "Can't locate your perl binary";

  if ($o->{ILSM}{INTERNAL}) {
    my $w = abs_path(PDL::Core::Dev::whereami_any());
    $w =~ s%/((PDL)|(Basic))$%%; # remove the trailing subdir
    $w .= '/blib/lib' unless $w =~ m|/blib/lib|;
    $inc = "-I$w"; # make sure we find the PP stuff
  } else { $inc = '' }

  my $ppflags = $o->{ILSM}{PPFLAGS};
  my $cmd = << "EOC";
$perl $ppflags $inc "-MPDL::PP qw[$module NONE $modfname $pkg]" $modfname.pd > out.pdlpp 2>&1
EOC
  # print STDERR "executing\n\t$cmd...\n";
  $cwd = &cwd;
  chdir $bdir;
  system($cmd) and do {
                chdir $cwd; # back to original dir
		croak <<END;

A problem was encountered while attempting to compile and install your Inline
$o->{API}{language} code. The command that failed was:
  $cmd

The build directory was:
$bdir

To debug the problem, cd to the build directory, and inspect the output files.

END
	      };
  chdir $cwd;
}

#==============================================================================
# This routine fixes problems with the MakeMaker Makefile.
#==============================================================================
my %fixes = (
	     INSTALLSITEARCH => 'install_lib',
	     INSTALLDIRS => 'installdirs',
	     XSUBPPARGS => 'xsubppargs',
	     INSTALLSITELIB => 'install_lib',
	    );

sub fix_make {
    use strict;
    my (@lines, $fix);
    my $o = shift;
    
    $o->{ILSM}{install_lib} = $o->{API}{install_lib};
    $o->{ILSM}{installdirs} = 'site';
    
    open(MAKEFILE, "< $o->{API}{build_dir}/Makefile")
      or croak "Can't open Makefile for input: $!\n";
    @lines = <MAKEFILE>;
    close MAKEFILE;
    
    open(MAKEFILE, "> $o->{API}{build_dir}/Makefile")
      or croak "Can't open Makefile for output: $!\n";
    for (@lines) {
	if (/^(\w+)\s*=\s*\S+.*$/ and
	    $fix = $fixes{$1}
	   ) {
	    print MAKEFILE "$1 = $o->{ILSM}{$fix}\n"
	}
	else {
	    print MAKEFILE;
	}
    }
    close MAKEFILE;
}

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


C<Inline::Pdlpp> is mostly a shameless rip-off of C<Inline::C>.
Most Kudos goes to Brian I.

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

   $a = sequence 10;
   print $a->inc,"\n";
   print $a->inc->dummy(1,10)->tcumul,"\n";

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

   $a = zeroes(10) + 30;;
   print $a->poidev(5),"\n";

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

Show the output of any compilations going on behind the scenes. Uses
C<tee> which must be available on your computer. Default is off.

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

  $a = sequence 10;
  $a(0:3)++;
  $a->inc;

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
