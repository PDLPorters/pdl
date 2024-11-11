use strict;
use warnings;

eval { require Devel::CheckLib; Devel::CheckLib->import; };
use 5.010_000;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;
use File::Spec;

# this will make PDL::Core::Dev's functions available
# in Makefile.PL files in all subdirectories
require "./Basic/lib/PDL/Core/Dev.pm"; PDL::Core::Dev->import();
$INC{'PDL/Core/Dev.pm'} = 1; # so require P:C:Dev will report success after

my @cpan_deps = my @prereq = (
  'Data::Dumper'        => 2.121,     # for PDL::IO::Dumper
  'File::Map'           => 0.57,      # test new mmap implementation
  'File::Which'         => 0,
  'Filter::Util::Call'  => 0,         # for PDL::NiceSlice
  'Filter::Simple'      => 0.88,      # for new PDL::NiceSlice
  'List::Util'          => '1.33',
  'Pod::Select'         => 0,         # version TBD for PDL::Doc
  'Scalar::Util'        => 0,
  'Storable'            => 1.03,      # for PDL::IO::Storable
  'Text::Balanced'      => '2.05',    # for PDL::NiceSlice
  'Math::Complex'       => 0,
);
my %min_version = (
  'OpenGL' => '0.70', # see also Graphics/TriD/Makefile.PL
  'OpenGL::GLUT' => '0.72', # see also Graphics/TriD/Makefile.PL
  'ExtUtils::F77' => '1.26', # eg 1.20 can leave off -lgfortran
  'Inline' => '0.83', # fixed ILSM-finding
  'Inline::C' => '0.62',
  'Term::ReadKey' => '2.34', #for perldl shell
  'PGPLOT' => '2.29',
  'Alien::proj' => '1.29', # min PROJ 7.1 as T:Proj4 needs degree_output
  'Astro::FITS::Header' => '2.1', # apparently before that, IO::FITS no work
);
for my $opt_dep (sort keys %min_version) {
  (my $file = $opt_dep) =~ s#::#/#g;
  next if !eval { require "$file.pm"; 1 }; # not installed, fine
  next if eval { $opt_dep->VERSION($min_version{$opt_dep}); 1 };
  push @prereq, $opt_dep => $min_version{$opt_dep};
}

##############################
# Hack to include fPIC on x86_64 systems -
# use similar mods to affect CCFLAGS on other systems as needed...

my $ccflags =  $Config{ccflags};
if($Config{archname}=~m/amd64|i686|x86_64/) {
    $ccflags .= " -fPIC";
}

# create GENERATED subdir with *.pm files during 'make dist' (to make metacpan.org happy)
my $preop = '$(PERLRUNINST) -MPDL::Core::Dev -e pdlpp_mkgen $(DISTVNAME)';

my %makefile_hash = (
  PREREQ_PM => { @prereq },
  LICENSE => 'perl',
  CONFIGURE_REQUIRES => {
    'Devel::CheckLib' => '1.01',
    'File::Which'     => 0,
    'ExtUtils::MakeMaker' => '7.12', # working .g.c
    'File::Path'          => 0,
    'ExtUtils::Depends' => '0.402',
  },
  TEST_REQUIRES => {
    'CPAN::Meta' => '2.120900',
    'IPC::Cmd' => '0.72',
    'Test::Exception' => 0,
    'Test::Warn' => 0,  # for t/pptest.t
    "Test::Deep" => 0, # for Test::Deep::PDL tests
  },
  BUILD_REQUIRES => {
    'ExtUtils::MakeMaker' => 0,
    'File::Path'          => 0,
    'Pod::Select'         => 0,
    'ExtUtils::ParseXS'   => '3.21', # ExtUtils::Typemaps::tidy_type
  },
  MIN_PERL_VERSION => '5.014',
  NAME => 'PDL',
  VERSION_FROM => 'Basic/lib/PDL/Core.pm',
  META_MERGE => {
     "meta-spec" => { version => 2 },
     resources => {
        homepage => 'http://pdl.perl.org/',
        bugtracker  => {web=>'https://github.com/PDLPorters/pdl/issues'},
        repository  => {
           url => 'git://github.com/PDLPorters/pdl.git',
           type => 'git',
           web => 'https://github.com/PDLPorters/pdl',
        },
        x_IRC => 'irc://irc.perl.org/#pdl',
     },
     prereqs => {
        configure => {
           recommends => {
              'ExtUtils::F77' => '1.10',
           },
        },
        runtime => {
           suggests => {
              'Sys::SigAction' => 0, # Ctrl-C handling in shells
           },
           recommends => \%min_version,
           requires => { @cpan_deps },
        },
     },
  },
  CCFLAGS => $ccflags,
  dist => { COMPRESS => 'gzip', SUFFIX => 'gz', PREOP => $preop },
  clean => { FILES => 'MANIFEST.bak _Inline/' },
  AUTHOR => 'PerlDL Developers <pdl-general@lists.sourceforge.net>',
  ABSTRACT => 'Perl Data Language',
  BINARY_LOCATION => 'PDL.tar.gz',
);

WriteMakefile(%makefile_hash);

# Extra build target to build the doc database
sub MY::postamble {
  my ($self) = @_;
  package MY;
  my  $text =
'
doctest ::
	cd Basic && make pm_to_blib
	$(NOECHO) $(ECHO) "doctest: Building PDL documentation database in blib ..."
	$(NOECHO) $(PERLRUNINST) Basic$(DFSEP)utils$(DFSEP)scantree.pl

doc_site_install :: doc_pdldoc_install

doc_pdldoc_install :: pure_install # needs installed .../PDL dir to exist
	$(NOECHO) $(ECHO) "doc_site_install: Building PDL documentation database ..."
	$(NOECHO) $(PERLRUNINST) Basic$(DFSEP)utils$(DFSEP)scantree.pl "$(DESTDIR)$(INSTALLSITEARCH)"

doc_vendor_install ::
	$(NOECHO) $(ECHO) "doc_vendor_install: Building PDL documentation database ..."
	$(NOECHO) $(PERLRUNINST) Basic$(DFSEP)utils$(DFSEP)scantree.pl "$(DESTDIR)$(INSTALLVENDORARCH)"

doc_perl_install ::
	$(NOECHO) $(ECHO) "doc_perl_install: Building PDL documentation database ..."
	$(NOECHO) $(PERLRUNINST) Basic$(DFSEP)utils$(DFSEP)scantree.pl "$(DESTDIR)$(INSTALLARCHLIB)"
';

$text .= "\n" . ::coretarget($self);
my $coretest = join ' ', map File::Spec->catfile('t', $_.'.t'), qw(
  01-pptest autoload bad basic bool clump constructor core croak lvalue
  math matrix matrixops nat_complex ops-bitwise ops pdl_from_string
  pdlchar pp_croaking pp_line_numbers primitive-* ppt-* pthread reduce
  slice subclass thread thread_def ufunc
);
$text .= <<EOF;

coretest : core
	cd Basic && \$(FULLPERLRUN) "$::Config{bin}/prove" -I ../blib/lib -I ../blib/arch $coretest
EOF

$text .= <<EOF;

update-ppport:
	cd Basic/Core && \$(PERLRUN) -MDevel::PPPort -e'Devel::PPPort::WriteFile' && \$(PERLRUN) ppport.h --strip

EOF

return $text

}

# the modules to test the ones in full PDL
my @basics; BEGIN { @basics = (
  [qw(Basic IO-Misc)],
  [qw(Basic IO-Pnm)],
  [qw(Basic Lib-Compression)],
  [qw(Basic Lib-FFT)],
  [qw(Basic Lib-ImageND)],
  [qw(Basic Lib-ImageRGB)],
) }
sub coretarget {
    my ($self) = @_;
    # remember the fundamental ones end up far to right as much deps on them
    # a "right" is either scalar (named target) or tuple of
    #     [ \@dir, \@targets, \@prereqs ]
    # @dir is dir parts for use by File::Spec
    # @targets is make targets within that dir
    # @prereqs are named targets - undef=[]
    # all a left's rights are made concurrently, no sequence - list ALL prereqs
    my @left2rights = (
        [
            coregen => [
                [ [ qw(Basic) ], [ qw(pm_to_blib) ], ],
            ]
        ],
        [
            core => [
                [
                  [ 'Basic' ],
                  [ map qq{build_\$(LINKTYPE)_PDL_$_},
                    qw(Core) ],
                  [ 'coregen' ]
                ],
                map [
                    [ 'Basic', $_ ],
                    [ qw(pure_all) ],
                    [ 'coregen' ],
                # the modules in PDL::LiteF, used in t/core.t
                ], qw(Ops Primitive Ufunc Slices Bad Math MatrixOps),
            ]
        ],
        [
            basic => [
                map [
                    $_,
                    [ qw(pm_to_blib linkext) ],
                    [ 'core' ],
                ], @basics,
            ]
        ],
        [
            basictest => [
                map [
                    $_,
                    [ 'test' ],
                    [ 'basic' ],
                ], @basics,
            ]
        ],
    );
    join "\n", map flatten_parallel_target($self, $_), @left2rights;
}

sub format_chunk {
    my ($self, $left, $deps, $dir, $targets) = @_;
    my @m = join ' ', $left, ':', @{$deps||[]};
    my $fsdir = File::Spec->catdir(@$dir);
    push @m, "\t" . $self->oneliner(
        "die \$! unless chdir q($fsdir); exec q(\$(MAKE) @$targets)"
    );
    join '', map "$_\n", @m;
}

# output: list of make chunks with target, deps, recipes
sub flatten_parallel_target {
    my ($self, $left2rights) = @_;
    my ($left, $rights) = @$left2rights;
    my (@deps, @recipes, @otherchunks);
    for my $right (@$rights) {
        if (ref $right) {
            #     [ \@dir, \@targets, \@prereqs ]
            # @dir is dir parts for use by File::Spec
            # @targets is make targets within that dir
            # @prereqs are named targets - undef=[]
            my ($dir, $targets, $prereqs) = @$right;
            my $target_name = parallel_target_mangle($self, $dir, $targets);
            push @deps, $target_name;
            push @otherchunks, format_chunk(
                $self, $target_name, $prereqs, $dir, $targets
            );
        } else {
            push @deps, $right;
        }
    }
    (
        join(' : ', $left, join ' ', @deps) . "\n",
        @otherchunks,
    );
}

sub parallel_target_mangle {
    my ($self, $dir, $targets) = @_;
    my $target = join '_', @$dir, @$targets;
    $target =~ s#[\$\(\)/\\_\.]+#_#g; # avoid ambiguity with filenames
    $target;
}
