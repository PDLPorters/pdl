use strict;
use warnings;
use lib qw(lib);
use PDL::Core::Dev;

eval { require Devel::CheckLib; Devel::CheckLib->import; };
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;
use File::Spec::Functions qw(catdir catfile);

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
  'Inline' => '0.83', # fixed ILSM-finding
  'Inline::C' => '0.62',
  'Term::ReadKey' => '2.34', #for perldl shell
  'Astro::FITS::Header' => '2.1', # apparently before that, IO::FITS no work
);
for my $opt_dep (sort keys %min_version) {
  (my $file = $opt_dep) =~ s#::#/#g;
  next if !eval { require "$file.pm"; 1 }; # not installed, fine
  next if eval { $opt_dep->VERSION($min_version{$opt_dep}); 1 };
  push @prereq, $opt_dep => $min_version{$opt_dep};
}

my $pdlroot = catfile qw(script pdl);
my @exe_files = map catfile('script', $_), qw(perldl pdldoc pptemplate);
push @exe_files, "$pdlroot$Config{exe_ext}" if $^O !~ /MSWin/;
my @cleanup = ("$pdlroot$Config{exe_ext}");

##############################
# Hack to include fPIC on x86_64 systems -
# use similar mods to affect CCFLAGS on other systems as needed...

my $ccflags =  $Config{ccflags};
if($Config{archname}=~m/amd64|i686|x86_64/) {
    $ccflags .= " -fPIC";
}

my $pthread_include = $ENV{POSIX_THREADS_INC} // '';
my $pthread_library = $ENV{POSIX_THREADS_LIBS} // '-lpthread';
if ($pthread_library) {
    (my $libname = $pthread_library) =~ s/^-l//;
    if (!eval { check_lib(lib=>$libname,header=>'pthread.h',incpath=>$pthread_include) }) {
        print "\tDevel::CheckLib with '$libname' failed\n";
        $pthread_library = '';
    }
}
if (!$pthread_library) {
    print "PDL will be built without POSIX thread support.\n";
    print "\t==> *NOTE*: PDL threads are unrelated to perl threads (usethreads=y)!\n";
    print "\t==> Enabling perl threads will not help!\n";
}
my $pthread_define  = $pthread_library ? ' -DPDL_PTHREAD ' : '';
my $libs_string = "$pthread_library ".get_maths_libs();

my $hdeps = join ' ', map catfile(qw(lib PDL Core), $_), qw(pdl.h pdlperl.h pdlcore.h pdlbroadcast.h pdlmagic.h);
my @cfiles = qw(pdlcore pdlapi pdlbroadcast pdlconv pdlmagic pdlaffine pdlutil);
my $cobj = join ' ', map catfile(qw(lib PDL Core), qq{$_\$(OBJ_EXT)}), @cfiles;
push @cleanup, $cobj;
my $core_obj = join('/', qw(lib PDL), 'Core$(OBJ_EXT)'); # EUMM XSMULTI targets are / not catfile

# the modules in PDL::LiteF, used in t/core.t
my @BUILD_CORE = qw(Bad Core Math MatrixOps Ops Primitive Slices Ufunc);
# the modules needed in "use PDL"
my @BUILD_BASIC = qw(Compression FFT IO/Misc IO/Pnm ImageND ImageRGB);

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
  },
  TEST_REQUIRES => {
    'CPAN::Meta' => '2.120900',
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
  MIN_PERL_VERSION => '5.016',
  NAME => 'PDL',
  VERSION_FROM => 'lib/PDL.pm',
  EXE_FILES => \@exe_files,
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
  XSMULTI => 1,
  XSBUILD => {
    xs => {
      'lib/PDL/Core' => {
        INC => join(' ',
          PDL::Core::Dev::PDL_INCLUDE(),
          map {length($_) ? qq{"$_"} : ()} $pthread_include
        ),
        OBJECT => "$core_obj $cobj",
      },
    },
  },
  DEFINE => $pthread_define,
  LIBS => [$libs_string],
  depend => {
    $core_obj => $hdeps,
    $cobj => $hdeps,
    (map +(
      "build_dynamic_PDL_".(s#/#_#gr) => catfile(qw($(INST_ARCHLIB) auto PDL), $_, (s#.*/##gr).".\$(DLEXT)"),
      "build_static_PDL_".(s#/#_#gr) => catfile(qw($(INST_ARCHLIB) auto PDL), $_, (s#.*/##gr)."\$(LIB_EXT)"),
    ), @BUILD_CORE, @BUILD_BASIC),
  },
  dist => { COMPRESS => 'gzip', SUFFIX => 'gz', PREOP => $preop },
  clean => { FILES => join ' ',
    qw(MANIFEST.bak _Inline/ .inlinepdlpp/ .inlinewith/ .pptest/), @cleanup,
    catfile(qw(lib PDL Core pdl.h)),
  },
  AUTHOR => 'PerlDL Developers <pdl-general@lists.sourceforge.net>',
  ABSTRACT => 'Perl Data Language',
  BINARY_LOCATION => 'PDL.tar.gz',
);

WriteMakefile(%makefile_hash);

{
package MY; # so that "SUPER" works right
my @pd_srcs;
sub postamble {
  my ($self) = @_;
  my $text = '';
  $text .= <<'EOF';
doctest :: pm_to_blib
	$(NOECHO) $(ECHO) "doctest: Building PDL documentation database in blib ..."
	$(NOECHO) $(PERLRUNINST) utils$(DFSEP)scantree.pl

doc_site_install :: doc_pdldoc_install

doc_pdldoc_install :: pure_install # needs installed .../PDL dir to exist
	$(NOECHO) $(ECHO) "doc_site_install: Building PDL documentation database ..."
	$(NOECHO) $(PERLRUNINST) utils$(DFSEP)scantree.pl "$(DESTDIR)$(INSTALLSITEARCH)"

doc_vendor_install ::
	$(NOECHO) $(ECHO) "doc_vendor_install: Building PDL documentation database ..."
	$(NOECHO) $(PERLRUNINST) utils$(DFSEP)scantree.pl "$(DESTDIR)$(INSTALLVENDORARCH)"

doc_perl_install ::
	$(NOECHO) $(ECHO) "doc_perl_install: Building PDL documentation database ..."
	$(NOECHO) $(PERLRUNINST) utils$(DFSEP)scantree.pl "$(DESTDIR)$(INSTALLARCHLIB)"
EOF
  $text .= <<EOT if $^O !~ /MSWin/;
$pdlroot$::Config{exe_ext} : $pdlroot.c
\t\$(CC) \$(CCFLAGS) \$(CPPFLAGS) \$(OPTIMIZE) \$(LDFLAGS) \$(CCCDLFLAGS) $pdlroot.c -o \$\@
EOT
  my $typespm = ::catfile(qw(lib PDL Types.pm));
  for (qw(pdl.h)) {
    my $base = ::catfile(qw(lib PDL Core), $_);
    $text .= "$base :: $base.PL $typespm\n\t\$(PERLRUN) $base.PL $base\n";
  }
  $text .= "\n" . ::coretarget($self);
  my $coretest = join ' ', map ::catfile('t', $_.'.t'), qw(
    01-pptest autoload bad basic bool clump constructor core croak lvalue
    math matrix matrixops nat_complex ops-bitwise ops pdl_from_string
    pdlchar pp_croaking pp_line_numbers pp_pod primitive-* ppt-* pthread reduce
    slice subclass thread thread_def ufunc
  );
  my $basictest = join ' ', map ::catfile('t', $_.'.t'), qw(
    compression fft imagergb picnorgb picrgb pic_16bit io-pnm pic-rim
    io-misc
  );
  $text .= <<EOF;
basictest : basic
	\$(FULLPERLRUN) "$::Config{bin}/prove" -b $basictest
coretest : core
	\$(FULLPERLRUN) "$::Config{bin}/prove" -b $coretest
update-ppport:
	\$(PERLRUN) -MDevel::PPPort -e'Devel::PPPort::WriteFile' && \$(PERLRUN) lib/PDL/Core/ppport.h --strip
EOF
  $text .= ::pdlpp_postamble_int(@pd_srcs);
  $text;
}
sub _nopl { $_[0] =~ s/[.]PL$//r }
sub init_PM {
  my ($self) = @_;
  $self->SUPER::init_PM;
  my ($pm, $plf) = @$self{qw(PM PL_FILES)};
  for my $f (grep /pdl\.h\.PL$/, keys %$pm) {
    $pm->{ ::catfile(split '/', _nopl($f)) } = _nopl(delete $pm->{$f}); # dealing in postamble but still install
  }
  for my $f (grep /\.PL$/, keys %$pm) {
    $pm->{ $plf->{$f} = _nopl($f) } = _nopl(delete $pm->{$f});
  }
  @pd_srcs = ::pdlpp_eumm_update_deep($self);
}
}

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
      [core => [ 'pm_to_blib', map qq{build_\$(LINKTYPE)_PDL_}.(s#/#_#gr), @BUILD_CORE ]],
      [basic => ['core', (map qq{build_\$(LINKTYPE)_PDL_}.(s#/#_#gr), @BUILD_BASIC)]],
    );
    join "\n", map flatten_parallel_target($self, $_), @left2rights;
}

sub format_chunk {
    my ($self, $left, $deps, $dir, $targets) = @_;
    my @m = join ' ', $left, ':', @{$deps||[]};
    push @m, "\t" . $self->oneliner(
        (@$dir ? "die \$! unless chdir q(".catdir(@$dir)."); " : '').
        "exec q(\$(MAKE) @$targets)"
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
