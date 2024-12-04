use strict;
use warnings;

eval { require Devel::CheckLib; Devel::CheckLib->import; };
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;
use File::Spec::Functions qw(rel2abs catdir);

use lib rel2abs catdir qw(Basic lib);
use PDL::Core::Dev;

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
  'ExtUtils::F77' => '1.26', # eg 1.20 can leave off -lgfortran
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

sub MY::postamble {
  <<'EOF';
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

coretest :
	cd Basic && $(MAKE) $@
basictest :
	cd Basic && $(MAKE) $@
update-ppport:
	cd Basic && $(MAKE) $@
EOF
}
