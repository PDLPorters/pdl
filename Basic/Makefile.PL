use strict;
use warnings;
use ExtUtils::MakeMaker;
use File::Spec::Functions;
use Config;
use lib qw(lib);
use PDL::Core::Dev;
eval { require Devel::CheckLib; Devel::CheckLib->import; };

my $pdlroot = catfile qw(script pdl);
my @exe_files = map catfile('script', $_), qw(perldl pdldoc pptemplate);
push @exe_files, "$pdlroot$Config{exe_ext}" if $^O !~ /MSWin/;
my @cleanup = ("$pdlroot$Config{exe_ext}");

my @pd_srcs;
undef &MY::postamble; # suppress warning
*MY::postamble = sub {
  my ($self) = @_;
  my $text = '';
  $text .= <<EOT if $^O !~ /MSWin/;
${pdlroot}$::Config{exe_ext} : ${pdlroot}.c
\t\$(CC) \$(CFLAGS) \$(LDFLAGS) \$(CCCDLFLAGS) ${pdlroot}.c -o \$\@
EOT
  my $typespm = catfile(qw(lib PDL Types.pm));
  for (qw(pdl.h)) {
    my $base = "lib/PDL/Core/$_"; # no catfile as init_PM uses File::Find which normalises to /
    $text .= "$base :: $base.PL $typespm\n\t\$(PERLRUN) $base.PL $base\n";
  }
  $text .= "\n" . ::coretarget($self);
  my $coretest = join ' ', map catfile('t', $_.'.t'), qw(
    01-pptest autoload bad basic bool clump constructor core croak lvalue
    math matrix matrixops nat_complex ops-bitwise ops pdl_from_string
    pdlchar pp_croaking pp_line_numbers primitive-* ppt-* pthread reduce
    slice subclass thread thread_def ufunc
  );
  my $basictest = join ' ', map catfile('t', $_.'.t'), qw(
    compression fft imagergb picnorgb picrgb pic_16bit io-pnm pic-rim
    io-misc
  );
  $text .= <<EOF;
basictest : basic pm_to_blib core
	\$(FULLPERLRUN) "$::Config{bin}/prove" -I ../blib/lib -I ../blib/arch $basictest
coretest : core pm_to_blib
	\$(FULLPERLRUN) "$::Config{bin}/prove" -I ../blib/lib -I ../blib/arch $coretest
update-ppport:
	\$(PERLRUN) -MDevel::PPPort -e'Devel::PPPort::WriteFile' && \$(PERLRUN) lib/PDL/Core/ppport.h --strip
EOF
  $text .= pdlpp_postamble_int(@pd_srcs);
  $text;
};

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
my $core_obj = catfile(qw(lib PDL), 'Core$(OBJ_EXT)');

# the modules in PDL::LiteF, used in t/core.t
my @BUILD_CORE = qw(Bad Core Math MatrixOps Ops Primitive Slices Ufunc);
# the modules needed in "use PDL"
my @BUILD_BASIC = qw(Compression FFT IO/Misc IO/Pnm ImageND ImageRGB);
WriteMakefile(
  NAME	=> 'PDL',
  VERSION_FROM => 'lib/PDL/Core.pm',
  MIN_PERL_VERSION => '5.014',
  EXE_FILES => \@exe_files,
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
  INC => PDL::Core::Dev::PDL_INCLUDE(),
  TYPEMAPS => [PDL::Core::Dev::PDL_TYPEMAP()],
  LIBS => [$libs_string],
  depend => {
    $core_obj => 'pm_to_blib '.$hdeps,
    $cobj => $hdeps,
    (map +(
      "build_dynamic_PDL_".(s#/#_#gr) => "\$(INST_ARCHLIB)/auto/PDL/$_/".(s#.*/##gr).".\$(DLEXT)",
      "build_static_PDL_".(s#/#_#gr) => "\$(INST_ARCHLIB)/auto/PDL/$_/".(s#.*/##gr)."\$(LIB_EXT)",
    ), @BUILD_CORE, @BUILD_BASIC),
  },
  clean => { FILES => join ' ',
    qw(MANIFEST.bak _Inline/ .inlinepdlpp/ .inlinewith/ .pptest/), @cleanup,
    catfile(qw(lib PDL Core pdl.h)),
  },
  NO_MYMETA => 1,
);

{
package MY; # so that "SUPER" works right
sub _nopl { $_[0] =~ s/[.]PL$//r }
sub init_PM {
  my ($self) = @_;
  $self->SUPER::init_PM;
  my ($pm, $plf) = @$self{qw(PM PL_FILES)};
  for my $f (grep /pdl\.h\.PL$/, keys %$pm) {
    $pm->{ _nopl($f) } = _nopl(delete $pm->{$f}); # dealing in postamble but still install
  }
  for my $f (grep /\.PL$/, keys %$pm) {
    $pm->{ $plf->{$f} = _nopl($f) } = _nopl(delete $pm->{$f});
  }
  @pd_srcs = ::pdlpp_eumm_update_deep($self);
  delete @$pm{grep /\.c$/, keys %$pm};
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
      [core => [ map qq{build_\$(LINKTYPE)_PDL_}.(s#/#_#gr), @BUILD_CORE ]],
      [basic => [(map qq{build_\$(LINKTYPE)_PDL_}.(s#/#_#gr), @BUILD_BASIC)]],
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
