package Inline::MakePdlppInstallable;

# just a dummy package

package Inline;

#==============================================================================
# override the original Inline::install method
# to allow Inline::Pdlpp code to be installed
#
# this is a hack !
#
# we put the modified function into its own little file
# to keep the runtime impact at a minimum
#
# use as follows in modules containing inlined PDL::PP code:
#
#   use Inline::MakePdlppInstallable;
#   use Inline Pdlpp => ....
#
# hopefully Inline will establishe a proper mechanism soon
# to allow installation of non-C modules -- at least Brian Ingerson
# promised to put it on the TODO list
#==============================================================================

# copied verbatim from Inline 0.43 apart from language_id check below
sub install {
    my ($module, $DIRECTORY);
    my $o = shift;

    # print STDERR "in redefined Inline::install\n";
    croak M64_install_not_c($o->{API}{language_id})
      unless uc($o->{API}{language_id}) eq 'C' ||
	uc($o->{API}{language_id}) eq 'PDLPP'; # also allow Pdlpp !
    croak M36_usage_install_main()
      if ($o->{API}{pkg} eq 'main');
    croak M37_usage_install_auto()
      if $o->{CONFIG}{AUTONAME};
    croak M38_usage_install_name()
      unless $o->{CONFIG}{NAME};
    croak M39_usage_install_version()
      unless $o->{CONFIG}{VERSION};
    croak M40_usage_install_badname($o->{CONFIG}{NAME}, $o->{API}{pkg})
      unless $o->{CONFIG}{NAME} eq $o->{API}{pkg};
#	      $o->{CONFIG}{NAME} =~ /^$o->{API}{pkg}::\w(\w|::)+$/
#	     );

    my ($mod_name, $mod_ver, $ext_name, $ext_ver) = 
      ($o->{API}{pkg}, $ARGV[0], @{$o->{CONFIG}}{qw(NAME VERSION)});
    croak M41_usage_install_version_mismatch($mod_name, $mod_ver, 
					     $ext_name, $ext_ver)
      unless ($mod_ver eq $ext_ver);
    $o->{INLINE}{INST_ARCHLIB} = $ARGV[1];

    $o->{API}{version} = $o->{CONFIG}{VERSION};
    $o->{API}{module} = $o->{CONFIG}{NAME};
    my @modparts = split(/::/,$o->{API}{module});
    $o->{API}{modfname} = $modparts[-1];
    $o->{API}{modpname} = join('/',@modparts);
    $o->{API}{suffix} = $o->{INLINE}{ILSM_suffix};
    $o->{API}{build_dir} = ( $o->{INLINE}{DIRECTORY} . '/build/' . 
			     $o->{API}{modpname}
			   );
    $o->{API}{directory} = $o->{INLINE}{DIRECTORY};
    my $cwd = Cwd::cwd();
    $o->{API}{install_lib} = "$cwd/$o->{INLINE}{INST_ARCHLIB}";
    $o->{API}{location} = "$o->{API}{install_lib}/auto/" .
      "$o->{API}{modpname}/$o->{API}{modfname}.$o->{INLINE}{ILSM_suffix}";
    unshift @::INC, $o->{API}{install_lib};
    $o->{INLINE}{object_ready} = 0;
}


1;
