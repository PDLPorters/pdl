package PDL::Demos::Sound;
use PDL::Demos;

use File::Which qw/which/;

our $player;
our $player_args;

# Find a suitable sound player and provide command line switches
# - aplay comes with ALSA (in Debian: package alsa-utils)
# - play comes with SoX (in Debian: package sox)
my %args = (
    'aplay' => ['--quiet'],     # we mostly use aplay's defaults
    'play'  => [
        '--no-show-progress',
        '--buffer'   => 256,    # makes it work on Win32
        '--type'     => 'raw',
        '--rate'     => 8000,   # the aplay default
        '--bits'     => 8,      # the aplay default
        '--channels' => 1,      # the aplay default
        '--encoding' => 'unsigned-integer',
        '-',                    # using STDIN
        '--channels' => 1,
        ($^O =~ /MSWin32/ ? ('--type' => 'waveaudio') : ()),
    ]
);
$args{sox} = [ @{$args{play}}, '--default' ];
FIND_EXE:
for my $candidate (qw(aplay play sox)) {
    which $candidate  and  do {
        $player = $candidate;
        $player_args = $args{$player};
        last FIND_EXE;
    }
}

sub init {
    return <<'EndOfText';
our $player = $PDL::Demos::Sound::player;
our $player_args = $PDL::Demos::Sound::player_args;
EndOfText
}

my $scale = <<'EndOfScale';

open (my $pipe, '|-', $player, @$player_args)
    or die "Can not start a sound player.  Demo failed.\n";
binmode $pipe;
my $n         = 4000;
my $samples   = pdl[0..$n-1];
my $raw_sound = byte(zeroes($n));
my $amplitude = 80;
for (8,9,10,32/3,12,40/3,15,16) {
    $raw_sound = byte($amplitude*(1+sin($samples*($_/8)*440*3.14/$n)));
    print $pipe ${$raw_sound->get_dataref};
}
close $pipe;
EndOfScale

my $chord = <<'EndOfChord';

open (my $pipe, '|-', $player, @$player_args)
    or die "Can not start a sound player.  Demo failed.\n";
binmode $pipe;
my $n         = 8000;
my $samples   = pdl[0..$n-1];
my $raw_sound = byte(zeroes($n));
my $amplitude = 30;
for (8,10,12,16) {
    $raw_sound += byte($amplitude*(1+sin($samples*($_/8)*440*6.28/$n)));
}
print $pipe ${$raw_sound->get_dataref};
close $pipe;
EndOfChord

my @demo = ([comment => "Listen to Perl and PDL: The A major scale\n"],
            [actnw   => $scale],
            [comment => "Listen to Perl and PDL: A Chord\n"],
            [actnw   => $chord],
        );
sub demo {
    return @demo if $PDL::Demos::Sound::player;
    ([comment => <<EndOfComment]);
This demo requires an external command to play the sound,
unfortunately no suitable candidate could be found.
The following players are supported:
 - 'aplay' (from the ALSA suite) comes with the package 'alsa-utils'
   on Linux
 - 'play' and 'sox' (from SoX) are available as package 'sox' on Linux,
   and can be obtained from https://sourceforge.net/projects/sox/
   for Microsoft Windows and MacOS.
EndOfComment
}

sub info {
    ('sound', 'Sound (requires a sound player)')
}

1;

__END__

=head1 NAME

PDL::Demos::Sound - play PDL-generated sounds

=head1 DESCRIPTION

This module is intended to be auto-discovered by the mechanisms of
L<PDL::Demos>.  That module also defines our interface.

The demo creates some simple sound waves (a scale and a chord) and
pipes them to an external sound player program.  It knows how to
invoke C<aplay> (from alsa-utils), C<play> and C<sox> (the latter two
from SoX - Sound eXchange).  All of these are available as packages in
Linux distributions, SoX is also available from
L<https://sourceforge.net/projects/sox/> for Microsoft Windows and
MacOS.

If no suitable player is discovered, the demo does not die but shows
an explanation where to obtain them.
