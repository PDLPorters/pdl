=head1 NAME

PDL::Graphics::TriD::Tk - Tk windows / widgets / menus for TriD.

=head1 SYNOPSIS

=head1 DESCRIPTION

=cut

package PDL::Graphics::TriD::Tk;
use strict;

use Tk;
use Tk::Entry;
use Tk::Dialog;

sub get_toplevel {
	my $top = $PDL::Graphics::TriD::Tk::toplevel;
	if(!defined $top) {
		$top = MainWindow->new();
		$top->withdraw;
		$PDL::Graphics::TriD::Tk::toplevel = $top;
	}
	return $top;
}

sub create_menu {
	my($win) = @_;
	my $t = get_toplevel();
	my $m = $t->Menu();
	$m->separator();
	$m->command(-label => 'Foobar', -command => sub {print "DOFOOBAR\n"});
	$m->command(-label => 'Save', -command => sub {print "DOSVEBLABLA\n";
		save_dialog($win);
		});
	$m->command(-label => 'Quit', -command => sub{quit_dialog($win)});
	return $m;
}

sub quit_dialog {
  my $win = shift;
  my $t = get_toplevel();
  my $D = $t->Dialog(
		     -title => 'Are you sure?',
		     -text  => "Will now quit\nAre you sure?",
		     -default_button => 'No',
		     -buttons        => ['No','yes']
		    );
  my $choice = $D->Show;  # use Show for Tk-b9.01
  print "Choice: $choice \n";
  exit if $choice =~ /yes/;
}

sub post_menu {
	my($win,$x,$y) = @_;
	my $menu = $win->{Tk_Menu};
	if(!defined $menu) {
		$menu = create_menu($win);
		$win->{Tk_Menu} = $menu;
	}
	$menu->post($x,$y);
	$menu->grabGlobal();
}

sub create_save_dialog {
	my($win,$m) = @_;

	require PDL::IO::Pic;

	my $sn = \($win->{Tk_SaveName} = "");
	my $st = \($win->{Tk_SaveType} = undef);
	my $s = $m->DialogBox(-title => "Save OpenGL Image",
		-buttons => ["Save","Cancel"]);
	my $f = $s->add("Frame");
	$f->Radiobutton(-text => 'FITS', -variable => $st, -value => 'FITS')
		->pack(-anchor => 'w');
	$f->Radiobutton(-text => 'Suffix', -variable => $st, -value => undef)
		->pack(-anchor => 'w');
	$f->Radiobutton(-text => 'Gif', -variable => $st, -value => 'GIF')
		->pack(-anchor => 'w') if PDL->wpiccan('GIF');
	$f->Radiobutton(-text => 'Jpeg', -variable => $st, -value => 'JPEG')
		->pack(-anchor => 'w') if PDL->wpiccan('JPEG');
	$f->Radiobutton(-text => 'Pnm', -variable => $st, -value => 'PNM')
		->pack(-anchor => 'w') if PDL->wpiccan('PNM');
	$f->Radiobutton(-text => 'PostScript', -variable => $st, -value => 'PS')
		->pack(-anchor => 'w') if PDL->wpiccan('PS');

	$f->pack();
	my $e = $s->add('Entry',-textvariable => $sn);
	$e->pack();
	return $s;
}

sub save_dialog {
	my($win) = @_;
	my $save = $win->{Tk_Save};
	if(!defined $save) {
		$save = create_save_dialog($win,$win->{Tk_Menu});
		$win->{Tk_Save} = $save;
	}
	my $res = $save->Show();
	if($res eq "Save") {
		print "Saving... $win->{Tk_SaveName}, $win->{Tk_SaveType}, (NOT)\n";
		if(-e $win->{Tk_SaveName}) {
			print "FILE EXISTS: NOT OVERWRITING!\n";
			return;
		}
		if($win->{Tk_SaveName} =~ /^\s*$/) {
			print "NO FILENAME SPECIFIED!\n";
			return;
		}
		my $ppdl = $win->read_picture();
		print "GOT PICTURE!\n";
		if ($win->{Tk_SaveType} = 'FITS') {
			require PDL::IO::Misc;
			$ppdl->wfits($win->{Tk_SaveName});
		} else {
		        $ppdl->wpic($win->{Tk_SaveName},{FORMAT => $win->{Tk_SaveType}});
		}
	} elsif($res eq "Cancel") {
		print "Canceled\n";
	}
}

1;
