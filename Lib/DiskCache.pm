=head1 NAME

PDL::DiskCache -- simple caching object for tieing lists of data

=head1 SYNOPSIS

NON-OO:
   use PDL::DiskCache;
   tie @a,'PDL::DiskCache', \@files, \%options;
   imag $a[3];

OO:
   use PDL::DiskCache;
   $a = diskcache(\@files,\%options);
   imag $a->[3];
or
   use PDL::DiskCache;
   $a = new PDL::DiskCache(\@files,\%options);
   imag $a->[4];

=over 3

=item \@files 

an array ref containing a list of file names

=item \%options 

a hash ref containing options for the PDL::DiskCache object (see "TIEARRAY"
below for details)

=back

=head1 DESCRIPTION

PDL::DiskCache stores one dimension of a data set on disk, caching some of them
in memory.  It's useful for operations where you have to look at a large
collection of files one or a few at a time.  

PDL::DiskCache connects to FITS files by default but can connect to any
sort of file at all -- the read/write routines are the only place
where it examines the underlying data.  To use PDL::DiskCache with other
sorts of data, subclass it or, for ease of quick-and-dirty use, you
can pass in the read and write routines in the options hash.

Items are swapped out on a FIFO basis, so if you have 10 slots
and an expression with 10 items in it then you're OK (but you probably
want more slots than that); but if you use more items in an expression than
there are slots, thrashing will occur!

The OO interface is preferred, since you then have access to all the methods
and not just the normal array-access methods.

=head1 Shortcomings & caveats

There's no file locking, so you could really hose yourself by having two of
these things going at once on the same files.

Since this is a tied array, things like Dumper traverse it transparently.
That is sort-of good but also sort-of dangerous.  You wouldn't want to
PDL::Dumper::sdump() a large PDL::DiskCache, for example -- that would defeat
the purpose of using a PDL::DiskCache in the first place...

=head1 Author, license, no warranty

Copyright 2001, Craig DeForest

This code may be distributed under the same terms as Perl itself
(license available at http://www.perl.org).  Copying, reverse engineering,
distribution, and modification are explicitly allowed so long as this notice
is preserved intact and modified versions are clearly marked as such.

If you modify the code and it's useful, please send a copy of the modified
version to cdeforest@solar.stanford.edu.

This package comes with NO WARRANTY.

=head1 FUNCTIONS

=cut

######################################################################
# Package initialization
$PDL::DiskCache::VERSION = 1.0;
 
use strict;
use Carp;

=head2 diskcache

Object constructor. 

=over 3

=item Synopsis

  $a = diskcache(\@f,\%options);

=item Options

see the TIEARRAY options,below.

=back

=cut

sub diskcache {
  my($f,$opt) = @_;
  return PDL::DiskCache::new('PDL::DiskCache',$f,$opt);
}

sub PDL::DiskCache::new {
  my($class,$f,$opt) = @_;
  my($a)=[];

  my($b) = tie @{$a},$class,$f,$opt;
  if(wantarray) {
    return ($a,bless($b,$class));
  } else {
    return $a;
  }
}

*PDL::DiskCache::diskcache = *diskcache;

=head2 TIEARRAY

Tied-array constructor.  

=over 3

=item Synopsis

  TIEARRAY(class,\@f,\%options)

=item Options

ro (default 0): If set, treat the files as read-only (modifications
to the tied array will only persist until the changed elements are
swapped out)

rw (default 1): If set, allow reading and writing to the files.
Because there's currently no way to determine reliably whether a PDL
has been modified, rw files are always written to disk when they're
swapped out -- this causes a slight performance hit.

mem (default 20): Number of files to be cached in memory at once.

read (default \&rfits): A function ref pointing to code that will read
list objects from disk.  The function must have the same syntax as
rfits: $object = rfits(filename).

write (default \&wfits): A function ref pointing to code that will
write list objects to disk.  The function must have the same syntax as
wfits: func(object,filename).

verbose (default 0): Get chatty.

=back

=cut

sub PDL::DiskCache::TIEARRAY { 
  my($class,$f,$opt) = @_;

  croak "PDL::DiskCache needs array ref as 2nd arg (did you pass an array instead?)\n"
    if(ref $f ne 'ARRAY');
  my($new) = {files   => $f                                # File list
	      , n       => scalar(@{$f})                     # no. of el.
	      , write   => $opt->{write} || \&main::wfits  # Write routine
	      , read    => $opt->{read} || \&main::rfits   # Read routine
	      , mem     => $opt->{mem} || 20               # No. of mem slots
	      , rw      => (!($opt->{ro}))                 # rw or ro
	      , fdex    => []    # Current file stored in each slot, by slot
	      , slot    => []    # Current slot in which each file is stored
	      , cache   => []    # Actual cached stuff gets held here
	      , opt     => {}    # Options stashed here for later reference
              , cache_next => 0  # Next cache slot to be used
	      };
  foreach $_(keys %{$opt}) {
    $new->{opt}->{$_} = $opt->{$_};
  }

  return bless($new,$class);
}

=head2 purge

Remove an item from the oldest slot in the cache, writing to disk as necessary.
You also send in how many slots to purge (default 1; sending in -1 purges
everything.)

For most uses, a nice MODIFIED flag in the data structure could save
some hassle here.  But PDLs can get modified out from under us 
with slicing and .= -- so for now we always assume everything is tainted
and must be written to disk.

=cut

sub PDL::DiskCache::purge {
  my($me,$n) = @_,1;
  $n = $me->{mem} if($n<0);
  
  print "purging $n items..." if($me->{opt}->{verbose});

    
  my($dex) = $me->{cache_next};

  local($_);
  for(1..$n) {
    if($me->{rw}) {
      print "writing $me->{files}->[$me->{fdex}->[$dex]]: " 
	if($me->{opt}->{verbose});

      eval {&{$me->{write}}($me->{cache}->[$dex],
			    $me->{files}->[$me->{fdex}->[$dex]]);
	  };
      print "WARNING: PDL::DiskCache::purge: problems with write of ".$me->{files}->[$me->{fdex}->[$dex]].", item $me->{fdex}->[$dex] from slot $dex: `$@' (".$me->{opt}->{varname}.") \n" if($@);
      $@ = 0;

      print "ok.\n" if($me->{opt}->{verbose});
    }
    

    print "Purging item $dex (file $me->{fdex}->[$dex])...\n" if($me->{opt}->{verbose});
    undef $me->{slot}->[$me->{fdex}->[$dex]];  # Purge from slot location list
    undef $me->{fdex}->[$dex];                 # Purge from slot fdex list
    undef $me->{cache}->[$dex];                # Purge from memory

    $dex++;
    $dex %= $me->{mem};
  }
  print "...done with purge.\n" if($me->{opt}->{verbose});
}

=head2 FETCH

Fetching routine.  (Does it have to be an lvalue?)

=cut

sub PDL::DiskCache::FETCH {
  my($me,$i) = @_;
  
  if($i < 0 || $i >= $me->{n}) {
    carp("PDL::DiskCache: Element $i is outside range of 0-",$me->{n}-1,"\n");
    return undef;
  }

  if(defined $me->{slot}->[$i]) {
    print "Item $i is in the cache...\n" if ($me->{opt}->{verbose});
    return ($me->{cache}->[$me->{slot}->[$i]]);
  }
  
  ### Got here -- we have to get the item from disk.  

  print "Item $i ($me->{files}->[$i]) not present. Retrieving..."
    if($me->{opt}->{verbose});
  
  if(defined($me->{fdex}->[$me->{cache_next}])) {
    print "cache full..." if($me->{opt}->{verbose});

    $me->purge(1);
  } 
  
  my($a) = $me->{cache_next};
  $me->{cache}->[$a] = eval { 
    &{$me->{read}}($me->{files}->[$i])
    } ;
  undef $@; # Keep this from hanging anything else.
  print "result is ",(defined $me->{cache}->[$a] ? "" : "un")."defined.\n"
    if($me->{opt}->{verbose});

  $me->{slot}->[$i] = $me->{cache_next};
  $me->{fdex}->[$me->{cache_next}] = $i;
  $me->{cache_next}++;
  $me->{cache_next} %= $me->{mem};
  $me->{cache}->[$a];
}

sub PDL::DiskCache::STORE {
  my($me, $i, $val) = @_;
  
  if( $me->{slot}->[$i] ) {
    print "Storing index $i, in cache\n" if($me->{opt}->{verbose});
    return $me->{cache}->[$me->{slot}->[$i]] = $val;
  } else {
    print "Storing index $i, not in cache\n" if($me->{opt}->{verbose});
    if(defined ($me->{fdex}->[$me->{cache_next}])) {
      print "cache full..." if($me->{opt}->{verbose});
      $me->purge(1);
    }
    
    my($a) = $me->{cache_next};
    $me->{slot}->[$i] = $a;
    $me->{fdex}->[$a] = $i;
    $me->{cache_next}++;
    $me->{cache_next} %= $me->{mem};

    return $me->{cache}->[$a] = $val;
  }

  croak("This never happens");

}
 
sub PDL::DiskCache::FETCHSIZE { 
  my($me) = shift;
  $me->{n};
}

sub PDL::DiskCache::STORESIZE { 
  my($me,$newsize) = @_;
  if($newsize > $me->{n}) {
    croak("PDL::DiskCache:  Can't augment array size (yet)!\n");
  }
  
  for( my($i) = $newsize-1; $i<$me->{n}; $i++ )  {
    if(defined $me->{slot}->[$i]) {
      if($me->{rw}) {
	print "Writing $me->{files}->[$me->{fdex}->[$i]]\n"
	  if($me->{opt}->{verbose});
	eval {&{$me->{write}}($me->{cache}->[$me->{slot}->[$i]],
			     $me->{files}->[$i]);
	    };
	$@ = 0; # Keep from hanging anything else
      }
      undef $me->{fdex}->[$me->{slot}->[$i]];
    }
  }
  $#{$me->{slot}} = $newsize-1;
  $#{$me->{files}} = $newsize-1;
  $me->{n} = $newsize;
}

=head2 sync

In a rw cache, flush all items out to disk but retain them in the cache.
This is useful primarily for cache protection and could be slow.  Because
we have no way of knowing what's modified and what's not in the cache,
all elements are always flushed from an rw cache.  For ro caches,
this is a not-too-slow (but safe) no-op.

=cut

sub PDL::DiskCache::sync {
  my($me) = shift;
  
  print "PDL::DiskCache::sync\n" if($me->{opt}->{verbose});
  
  if($me->{rw}) {
    for(0..$me->{mem}-1) {
      if(defined $me->{fdex}->[$_]) {

	print "  writing $me->{files}->[$me->{fdex}->[$_]]...\n"
	  if($me->{opt}->{verbose});

	eval {&{$me->{write}}($me->{cache}->[$_],
			      $me->{files}->[$me->{fdex}->[$_]]);
	    };
	$@ = 0; # keep from hanging anything else
      }
    }
  }
}

=head2 DESTROY

Synchronize the cache out to disk if it's an rw cache, before allowing
it to be broken down by the destructor crew.

=cut

sub PDL::DiskCache::DESTROY {
  my($me) = shift;

  $me->sync;

}

# return true
1;
