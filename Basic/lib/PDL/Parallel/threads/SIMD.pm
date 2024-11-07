package PDL::Parallel::threads::SIMD;

use strict;
use warnings;
use Carp;

our $VERSION = '0.02';

require Exporter;
our @ISA = qw(Exporter);


our @EXPORT_OK = qw(parallelize parallel_sync parallel_id);

use threads qw(yield);
use threads::shared qw(cond_wait);

our $N_threads = -1;

################
# barrier_sync #
################

our $barrier_count :shared = 0;
our $barrier_state :shared = 'ready';

sub parallel_sync () {
	if ($N_threads < 1) {
		carp("Cannot call parallel_sync outside of a parallelized block");
		return;
	}

	yield until $barrier_state eq 'ready' or $barrier_state eq 'up';

	lock($barrier_count);
	$barrier_state = 'up';
	$barrier_count++;

	if ($barrier_count == $N_threads) {
		$barrier_count--;
		$barrier_state = 'down';
		cond_broadcast($barrier_count);
		yield;
	}
	else {
		cond_wait($barrier_count) while $barrier_state eq 'up';
		$barrier_count--;
		$barrier_state = 'ready' if $barrier_count == 0
	}
}

sub parallel_id () {
	goto \&get_tid;
}

sub get_tid {
	carp("Cannot get parallel_id outside of a parallelized block");
}

##################
# SIMD launching #
##################

sub run_it {
	my ($tid, $subref) = @_;
	no warnings 'redefine';
	local *get_tid = sub { $tid };
	$subref->();
	parallel_sync;
}

# Takes a block, the number of threads, and any thread arguments
sub parallelize (&$) {
	my ($sub, $requested_threads,) = @_;
	# For now, prevent nested launches
	croak("Cannot nest parallelized blocks (yet)")
		unless $N_threads == -1;
	croak("Must request a positive number of parallelized threads")
		unless $requested_threads =~ /^\d/ and $requested_threads > 0;
	# This does not need to be localized once nested launches gets
	# figured out. But for now, localize it so it gets restored to -1 at
	# the end of the function call.
	local $N_threads = $requested_threads;
#	local $barrier_count = 0;
#	local $barrier_state = 'ready';

	# Launch N-1 threads...
	my @to_join = map {
		threads->create(\&run_it, $_, $sub)
	} (1..$N_threads-1);
	# ... and execute the last thread in this, the main thread
	run_it(0, $sub);

	# Reap the threads
	for my $thr (@to_join) {
		$thr->join;
	}
}

1;

__END__

=head1 NAME

PDL::Parallel::threads::SIMD - launch and synchronize
Single-Instruction-Multiple-Dataset code

=head1 VERSION

This documentation describes version 0.02 of PDL::Parallel::threads::SIMD.

=head1 SYNOPSIS

 use PDL::Parallel::threads::SIMD qw(parallelize parallel_sync parallel_id);

 # Launch five threads that all print a statement
 parallelize {
   my $pid = parallel_id;
   print "Hello from parallel thread $pid\n";
 } 5;

 my @positions :shared;

 # Run 47 time steps, performing the calculations
 # for the time steps in parallel
 my $size = 100;
 my $N_threads = 10;
 my $stride = $size / $N_threads;
 parallelize {
   my $pid = parallel_id;

   # Set this thread's portion of the positions to zero
   my $start = $stride * $pid;
   my $end = $start + $stride - 1;
   @positions[$start..$end] = (0) x $stride;

   for (1..47) {
     # First make sure all the threads are lined up
     parallel_sync;

     # Now calculate the next positions
     $positions[$_] += $velocities[$_] for $start .. $end;
   }
 } $N_threads;

=head1 DESCRIPTION

In my experience, parallel algorithms are nearly always expressed in a form
called single-instruction, multiple-dataset (SIMD). That is, the exact same
code runs in multiple threads, and the only difference between the threads
is the data they manipulate. This is certainly the case for MPI
and CUDA, two high-performance parallel computing frameworks. The goal of
this module is to provide a means for you to write single-machine SIMD code
in Perl. It won't be as performant as MPI, CUDA, or OpenCL, but with a little
work I hope it can give decent results. In the very least, I hope it can
serve as a good pedagogical tool for understanding parallel algorithms.

SIMD code needs three facilities: a fast mechanism for
data sharing, a means to enforce barrier synchronization, and an indication
of which thread is which, typically in the form of a thread id. This module
provides a way to realize the second and third of these; data sharing is
already available thanks to general Perl data sharing (not fast, but it is
easy to share data) and L<PDL::Parallel::threads>, which provides a simple
way to share PDL data across threads in a way that is quite fast.

=for soon
I<At some point, a discussion for launching a general series of workhorse
threads might be useful. For now, I am going to avoid that discussion.>

The main element that this module provides is the L</parallelize> function,
which allows for a simple and obvious specification for your SIMD code. From
within the block, you can obtain the parallelized thread id, which is a
block-specific number between 0 and one less the number of threads executing
in your parallelized block. You obtain the parallel thread id by calling
L</parallel_id>. Also from within the block, you can enforce a
barrier synchronization point using L</parallel_sync>.

For example, here's a complete working script that demonstrates the use of
L</parallelize> and L</parallel_id>:

 use PDL::Parallel::threads::SIMD qw(parallelize parallel_id);
 parallelize {
   my $pid = parallel_id;
   print "Hello from parallel thread $pid\n"
 } 10;

When I run this on my machine, I get this output:

 Hello from parallel thread 1
 Hello from parallel thread 2
 Hello from parallel thread 3
 Hello from parallel thread 4
 Hello from parallel thread 5
 Hello from parallel thread 6
 Hello from parallel thread 7
 Hello from parallel thread 8
 Hello from parallel thread 0
 Hello from parallel thread 9

Look closely at that output and you should notice that between thread 8 and
9 comes thread 0. In general, parallel threads have no guarantee of ordering
and for longer parallelized blocks the eventual order for such a printout is
often essentially random.

As you can see, the block that you provide to L</parallelize> gets executed
ten times, but within each block the value returned by L</parallel_id> is a
unique integer between 0 and 9. Perl assigns a unique id to every thread
that it runs, so my use of the phrase I<parallel thread ids> here is a
deliberate way to distinguish this id from Perl's thread id. Perl's thread
id will incrementally increase throughout the life of your program,
increasing with each thread that you spawn, but the
parallel thread id will always begin counting from zero for a given
parallelized block.

Why would you want each thread to have a different number that is distinct
from its Perl-assigned thread id? The reason is that having such unique,
sequential, and normalized numbers makes it very easy for you to divide the
work between the threads in a simple and predictable way. For example, in
the code shown below, the bounds for the slice are calculated in a
thread-specific fashion based on the parallel thread id.

 use PDL;
 use PDL::Parallel::threads;
 use PDL::Parallel::threads:SIMD qw(parallelize parallel);
 use PDL::NiceSlice;

 # Load the data with 7 million elements into $to_sum...
 # ...
 # Share it.
 $to_sum->share_as('to-sum');

 # Also allocate some shared, temproary memory:
 my $N_threads = 10;
 zeroes($N_threads)->share_as('workspace');

 my $stride = $to_sum->nelem / $N_threads;

 # Parallelize the sum:
 parallelize {
   my $pid = parallel_id;
   my ($to_sum, $temporary)
     = retrieve_pdls('to-sum', 'workspace');

   # Calculate the thread-specific slice bounds
   my $start = $stride * $pid;
   my $end = $start + $stride - 1;
   $end = $to_sum->nelem - 1 if $end >= $to_sum->nelem;

   # Perform this thread's sum
   $temporary($pid)
     .= $to_sum($start:$end)->sum;
 });

 # This code will not run until that launch has returned
 # for all threads, so at this point we can assume the
 # workspace memory has been filled.
 my $final_sum = retrieve_pdls('workspace')->sum;

 print "The sum is $final_sum\n";

As mentioned in the last comment in that example code, the last
L</parallelize> block will always finish executing before the next line of
Perl code in your script. In other words, all the threads perform a
barrier synchronization just before returning control to your code. Any why
would anybody want to force code to wait at a barrier, you ask?

Nontrivial multi-threaded code must be able to set locations in the code
that all threads must reach before any threads go forward. This is called
barrier synchronization and is important when your threaded code has
multiple stages. A particularly important example of an algorithm that needs
the ability to set barrier synchronization points is a time-stepping
simulation. In that case, you need to make sure that all of your threads
have a chance to reach the "end" of the time step before moving to the next
time step, since ostensibly the results of one thread's time step depend on
the previous results of other threads. If the calculations for each thread
on one step depend on all (or at least some of) the threads having completed
a previous set of calculations, you should use a barrier synchronization
event by calling L</parallel_sync>.

In light of the synchronization that occurs just before returning control to
your code, you can conceptualize the timeline of your code's execution as
follows:

 PDL::Parallel::threads::SIMD Execution
 ======================================

         main thread
             |
             |
             |
             V
 th0 th1 th2 th3 th4 th5 ... th(N-1)
  |   |   |   |   |   |        |
  |   |   |   |   |   |        |
  V   V   V   V   V   V        V
         main thread
             |
             |
             |
             V

This is in contrast to, say, CUDA, in which a thread-launch returns
immediately, thus allowing you to perform calculations on your CPU while you
wait for the GPU to finish:

 CUDA Execution
 ==============

 main thread
     |
     |
     |
     | --> thread launch
     |   th0 th1 th2 th3 th4 th5 ... th(N-1)
     |    |   |   |   |   |   |        |
     |    |   |   |   |   |   |        |
     |    V   V   V   V   V   V        V
     |
     |
     |
     |
     V

It also contrasts with MPI, in which there is no I<main thread> to speak of:
all execution occurs in one thread or another, and any central coordination
needs to be specifically orchestrated through a chosen thread.

It is important to note that if any thread makes a call to L</parallel_sync>,
I<ALL> threads must make a call to L</parallel_sync>. Otherwise, the thread
will hang until, possibly, the next call for barrier synchronization, and
that could lead to I<VERY> confusing apparent errors in logic. For example:

 ...
 parallelize {
   my $pid = parallel_id;

   # do some calculations

   # Do some thread-specific work
   if ($pid < 5) {
     # Notice the *two* barriers set up here:
     parallel_sync;
     # Do something that requires synchronization
     parallel_sync;
   }
   else {
     # THIS PART IS NECESSARY TO PREVENT PROBLEMS
     # Call parallel_sync the same number of times
     # in this else block as in the if block
     parallel_sync;
     parallel_sync;
   }
 } 10;

As a general rule, avoid putting L</parallel_sync> in conditional blocks
like C<if> statements. C<while> loops are another possible problem if the
condition within the while loop (more specifically, the number of iterations
through the C<while> loop) depends on thread-specific aspects of the data.
You can do it, of course, but you have to be very careful that I<all>
threads make the same number of calls at the same algorithmically-intended
point in the execution.

=head1 FUNCTIONS

This module provides three functions: one for lanching a block of code in
parallel across multiple threads, and two that are meant to be called within
that block: a function for synchronizing the execution of the different
threads executing that block and a function to obtain the parallel block's
sequential id.

=head2 parallelize

=for ref

Launches a block of code in parallel across a bunch of threads.

=for usage

  parallelize BLOCK N_THREADS

This function requires two arguments: the block to execute and the number of
threads to launch to execute this block, and returns nothing. This is the
means by which you specify the code that you want run in parallel.

=head2 parallel_sync

=for ref

Synchronizes all threads at the given barrier.

Usage:

=for usage

  parallelize {
    # ...

    parallel_sync;

    # ...

  } $N_threads;

This function enforces barrier synchronization among all the threads in your
parallelized block. It takes no arguments and does not return anything.

The barrier synchronization is tightly coupled with the L</parallelize>
function: you can only call C<parallel_sync> from the middle of a
L</parallelize> block. If you call C<parallel_sync> from outside a
L</parallelize> block, you will get an error.

I need to include an example and exposition on when and why to synchronize...

=head2 parallel_id

=for ref

Gives the thread's I<parallel id>.

Usage:

=for usage

  parallelize {
    # ...

    my $pid = parallel_id;

    # ...

  } $N_threads;

From within the L</parallelize> block, you obtain the current thread's
parallel id with this simple function. When called outside the scope of a
L</parallelize> block, the function simply croaks.

=head1 DIAGNOSTICS

This module does not croak. It does, however, issue a handful of warnings.

=over

=item C<< Cannot call parallel_sync outside of a parallelized block >>

You tried to issue a barrier synchronization (L</parallel_sync>) outside the
context of a L</parallelize> block, but that's the only context where it
makes sense.

=item C<< Cannot get parallel_id outside of a parallelized block >>

You will get this warning when you ask for a parallel id from code that is
not executing in a parallel block. The resulting return value will be the
undefined value.

=item C<< Cannot nest parallelized blocks (yet) >>

This exception gets thrown when you have a L<parallelize> block within
another L<parallelize> block. That's not presently allowed, though I'm open
to any ideas for implementing it if you have any. :-)

=item C<< Must request a positive number of parallelized threads >>

If you send something that's not a positive integer as the number of threads
to launch on your parallelized block, you will get this error. Always
specify a positive integer number.

=back

=head1 LIMITATIONS

I am actually quite pleased with how this module has turned out, but there
are certainly some limitations. For example, you cannot launch a parallel
block from within another parallel block. You can still create and join
threads, you just cannot do it with the L</parallelize> function.

I'm sure there are plenty of limitations, but it's hard for me to see what
differentiates a design goal from a limitation. Feedback on this would be
much appreciated.

=head1 BUGS

None known at this point.

=head1 SEE ALSO

The basic module for Perl parallel computing is L<threads>.

Work on this module was originally inspired by work on
L<PDL::Parallel::threads>, so you might want to check that out.

Modules related to scientific parallel computing include
L<PDL::ParallelCPU>, L<Parallel::MPI>, L<Parallel::MPI::Simple>,
L<PDL::Parallel::MPI> and L<OpenCL>.

Other modules provide alternative parallel computing frameworks. These may
be less suitable for scientific computing, but will likely serve other
purposes: L<Parallel::Loops>, Gearman, L<Parallel::ForkManager>, L<forks>,
L<Thread::Pool>.

=head1 AUTHOR, COPYRIGHT, LICENSE

This module was written by David Mertens. The documentation is copyright (C)
David Mertens, 2012. The source code is copyright (C) Northwestern University,
2012. All rights reserved.

This module is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 DISCLAIMER OF WARRANTY

Parallel computing is hard to get right, and it can be exacerbated by errors
in the underlying software. Please do not use this software in anything that
is mission-critical unless you have tested and verified it yourself. I cannot
guarantee that it will perform perfectly under all loads. I hope this is
useful and I wish you well in your usage thereof, but BECAUSE THIS SOFTWARE
IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE SOFTWARE, TO THE
EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING
THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE SOFTWARE "AS IS"
WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT
NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
SOFTWARE IS WITH YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE
COST OF ALL NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL
ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE
THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE LIABLE TO YOU FOR DAMAGES,
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING
OUT OF THE USE OR INABILITY TO USE THE SOFTWARE (INCLUDING BUT NOT LIMITED
TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU
OR THIRD PARTIES OR A FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER
SOFTWARE), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGES.

=cut
