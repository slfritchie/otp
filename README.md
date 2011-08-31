The dtrace-experiment branch of Erlang/OTP
==========================================

The purpose of this branch is to bring DTrace user-space probes to the
Erlang virtual machine.  At the moment, it is a proof of concept:
unpolished, rough, and occasionally ugly.  The goal: merge this code
(or something philosophically similar) into Ericsson's Erlang/OTP
source distribution.

History
-------

The first implementation of DTrace probes for the Erlang virtual
machine was presented at the [2008 Erlang User Conference] [4].  Most
of the source from that effort has been lost.  If you have a copy of
that work, please contact **slfritchie**.

Several users have created Erlang port drivers, linked-in drivers, or
NIFs that allow Erlang code to try to activate a probe,
e.g. `foo_module:dtrace_probe("message goes here!")`.

Goals
-----

1. Annotate as much of the Erlang VM as is practical.
   * The initial goal is to trace file I/O operations.
2. Support all platforms that implement DTrace: OS X, Solaris,
   and (I hope) FreeBSD and NetBSD.
3. To the extent that it's practical, support SystemTap on Linux
   via DTrace provider compatibility.
4. Allow Erlang code to supply annotations.

Supported platforms
-------------------

The autoconf procedure is supported, I believe, for OS X/Snow Leopard
and OpenSolaris/64-bit.  Just add the `--enable-dtrace` option your
command to run the `configure` script.

Implementation summary
----------------------

So far, most effort has been focused on the `efile_drv.erl` code,
which implements most file I/O on behalf of the Erlang virtual
machine.  This driver also presents a big challenge: its use of an I/O
worker pool (enabled by using the `erl +A 8` flag, for example) makes
it much more difficult to trace I/O activity because each of the
following may be executed in a different Pthread:

* I/O initiation (Erlang code)
* I/O proxy process handling, e.g. read/write when file is not opened
  in `raw` mode, operations executed by the code & file server processes.
  (Erlang code)
* `efile_drv` command setup (C code)
* `efile_drv` command execution (C code)
* `efile_drv` status return (C code)

**TODO: keep this description up-to-date.**

In the current prototype, probes for a `file:rename("old-name", "new-name")`of the last three steps can be
formatted like this:

        FUNCTION-NAME:PROBE-NAME     ARGUMENTS
        ------------------------     ---------
          file_output:file_drv_entry 0 517 some-user-tag | 12 | old-name new-name
    invoke_rename:file_drv_int_entry 0 517 5004 | 12
    invoke_rename:file_drv_int_return 0 517 5004 | 12
    file_async_ready:file_drv_return 0 517 some-user-tag | 12 | 0 2 5004

... where the following key can help decipher the output:

* `0 517` is the Erlang scheduler thread number (0) and operation
  counter number (517) assigned to this I/O operation.  Together,
  these two numbers form a unique ID for the I/O operation.
* `12` is the command number for the rename operation.  See the
  definition for `FILE_RENAME` in the source code file `efile_drv.c`.
* `old-name` and `new-name` are the two string arguments for the
  source and destination of the `rename(2)` system call.
* `5004` is the worker pool thread number that will perform the actual
  I/O work.  (I/O worker pool thread ID numbers start at 5,000 to make
  it more difficult to confuse them with Erlang scheduler thread
  numbers.)
* `0 2` means that the command was not successful (0) and that the
  POSIX errno value was 2, a.k.a. `ENOENT`, because the file
  `old-name` does not exist.
* The `file_drv_return` probe was activated by thread 5004, i.e. the
  same thread as the `file_drv_int_entry` and `file_drv_int_return`
  probes.
* The `file_drv_int_return` probe is provided in case the user is
  interested in measuring only the latency of code executed by
  `efile_drv` asynchronous functions by I/O worker pool threads.

So, where does the `some-user-tag` string come from?

At the moment, the user tag comes from code like the following:

    put(dtrace_utag, "some-user-tag"),
    file:rename("old-name", "new-name").

This method of tagging I/O at the Erlang level is subject to change.

Example DTrace probe specification
----------------------------------

        /* Async driver pool */
        
        probe async_io_pool_add(int, int);  /* Pool member #, post-op queue length */
        probe async_io_pool_get(int, int);  /* Pool member #, post-op queue length */
        
        /* thread-id, tag,  user-tag,  command, 2 char* args, 4 int args */
        probe file_drv_entry(int, int, char *, int, char *, char *, int64_t, int64_t, int64_t, int64_t);
        /* thread-id, tag, work-thread-id,  command */
        probe file_drv_int_entry(int, int, int, int);
        probe file_drv_int_return(int, int, int, int);
        /* thread-id, tag, user-tag, command, int success?, int errno, sched-thread-id, .??. */
        probe file_drv_return(int, int, char *, int, int, int, int);



Erlang/OTP
==========

**Erlang** is a programming language used to build massively scalable soft
real-time systems with requirements on high availability. Some of its
uses are in telecom, banking, e-commerce, computer telephony and
instant messaging. Erlang's runtime system has built-in support for
concurrency, distribution and fault tolerance.

**OTP** is set of Erlang libraries and design principles providing
middle-ware to develop these systems. It includes its own distributed
database, applications to interface towards other languages, debugging
and release handling tools.

More information can be found at [erlang.org] [1].

Building and Installing
-----------------------

Information on building and installing Erlang/OTP can be found
in the `INSTALL.md` document.

Contributing to Erlang/OTP
--------------------------

Here are the [instructions for submitting patches] [2].

In short:

*   We prefer to receive proposed updates via email on the
    [`erlang-patches`] [3] mailing list rather than through a pull request.
    Pull requests are not practical because we have a strict policy never to
    merge any untested changes to the development branch (the only exception
    being **obviously** correct changes, such as corrections of typos).

*   We merge all proposed updates to the `pu` (*proposed updates*) branch,
    typically within one working day.

*   At least once a day, the contents of the `pu` branch will be built on
    several platforms (Linux, Solaris, Mac OS X, Windows, and so on) and
    automatic test suites will be run. We will email you if any problems are
    found.

*   If a proposed change builds and passes the tests, it will be reviewed
    by one or more members of the Erlang/OTP team at Ericsson. The reviewer
    may suggest improvements that are needed before the change can be accepted
    and merged.

*   Once or twice a week, a status email called "What's cooking in Erlang/OTP"
    will be sent to the [`erlang-patches`] [3] mailing list.

Copyright and License
---------------------

> %CopyrightBegin%
>
> Copyright Ericsson AB 2010. All Rights Reserved.
>
> The contents of this file are subject to the Erlang Public License,
> Version 1.1, (the "License"); you may not use this file except in
> compliance with the License. You should have received a copy of the
> Erlang Public License along with this software. If not, it can be
> retrieved online at http://www.erlang.org/.
>
> Software distributed under the License is distributed on an "AS IS"
> basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
> the License for the specific language governing rights and limitations
> under the License.
>
> %CopyrightEnd%



   [1]: http://www.erlang.org
   [2]: http://wiki.github.com/erlang/otp/submitting-patches
   [3]: http://www.erlang.org/faq.html
   [4]: http://www.erlang.org/euc/08/
