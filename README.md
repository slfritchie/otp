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

The code has been only very lightly tested on OS X.  It ought to
compile on a Solaris 10 or OpenSolaris machine, but no promises yet.

The autoconf stuff is ugly right now.  It could use some cleaning up.
For example:

* After editing the `erlang_dtrace.d` file, you need to re-run the
* top-level "configure" script in order to update `erlang_dtrace.h`.
* `make clean` will remove `erlang_dtrace.h`.  A build will fail
  unless the top-level "configure" script is re-run to re-create that
  file.
* The `erlang_dtrace.h` file's location should probably be moved to an
  OTP platform-specific build dir, for example,
  `path/to/somewhere/i386-apple-darwin10.8.0`
* There are probably some other build by-products that are also being
  put into the "wrong" directory, for example, `erlang_dtrace.o` for
  Solaris platforms.

Contributions
-------------

Code contributions are welcome!  This is a side project for me (SLF),
so things would go faster if other people are willing to pitch in.
Please use the GitHub pull request mechanism or send me an email
message.

To build from scratch, use this recipe.  If you're an experienced Git
user and wish to add my repository as a remote repository, be my
guest.  Just resume the recipe at command #4.

    % git clone git://github.com/slfritchie/otp.git
    % cd otp
    % git checkout -b dtrace-experiment origin/dtrace-experiment
    % env ERL_TOP=`pwd` ./otp_build autoconf
    % env ERL_TOP=`pwd` ./configure --enable-dtrace + whatever args you need
    % env ERL_TOP=`pwd` make

Then `make install` and then start an Erlang shell via
`/path/to/installed/bin/erl +A 8`.  The Erlang shell's banner should
include `[dtrace]`.

Try using this (ugly) DTrace command to watch file I/O probes in use
(tested on OS X only, sorry):

    dtrace -Z -n 'erlang*:::efile_drv-entry {printf("%d %d %s | %d | %s %s , %d %d %d", arg0, arg1, arg2 == NULL ? "" : copyinstr(arg2), arg3, arg4 == NULL ? "" : copyinstr(arg4), arg5 == NULL ? "" : copyinstr(arg5), arg6, arg7, arg8)} erlang*:::efile_drv-int* {printf("%d %d %d | %d", arg0, arg1, arg2, arg3);} erlang*:::efile_drv-return {printf("%d %d %s | %d | %d %d %d", arg0, arg1, arg2 == NULL ? "" : copyinstr(arg2), arg3, arg4, arg5, arg6 ) ; }'

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

     /**
      * Show the post-add length of the async driver thread pool member's queue.
      *
      * @param pool member number
      * @param new queue length
      */
     probe async_io_pool_add(int, int);

     /**
      * Show the post-get length of the async driver thread pool member's queue.
      *
      * @param pool member number
      * @param new queue length
      */
     probe async_io_pool_get(int, int);

     /* Probes for efile_drv.c */

     /**
      * Entry into the efile_drv.c file I/O driver
      *
      * For a list of command numbers used by this driver, see the section
      * "Guide to probe arguments" in ../../../README.md.  That section
      * also contains explanation of the various integer and string
      * arguments that may be present when any particular probe fires.
      *
      * @param thread-id number of the scheduler Pthread                   arg0
      * @param tag number: {thread-id, tag} uniquely names a driver operation
      * @param user-tag string                                             arg2
      * @param command number                                              arg3
      * @param string argument 1                                           arg4
      * @param string argument 2                                           arg5
      * @param integer argument 1                                          arg6
      * @param integer argument 2                                          arg7
      * @param integer argument 3                                          arg8
      * @param integer argument 4                                          arg9
      */
     probe file_drv_entry(int, int, char *, int, char *, char *,
			  int64_t, int64_t, int64_t, int64_t);

     /*     0       1              2       3     */
     /* thread-id, tag, work-thread-id,  command */
     /**
      * Entry into the driver's internal work function.  Computation here
      * is performed by a async worker pool Pthread.
      *
      * @param thread-id number
      * @param tag number
      * @param worker pool thread-id number
      * @param command number
      */
     probe file_drv_int_entry(int, int, int, int);

     /**
      * Return from the driver's internal work function.
      *
      * @param thread-id number
      * @param tag number
      * @param worker pool thread-id number
      * @param command number
      */
     probe file_drv_int_return(int, int, int, int);

     /**
      * Return from the efile_drv.c file I/O driver
      *
      * @param thread-id number                                            arg0
      * @param tag number                                                  arg1
      * @param user-tag string                                             arg2
      * @param command number                                              arg3
      * @param Success? 1 is success, 0 is failure                         arg4
      * @param If failure, the errno of the error.                         arg5
      * @param thread-id number of the scheduler Pthread executing now     arg6
      */
     probe file_drv_return(int, int, char *, int, int, int, int);

Guide to probe arguments
------------------------

    /* Driver op code: used by file_drv_entry      arg3 */
    /*                 used by file_drv_int_entry  arg3 */
    /*                 used by file_drv_int_return arg3 */
    /*                 used by file_drv_return     arg3 */
    
    #define FILE_OPEN            1                 (probe arg3)
            probe arg6 = C driver dt_i1 = flags;
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_READ            2                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = size;
    
    #define FILE_LSEEK           3                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = offset;
            probe arg8 = C driver dt_i3 = origin;
    
    #define FILE_WRITE           4                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = size;
    
    #define FILE_FSTAT           5                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
    
    #define FILE_PWD             6                 (probe arg3)
            none
    
    #define FILE_READDIR         7                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_CHDIR           8                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_FSYNC           9                 (probe arg3)
                probe arg6 = C driver dt_i1 = fd;
    
    #define FILE_MKDIR          10                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_DELETE         11                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_RENAME         12                 (probe arg3)
            probe arg4 = C driver dt_s1 = old_name;
            probe arg5 = C driver dt_s2 = new_name;
    
    #define FILE_RMDIR          13                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_TRUNCATE       14                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
    
    #define FILE_READ_FILE      15                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_WRITE_INFO     16                 (probe arg3)
            probe arg6 = C driver dt_i1 = mode;
            probe arg7 = C driver dt_i2 = uid;
            probe arg8 = C driver dt_i3 = gid;
    
    #define FILE_LSTAT          19                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_READLINK       20                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_LINK           21                 (probe arg3)
            probe arg4 = C driver dt_s1 = existing_path;
            probe arg5 = C driver dt_s2 = new_path;
    
    #define FILE_SYMLINK        22                 (probe arg3)
            probe arg4 = C driver dt_s1 = existing_path;
            probe arg5 = C driver dt_s2 = new_path;
    
    #define FILE_CLOSE          23                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
    
    #define FILE_PWRITEV        24                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = size;
    
    #define FILE_PREADV         25                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = size;
    
    #define FILE_SETOPT         26                 (probe arg3)
            probe arg6 = C driver dt_i1 = opt_name;
            probe arg7 = C driver dt_i2 = opt_specific_value;
    
    #define FILE_IPREAD         27                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = offsets[0];
            probe arg9 = C driver dt_i4 = size;
    
    #define FILE_ALTNAME        28                 (probe arg3)
            probe arg4 = C driver dt_s1 = path;
    
    #define FILE_READ_LINE      29                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = flags;
            probe arg8 = C driver dt_i3 = read_offset;
            probe arg9 = C driver dt_i4 = read_ahead;
    
    #define FILE_FDATASYNC      30                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
    
    #define FILE_FADVISE        31                 (probe arg3)
            probe arg6 = C driver dt_i1 = fd;
            probe arg7 = C driver dt_i2 = offset;
            probe arg8 = C driver dt_i3 = length;
            probe arg9 = C driver dt_i4 = advise_type;
    
    /* Return codes: used by file_drv_return arg4 */
    
    #define FILE_RESP_OK         0
    #define FILE_RESP_ERROR      1
    #define FILE_RESP_DATA       2
    #define FILE_RESP_NUMBER     3
    #define FILE_RESP_INFO       4
    #define FILE_RESP_NUMERR     5
    #define FILE_RESP_LDATA      6
    #define FILE_RESP_N2DATA     7
    #define FILE_RESP_EOF        8
    #define FILE_RESP_FNAME      9
    #define FILE_RESP_ALL_DATA  10


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
