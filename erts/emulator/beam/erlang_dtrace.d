/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

/**************************************************************
SLF TODO: annotation challenges:

    1. Some calls have port owned directly by user proc, e.g.
       file:open() with 'raw' option.
    2. Some calls have intermediate owner proc, e.g. file:open()
       without 'raw' options.
    3. Some calls are performed by 'file_server_2' process, e.g.
       make_link() and rename().  (usually pid <0.18.0>)
    4. Some are done by 'erl_prim_loader'. (usually pid <0.3.0>)

"Best" official way to annotate efile_drv I/O is to add extra
NUL-terminated string to all calls?
    * But how to get that string from user Erlang code to driver?
    * port_open option isn't flexible enough: if owner is a proxy
      like file_server_2 or non-raw-read/write, proxy isn't good
      enough.
    * file.erl API is cast in stone.
    * abuse proc dict??
        - what about proxies, e.g. non-raw-read/write??
**************************************************************/

provider erlang_vm {
        /* TODO: Create better definitions for these two */
        probe spawn_entry(char *, int);
        probe spawn_return(char *);

        /* TODO: Add more */

        /* Async driver pool */

        probe async_io_pool_add(int, int);  /* Pool member #, post-op queue length */
        probe async_io_pool_get(int, int);  /* Pool member #, post-op queue length */

        /* Probes for efile_drv.c */

        /*     0       1         2       3       4,5            6,7,8,9  */
        /* thread-id, tag,  user-tag,  command, 2 char* args, 4 int args */
        probe file_drv_entry(int, int, char *, int, char *, char *, int64_t, int64_t, int64_t, int64_t);

/*
 * NOTE:
 * For formatting the last N int64_t arguments to the
 * file_drv_entry probe, see:
 *
 *   http://mail.opensolaris.org/pipermail/dtrace-discuss/2006-November/002830.html
 *
 *   "1) you don't need the 'l' printf() modifiers with DTrace ever"
 */

        /*     0       1              2       3     */
        /* thread-id, tag, work-thread-id,  command */
        probe file_drv_int_entry(int, int, int, int);
        probe file_drv_int_return(int, int, int, int);

        /*     0       1       2       3          4             5           6           .??. */
        /* thread-id, tag, user-tag, command, int success?, int errno, sched-thread-id, .??. */
        probe file_drv_return(int, int, char *, int, int, int, int);

/*
 * NOTE: For file_drv_return + SMP + R14B03 (and perhaps other
 *       releases), the sched-thread-id will be the same as the
 *       work-thread-id: erl_async.c's async_main() function
 *       will call the asynchronous invoke function and then
 *       immediately call the drivers ready_async function while
 *       inside the same I/O worker pool thread.
 *       For R14B03's source, see erl_async.c lines 302-317.
 */
};

#pragma D attributes Evolving/Evolving/Common provider erlang_vm provider
#pragma D attributes Private/Private/Common provider erlang_vm module
#pragma D attributes Private/Private/Common provider erlang_vm function
#pragma D attributes Evolving/Evolving/Common provider erlang_vm name
#pragma D attributes Evolving/Evolving/Common provider erlang_vm args

provider erlang {
    /**
     * Fired when a message is sent from one local process to another.
     *
     * @param sender the PID (string form) of the sender
     * @param receiver the PID (string form) of the receiver
     * @param size the size of the message being delivered
     */
    probe send(char *sender, char *receiver, uint32_t size);

    /**
     * Fired when an Eterm structure is being copied.
     *
     * @param size the size of the structure
     */
    probe copy_struct(uint32_t size);

    /**
     * Fired when an Eterm is being copied onto a process.
     *
     * @param proc the PID (string form) of the recipient process
     * @param size the size of the structure
     */
    probe copy_object(char *proc, uint32_t size);

    /* PID, Module, Function, Arity */

    /**
     * Fired whenever a user function is being called.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     * @param depth the stack depth
     */
    probe function__entry(char *p, char *mfa, int depth);

    /**
     * Fired whenever a user function returns.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     * @param depth the stack depth
     */
    probe function__return(char *p, char *mfa, int depth);

    /**
     * Fired whenever a Built In Function is called.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     */
    probe bif__entry(char *p, char *mfa);

    /**
     * Fired whenever a Built In Function returns.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     */
    probe bif__return(char *p, char *mfa);

    /**
     * Fired whenever a Native Function is called.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     */
    probe nif__entry(char *p, char *mfa);

    /**
     * Fired whenever a Native Function returns.
     *
     * @param p the PID (string form) of the process
     * @param mfa the m:f/a of the function
     */
    probe nif__return(char *p, char *mfa);

    /**
     * Fired when a process is spawned.
     *
     * @param p the PID (string form) of the new process.
     * @param mfa the m:f/a of the function
     */
    probe spawn(char *p, char *mfa);

    /**
     * Fired when a process is exiting.
     *
     * @param p the PID (string form) of the exiting process
     * @param reason the reason for the exit (may be truncated)
     */
    probe exit(char *p, char *reason);

    /**
     * Fired when a major GC is starting.
     *
     * @param p the PID (string form) of the exiting process
     * @param need the number of words needed on the heap
     */
    probe gc_major__start(char *p, int need);

    /**
     * Fired when a minor GC is starting.
     *
     * @param p the PID (string form) of the exiting process
     * @param need the number of words needed on the heap
     */
    probe gc_minor__start(char *p, int need);

    /**
     * Fired when a major GC is starting.
     *
     * @param p the PID (string form) of the exiting process
     * @param reclaimed the amount of space reclaimed
     */
    probe gc_major__end(char *p, int reclaimed);

    /**
     * Fired when a minor GC is starting.
     *
     * @param p the PID (string form) of the exiting process
     * @param reclaimed the amount of space reclaimed
     */
    probe gc_minor__end(char *p, int reclaimed);

    /**
     * Fired when a process is scheduled.
     *
     * @param p the PID (string form) of the exiting process
     */
    probe process_scheduled(char *p);
};
