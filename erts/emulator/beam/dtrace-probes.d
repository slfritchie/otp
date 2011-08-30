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

provider erlang_vm {
        /* TODO: Create better definitions for these two */
        probe spawn_entry(char *, int);
        probe spawn_return(char *);

        /* TODO: Add more */

        /* Async driver pool */

        probe async_io_pool_add(int, int);  /* Pool member #, post-op queue length */
        probe async_io_pool_get(int, int);  /* Pool member #, post-op queue length */

        /* First real attempt to instrument efile_drv.c */

        /*     0       1              2       3       4,5            6,7,8,9  */          
        /* thread-id, tag,           zero,  command, 2 char* args, 4 int args */
        probe file_drv_entry(int, int, int, int, char *, char *, int64_t, int64_t, int64_t, int64_t);
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

        /*     0       1              2       3       4             5         .??. */
        /* thread-id, tag, sched-thread-id, command, int success?, int errno, .??. */
        probe file_drv_return(int, int, int, int, int, int);

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
