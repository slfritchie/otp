/* example usage: dtrace -q -s /path/to/efile_drv.d */
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

erlang*:::aio_pool-add
{
    printf("async I/O pool add thread %d queue len %d\n", arg0, arg1);
}

erlang*:::aio_pool-get
{
    printf("async I/O pool get thread %d queue len %d\n", arg0, arg1);
}

erlang*:::efile_drv-entry
{
    printf("efile_drv enter %d %d %s | %d | %s %s , %d %d\n",
	   arg0, arg1, arg2 == NULL ? "" : copyinstr(arg2), arg3,
	   arg4 == NULL ? "" : copyinstr(arg4),
	   arg5 == NULL ? "" : copyinstr(arg5), arg6, arg7)
}

erlang*:::efile_drv-int*
{
    printf("async I/O worker %d %d %d | %d\n", arg0, arg1, arg2, arg3);
}

erlang*:::efile_drv-return
{
    printf("efile_drv return %d %d %s | %d | %d %d %d\n", arg0, arg1,
	   arg2 == NULL ? "" : copyinstr(arg2), arg3, arg4, arg5, arg6);
}
