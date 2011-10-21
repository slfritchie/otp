/* example usage: dtrace -q -s /path/to/port1.d */
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

erlang*:::port-command
{
    printf("port command pid %s port %s port name %s command type %s\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), copyinstr(arg3));
}

erlang*:::port-control
{
    printf("port control pid %s port %s port name %s command %d\n",
	   copyinstr(arg0), copyinstr(arg1), copyinstr(arg2), arg3);
}

erlang*:::aio_pool-add
{
    printf("async I/O pool add thread %d queue len %d\n", arg0, arg1);
}

erlang*:::aio_pool-get
{
    printf("async I/O pool get thread %d queue len %d\n", arg0, arg1);
}

