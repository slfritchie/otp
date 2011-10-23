/* example usage: dtrace -q -s /path/to/messages.d */
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

typedef struct {                /* Must match definition in dtrace-wrapper.h */
    int64_t     label;
    int64_t     serial1;
    int64_t     serial2;
} seq_trace_t;

erlang*:::message-send
{
    printf("send: %s -> %s: %d bytes\n", copyinstr(arg0), copyinstr(arg1), arg2);
}

erlang*:::message-send_stt
{
    sttok = (seq_trace_t *) copyin(arg1, 8*3);
    printf("send: %s token %d {%d,%d} -> %s: %d bytes\n",
           copyinstr(arg0),
           sttok->label, sttok->serial1, sttok->serial2,
           copyinstr(arg1), arg3);
}

erlang*:::message-receive
{
    printf("recv: %s: %d bytes, queue len %d\n", copyinstr(arg0), arg1, arg2);
}
