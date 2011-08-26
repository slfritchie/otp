provider erlang_vm {
        /* TODO: Create better definitions for these two */
        probe spawn_entry(char *, int);
        probe spawn_return(char *);
        probe file_drv_open_entry(int, char *, int, int); /* key, path, flags, pthread */
        probe file_drv_open_return(int, int, int, int); /* key , success?, fd/errno, pthread */
        probe file_drv_open_i_entry(int); /* pthread */
        /* TODO: Add more */

        /* First real attempt to instrument efile_drv.c */

        /*     0       1              2       3       4,5            6,7,8,9  */          
        /* thread-id, tag,           zero,  command, 2 char* args, 4 int args */
        probe file_drv_entry(int, int, int, int, char *, char *, int, int, int, int);

        /*     0       1              2       3     */
        /* thread-id, tag, work-thread-id,  command */
        probe file_drv_int_entry(int, int, int, int);
        probe file_drv_int_return(int, int, int, int);

        /*     0       1              2       3       4             5         .??. */
        /* thread-id, tag, sched-thread-id, command, int success?, int errno, .??. */
        probe file_drv_return(int, int, int, int, int, int);
};

#pragma D attributes Evolving/Evolving/Common provider erlang_vm provider
#pragma D attributes Private/Private/Common provider erlang_vm module
#pragma D attributes Private/Private/Common provider erlang_vm function
#pragma D attributes Evolving/Evolving/Common provider erlang_vm name
#pragma D attributes Evolving/Evolving/Common provider erlang_vm args
