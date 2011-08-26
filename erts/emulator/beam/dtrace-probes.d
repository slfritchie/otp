provider erlang_vm {
        /* TODO: Create better definitions for these two */
        probe spawn_entry(char *, int);
        probe spawn_return(char *);
        probe file_drv_open_entry(int, char *, int, int); /* key, path, flags, pthread */
        probe file_drv_open_return(int, int, int, int); /* key , success?, fd/errno, pthread */
        probe file_drv_open_i_entry(int); /* pthread */
        /* TODO: Add more */

        /* 2 char* args, 4 int args, pthread-id */
        probe file_drv_entry(char *, char *, int, int, int, int, int);
        /* int success?, int errno, .... */
        probe file_drv_return(char *, char *, int, int, int, int, int);

};

#pragma D attributes Evolving/Evolving/Common provider erlang_vm provider
#pragma D attributes Private/Private/Common provider erlang_vm module
#pragma D attributes Private/Private/Common provider erlang_vm function
#pragma D attributes Evolving/Evolving/Common provider erlang_vm name
#pragma D attributes Evolving/Evolving/Common provider erlang_vm args
