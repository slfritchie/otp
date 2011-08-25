provider erlang_vm {
        /* TODO: Create better definitions for these two */
        probe spawn_entry(char *, int);
        probe spawn_return(char *);
        probe file_drv_open_entry(int, char *, int); /* key, path, flags */
        probe file_drv_open_return(int, int, int); /* key , success?, fd/errno */
        /* TODO: Add more */
};

#pragma D attributes Evolving/Evolving/Common provider erlang_vm provider
#pragma D attributes Private/Private/Common provider erlang_vm module
#pragma D attributes Private/Private/Common provider erlang_vm function
#pragma D attributes Evolving/Evolving/Common provider erlang_vm name
#pragma D attributes Evolving/Evolving/Common provider erlang_vm args
