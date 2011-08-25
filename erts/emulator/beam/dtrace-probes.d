provider erlang_vm {
        /* TODO: Create better definitions for these two */
        probe spawn__entry(char *, int);
        probe spawn__return(char *);
        probe file_drv__open_entry(char *, int);
        probe file_drv__open_return(char *, int, int); /* name, success?, fd/errno */
        /* TODO: Add more */
};

#pragma D attributes Evolving/Evolving/Common provider erlang_vm provider
#pragma D attributes Private/Private/Common provider erlang_vm module
#pragma D attributes Private/Private/Common provider erlang_vm function
#pragma D attributes Evolving/Evolving/Common provider erlang_vm name
#pragma D attributes Evolving/Evolving/Common provider erlang_vm args
