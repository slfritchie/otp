#ifndef DTRACE_HELPERS_H
#define DTRACE_HELPERS_H 1

#define DTRACE_TERM_BUF_SIZE 256

inline void
dtrace_pid_str(Process *process, char *process_buf);

inline void
dtrace_fun_decode(Process *process,
                  Eterm module, Eterm function, int arity,
                  char *process_buf, char *mfa_buf);

#endif /* DTRACE_HELPERS_H */
