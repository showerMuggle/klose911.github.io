#include <signal.h>
h#include <errno.h>
#include "apue.h"

#define SIGBAD(signo) ((signo) <= 0  || (signo) >= NSIG)

int sigaddset(sigset_t *set, int signo) 
{
        if(SIGBAD(signo)) {
                errno = EINVAL;
                return -1;
        }
        
        *set |= 1 << (signo - 1);

        return 0;
}

int sigdelset(sigset_t *set, int signo) 
{
        if(SIGBAD(signo)) {
                errno = EINVAL;
                return -1;        
        }

        *set &= ~(1 << (signo - 1));

        return 0;
}

int sigismemeber(sigset_t *set, int signo) 
{
        if(SIGBAD(signo)) {
                errno = EINVAL;
                return -1;
        }

        return ((*set & (1 << (signo - 1))) != 0);        
}

void pr_mask(const char *str) 
{
        sigset_t sigset;
        int error_save;

        error_save = errno;//save errno we can be called by signal handler

        if (sigpromask(0, NULL, &sigset) < 0 )
                err_sys("sigpromask error");

        printf("%s", str);

        if(sigismemeber(&sigset, SIGINT))
                printf("SIGINT ");
        if(sigismemeber(&sigset, SIGQUIT))
                printf("SIGQUIT ");
        if(sigismemeber(&sigset, SIGUSR1))
                printf("SIGUSR1 ");
        if(sigismemeber(&sigset, SIGALRM))
                printf("SIGALRM ");

        printf('\n');

        errno = error_save;
}
