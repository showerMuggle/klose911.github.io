#include <signal.h>
#include "apue.h"

static void sig_quit(int);

static void sig_quit(int signo) 
{
        printf("caught SIGQUIT\n");

        if(SIG_ERR == (signal(SIGQUIT, SIG_DFL)) )
                err_sys("can't reset SIGQUIT");

        return;
}


int main(void) 
{
        sigset_t newmask, oldmask, pendmask;

        if(SIG_ERR == (signal(SIGQUIT, sig_quit)))
                err_sys("can't catch SIGQUIT");

        sigemptyset(&newmask);
        sigaddset(&newmask, SIGQUIT);

        //block SIGQUIT and save current signal mask
        if(sigprocmask(SIG_BLOCK, &newmask, &oldmask) < 0 )
                err_sys("SIG_BLOCK error");

        sleep(5); //SIGQUIT remain pending

        if(sigpending(&pendmask) < 0 )
                err_sys("sigpending error");
        if(sigismember(&pendmask, SIGQUIT))
                printf("\nSIGQUIT pending\n");

        //reset signal mask which unblocks SIGQUIT
        if(sigprocmask(SIG_SETMASK, &oldmask, NULL) < 0)
                err_sys("SIG_SETMASK error");

        printf("SIGQUIT unblocked\n");

        sleep(5);

        exit(0);
}
