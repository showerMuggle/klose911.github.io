#include <signal.h>
#include "apue.h"

static void sig_int(int);
volatile sig_atomic_t quitflag;


int main(void) 
{
        sigset_t newmask, oldmask, zeromask;

        if(SIG_ERR == (signal(SIGINT, sig_int)))
                err_sys("signal(SIGINT) error");
        if(SIG_ERR == (signal(SIGQUIT, sig_int)))
                err_sys("signal(SIGQUIT) error");

        sigemptyset(&zeromask);

        sigemptyset(&newmask);
        sigaddset(&newmask, SIGQUIT);

        //block SIGQUIT and save current signal mask
        if(sigprocmask(SIG_BLOCK, &newmask, &oldmask) < 0)
                err_sys("SIG_BLOCK error");

        while(0 == quitflag)
                sigsuspend(&zeromask);

        //SIGQUIT is now caught and is now blocked; do whatever
        quitflag = 0;

        //reset signal mask which unblocks SIGQUIT
        if(sigprocmask(SIG_SETMASK, &oldmask, NULL) < 0)
                err_sys("SIG_SETMASK error");

        exit(0);        
}


static void sig_int(int signo) 
{
        if(SIGINT == signo)
                printf("\ninterupt\n");
        else if(SIGQUIT == signo) {
                printf("\nquit\n");
                quitflag = 1;
        }
        return;
}
