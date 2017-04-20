#include <signal.h>
#include "apue.h"

static void sig_usr(int);

int main(void) 
{
        if ( SIG_ERR == signal(SIGUSR1, sig_usr))
                err_sys("can't catch signal SIG_USR1");
        if( SIG_ERR == signal(SIGUSR2, sig_usr))
                err_sys("can't catch signal SIG_USR2");

        for (; ; )
                pause();
        
}

static void sig_usr(int signo)
{
        if (SIGUSR1 == signo)
                printf("received SIGUSR1\n");
        else if (SIGUSR2 == signo)
                printf("received SIGUSR2\n");
        else
                err_dump("received signal %d \n", signo);
        return ;
        
}


