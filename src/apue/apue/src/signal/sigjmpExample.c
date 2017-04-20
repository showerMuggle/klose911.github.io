#include <signal.h>
#include <setjmp.h>
#include <time.h>
#include "apue.h"

static void sig_usr1(int);
static void sig_alrm(int);

static jmp_buf jmpbuf;
static volatile sig_atomic_t canjump;

int main(void) 
{
        if(SIG_ERR == (signal(SIGUSR1, sig_usr1)) )
                err_sys("signal(SIGUSR1) error");
        if(SIG_ERR == (signal(SIGALRM, sig_alrm)) )
                err_sys("signal(SIGALRM) error");

        pr_mask("starting main: ");

        if(sigsetjmp(jmpbuf, 1)) {
                pr_mask("ending main: ");
                exit(0);
        }

        canjump = 1;// now sigsetjmp() is OK

        for(; ;)
                pause();
}


static void sig_usr1(int signo) 
{
        time_t starttime;

        if(0 == canjump)
                return;

        pr_mask("starting sig_usr1:");

        alarm(3); //SIG_ALRM in 3 seconds

        starttime = time(NULL);
        for(; ;) // busy wait for 5 seconds 
                if(time(NULL) > starttime + 5)
                        break;

        pr_mask("finishing sig_usr1: "); 

        canjump = 0;

        siglongjmp(jmpbuf, 1); // jump back to main, do not return 
}

static void sig_alrm(int signo) 
{
        pr_mask("in sig_alrm: ");
        return;
}
