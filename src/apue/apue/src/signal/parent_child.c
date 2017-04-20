#include <sys/types.h>
#include <signal.h>
#include "apue.h"

static sig_atomic_t sigflag;

static sigset_t newmask, oldmask, zeromask;

static void sig_usr(int signo);

static void TELL_WAIT1(void);
static void TELL_PARENT1(pid_t pid);
static void WAIT_PARENT1(void);
static void TELL_CHILD1(pid_t pid);
static void WAIT_CHILD1(void);

static void charatatime(char *str);

int main(void) 
{
        pid_t pid;

        TELL_WAIT1();

        if( (pid = fork() ) < 0)
                err_sys("fork error");
        else if(0 == pid) {
                WAIT_PARENT1(); //parent goes first
                charatatime("output from child\n");
        } else {
                charatatime("output from parent\n");
                TELL_CHILD1(pid);
        }

        exit(0);
}



static void sig_usr(int signo) 
{
        sigflag = 1;
        return;
}

static void TELL_WAIT1(void) 
{
        if(SIG_ERR == (signal(SIGUSR1, sig_usr)) )
                err_sys("signal(SIGUSR1) error");
        if(SIG_ERR == (signal(SIGUSR2, sig_usr)) )
                err_sys("signal(SIGUSR2) error");

        sigemptyset(&zeromask);

        sigemptyset(&newmask);
        sigaddset(&newmask, SIGUSR1);
        sigaddset(&newmask, SIGUSR2);

        //block SIGUSR1, SIGUSR2 and save current signal mask
        if(sigprocmask(SIG_BLOCK, &newmask, &oldmask) < 0)
                err_sys("SIG_BLOCK error");
}


static void TELL_PARENT1(pid_t pid) 
{
        kill(pid, SIGUSR2); //tell parent we are done 
}

static void WAIT_PARENT1(void) 
{
        while(0 == sigflag)
                sigsuspend(&zeromask); // wait for parent 

        sigflag = 0;

        //reset signal mask to original value
        if(sigprocmask(SIG_SETMASK, &oldmask, NULL) < 0)
                err_sys("SIG_SETMASK error");
}

static void TELL_CHILD1(pid_t pid) 
{
        kill(pid, SIGUSR1); //tell child we are done 
}

static void WAIT_CHILD1(void) 
{
        while(0 == sigflag)
                sigsuspend(&zeromask); // wait for child 

        sigflag = 0;

        //reset signal mask to original value
        if(sigprocmask(SIG_SETMASK, &oldmask, NULL) < 0)
                err_sys("SIG_SETMASK error");
}

static void charatatime(char *str)
{
        char *ptr;
        int c;

        setbuf(stdout, NULL);
        for(ptr = str; c = *ptr++; )
                putc(c, stdout);        
}
