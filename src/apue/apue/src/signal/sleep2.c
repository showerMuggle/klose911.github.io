#include <setjmp.h>
#include <signal.h>
#include <unistd.h>

static jmp_buf env_alrm;

static void sig_alrm(int signo);
static void sig_int(int signo);
static unsigned int sleep2(unsigned int nsecs);


int main(void) 
{
        unsigned int unslept;

        if (SIG_ERR == (signal(SIGINT, sig_int)) )
                err_sys("signal(SIGINT) error");

        unslept = sleep2(10);

        printf("sleep2 returned: %u\n", unslept);

        exit(0);
        
}

static void sig_alrm(int signo)
{
        longjmp(env_alrm, 1);
}

static void sig_int(int signo) 
{
        int i;
        volatile int j;

        printf("\n sig_int starting \n");

        for(i = 0; i < 200000; i++) {
                j += i * i;
                printf("i is %d, j is %d\n", i, j);
        }
        
        
        printf("sig_int finished\n");

        return;
}

static unsigned int sleep2(unsigned int nsecs) 
{
        if( SIG_ERR == signal(SIGALRM, sig_alrm))
                return nsecs;

        if(0 == setjmp(env_alrm) ) {
                alarm(nsecs);
                pause();
        }

        return alarm(0);
}

        
        
