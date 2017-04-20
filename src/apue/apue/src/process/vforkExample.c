#include "apue.h"

// external variable in initialized data
int glob = 6;

int main(void)
{
        // automatic variable on the stack
        volatile int var;
        pid_t pid;

        var = 88;
        printf("before fork\n");

        if((pid = vfork()) < 0) {
                err_sys("fork error");
        } else if(pid == 0) { //child 
                glob++; // change variable 
                var++; 
                _exit(0); //child terminated
                //exit(0);
        }

        //parent
        printf("pid = %d, glob = %d, var = %d\n", getpid(), glob, var);
        exit(0);
}
