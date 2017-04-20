#include <sys/types.h>
#include "apue.h"

int glob = 6;
char buf[] = "a write to stdin\n";

int main(void)
{
        int var;
        pid_t pid;

        var = 88;
        if(write(STDOUT_FILENO, buf, sizeof(buf) - 1) != (sizeof(buf) - 1))
                err_sys("write error");
        printf("before fork\n"); // we don't flush stdout

        if( (pid = fork() ) < 0)
                err_sys("fork error");
        
        else if(0 == pid) { //child process
                // modify variable
                glob++; 
                var++; 
        } else { // parent process
                sleep(2);
        }

        //child & parent share code
        printf("pid = %d, glob = %d, var = %d\n", getpid(), glob, var);
        exit(0);
}
