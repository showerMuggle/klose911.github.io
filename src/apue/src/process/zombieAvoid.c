#include <sys/types.h>
#include <sys/wait.h>
#include "apue.h"

int main(void)
{
        pid_t pid;

        if( ( pid = fork() ) < 0)
                err_sys("1. fork error");
        else if (0 == pid) {
                if( ( pid = fork() ) < 0)
                        err_sys("2.fork error");
                else if(pid > 0) //parent from second child = first child
                        exit(0);

                /*
                  The second child; our parent becomes init as soon as our real          parent calls exit() in the statement above.
                  Here is where we'd contine executing,
                  knowing that when we are done, init will reap our status
                */
                sleep(2);
                printf("second child parent pid = %d\n", getppid());
                exit(0);
        }

        if(waitpid(pid, NULL, 0) != pid)
                err_sys("waitpid error");

        /*
          We are the parent(the orginal process).
          We continue executing,
          knowing that we are not the parent of the second child.
        */

        exit(0);
        
}
