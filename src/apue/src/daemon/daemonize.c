#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "apue.h"

int daemon_init(void)
{
        pid_t pid;

        if((pid = fork()) < 0)
                err_sys("for error");

        else if(pid > 0)
                exit(0); // parent says good bye

        //child continues
        setsid(); //become session leader 

        if(chdir("/") <0) // change work directory
                err_sys("chdir error");
        
        umask(0); // clear file creation mask

        // @TODO close unnessagry file desciptors
        return 0;
}

int main(void)
{
        daemon_init();
        for(; ;)
                pause();

        exit(0);
}
