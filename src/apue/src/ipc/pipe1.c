#include "apue.h"

int main(void)
{
        int		n;
        int		fd[2];
        pid_t	pid;
        char	line[MAXLINE];

        if (pipe(fd) < 0)
                err_sys("pipe error");

        if ((pid = fork()) < 0) {
                err_sys("fork error");
        } else if (pid > 0) {		/* parent */
                close(fd[0]);// close read tunnel of pipe in parent process
                if(write(fd[1], "hello world\n", 12) < 0)
                        err_sys("write to pipe error");
        } else {				/* child */
                close(fd[1]); // close write tunnel of pipe in child process
                n = read(fd[0], line, MAXLINE);
                if(write(STDOUT_FILENO, line, n) < 0)
                        err_sys("write to console error");
        }
    
        exit(0);
}
