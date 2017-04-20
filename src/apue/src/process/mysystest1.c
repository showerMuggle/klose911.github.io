#include "apue.h"
#include <sys/wait.h>

extern int mysystem(const char *cmdstring);

int main(void)
{
        int		status;

        if ((status = mysystem("date")) < 0)
                err_sys("system() error");
        pr_exit(status);

        if ((status = mysystem("nosuchcommand")) < 0)
                err_sys("system() error");
        pr_exit(status);

        if ((status = mysystem("who; exit 44")) < 0)
                err_sys("system() error");
        pr_exit(status);

        exit(0);
}
