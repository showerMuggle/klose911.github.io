#include "apue.h"

extern int mysystem(const char *cmdstring);

int main(int argc, char *argv[])
{
        int		status;

        if (argc < 2)
                err_quit("command-line argument required");

        if ((status = mysystem(argv[1])) < 0)
                err_sys("system() error");
        pr_exit(status);

        exit(0);
}
