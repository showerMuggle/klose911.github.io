#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <limits.h>
#include <string.h>
#include <unistd.h>

#include <termios.h>

#include "apue.h"

#define DEV    "/dev/"
#define DEVLEN sizeof(DEV) - 1 

int isatty(int fd);
char *ttyname(int fd);

int main(void)
{
        printf("fd 0: %s\n", isatty(0) ? ttyname(0) : "not a tty");
        printf("fd 1: %s\n", isatty(1) ? ttyname(1) : "not a tty");
        printf("fd 2: %s\n", isatty(2) ? ttyname(2) : "not a tty");

        exit(0);
        
}


int isatty(int fd)
{
        struct termios term;

        return ( tcgetattr(fd, &term) != -1); //true if no error (is a tty)
}

char *ttyname(int fd)
{
        struct stat fdstat, devstat;
        DIR *dp;
        struct dirent *dirp;
        static char  pathname[_POSIX_PATH_MAX + 1];
        char *rval;

        if(0 == isatty(fd))
                return NULL;

        if(fstat(fd, &fdstat) < 0)
                return NULL;

        if(0 == S_ISCHR(fdstat.st_mode))
                return NULL;

        strcpy(pathname, DEV);

        if(NULL == (dp = opendir(DEV)) )
                return NULL;

        rval = NULL;

        while ( (dirp = readdir(dp)) != NULL) {
                if(dirp->d_ino != fdstat.st_ino)
                        continue;

                strncpy(pathname + DEVLEN, dirp->d_name, _POSIX_PATH_MAX - DEVLEN);

                if(stat(pathname, &devstat) < 0)
                        continue;
                if(devstat.st_ino == fdstat.st_ino &&
                   devstat.st_dev == fdstat.st_dev ) {
                        rval = pathname;
                        break;   
                }
        }

        closedir(dp);
        return rval;
}



