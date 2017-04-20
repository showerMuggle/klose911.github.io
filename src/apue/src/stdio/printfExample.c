#include "apue.h"

void pr_stdio(const char *, FILE *);

int main(void) 
{
        FILE *fp;
        fputs("enter any characters\n", stdout);
        if(EOF == getchar() )
                err_sys("getchar error");
        
        fputs("one line to standard error\n", stderr);

        pr_stdio("stdin", stdin);
        pr_stdio("stdout", stdout);
        pr_stdio("stderr", stderr);

        if(NULL == (fp = fopen("/etc/man.conf", "r")) )
                err_sys("fopen error");
        if(EOF == getc(fp) )
                err_sys("getc error");

        pr_stdio("/etc/man.conf", fp);

        exit(0);
        
}

void pr_stdio(const char *name, FILE *fp)
{
        printf("stream= %s ", name);

        if(fp->_flags & _IONBF)
                printf("unbuffered");
        else if(fp->_flags & _IOLBF)
                printf("line buffered");
        else
                printf("fully buffered");

        printf(", buffer size = %d\n", (fp->_IO_buf_end - fp->_IO_buf_base));
        
}
