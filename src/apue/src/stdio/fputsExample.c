#include "apue.h"

int main(void)
{
        char buf[MAXLINE];

        while(NULL != fgets(buf, MAXLINE, stdin))
                if(EOF == fputs(buf, stdout))
                        err_sys("output error");

        if(ferror(stdin))
                err_sys("input error");

        exit(0);
        
}
