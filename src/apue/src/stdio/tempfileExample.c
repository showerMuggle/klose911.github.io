#include "apue.h"

int main(void)
{
        char name[L_tmpnam], line[MAXLINE];
        FILE *fp;

        printf("%s\n", tmpnam(NULL) );

        tmpnam(name);
        printf("%s\n", name);

        if(NULL == (fp = tmpfile() ) )
                err_sys("tempfile error");

        fputs("Hello World\n", fp);
        rewind(fp);
        if(NULL == (fgets(line, sizeof(line), fp) ) )
                err_sys("fgets error");
        fputs(line, stdout);

        exit(0);
        
}
