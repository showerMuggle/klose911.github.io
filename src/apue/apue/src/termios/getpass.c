#include <signal.h>
#include <termios.h>
#include <stdio.h>
#include "apue.h"

#define MAX_PASS_LEN 8

char *getpass(const char *prompt);

int main(void)
{
        char *ptr;

        if(NULL == (ptr = getpass("Enter password:")) )
                err_sys("getpass error");

        printf("password: %s\n", ptr);

        while(*ptr != 0)
                *ptr++ = 0;

        exit(0);
        
}

char *getpass(const char *prompt)
{
        static char buf[MAX_PASS_LEN + 1];
        char *ptr;
        sigset_t sig, sigsave;
        struct termios term, termsave;
        FILE *fp;
        int c;

        if(NULL == ( fp = fopen(ctermid(NULL), "r+") ) )
                return NULL;

        setbuf(fp, NULL); //set standard input/output no buffer 

        sigemptyset(&sig); //block SIGINT & SIGTSTP, save signal mask
        sigaddset(&sig, SIGINT);
        sigaddset(&sig, SIGTSTP);
        sigprocmask(SIG_BLOCK, &sig, &sigsave);

        tcgetattr(fileno(fp), &termsave); //save tty state
        term = termsave; //struct copy
        term.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL); //echo off
        tcsetattr(fileno(fp), TCSAFLUSH, &term);

        fputs(prompt, fp);

        ptr = buf;
        while( (c = getc(fp) ) != EOF && c != '\n' ) {
                if(ptr < &buf[MAX_PASS_LEN])
                        *ptr++ = c;
        }
        
        *ptr = 0; //null terminate

        putc('\n', fp); // we echo a new line

        tcsetattr(fileno(fp), TCSAFLUSH, &termsave); //restore tty state

        sigprocmask(SIG_SETMASK, &sigsave, NULL); //restore signal mask 

        fclose(fp); // done with /dev/tty

        return buf;
}
