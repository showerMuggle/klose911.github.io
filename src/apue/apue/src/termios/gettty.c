#include <termios.h>
#include "apue.h"

int main(void)
{
        struct termios term;
        int size;

        if(tcgetattr(STDIN_FILENO, &term) < 0)
                err_sys("tcgetattr error");

        size = term.c_cflag & CSIZE;
        if(size == CS5)
                printf(" 5 bits/byte \n");
        else if(size == CS6)
                printf(" 6 bits/byte \n");
        else if(size == CS7)
                printf(" 7 bits/byte \n");
        else if(size == CS8)
                printf(" 8bits/byte \n");
        else
                printf(" unknown bits/byte \n");

        term.c_cflag &= ~CSIZE;
        term.c_cflag |= CS8;

        if(tcsetattr(STDIN_FILENO, TCSANOW, &term) < 0)
                err_sys("tcsetattr error");

        exit(0);
        
        
}
