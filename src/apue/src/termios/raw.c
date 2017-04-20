#include <termios.h>
#include <signal.h> 
#include <unistd.h>
#include "apue.h"

static struct termios save_termios;
static int ttysavefd = -1;
static enum
{
        RESET, RAW, CBREAK
} ttystate = RESET;

//put tty into cbreak mode
int tty_cbreak(int fd)
{
        struct termios buf;

        if(tcgetattr(fd, &save_termios) < 0)
                return -1;

        buf = save_termios;

        buf.c_lflag &= ~(ECHO | ICANON); //echo off; canonical mode off
        // 1 byte at a time, no timer
        buf.c_cc[VMIN] = 1;
        buf.c_cc[VTIME] = 0;

        if(tcsetattr(fd, TCSAFLUSH, &buf) < 0)
                return -1;

        ttystate = CBREAK;
        ttysavefd = fd;

        return 0;
}

//put tty into raw  mode
int tty_raw(int fd)
{
        struct termios buf;

        if(tcgetattr(fd, &save_termios) < 0)
                return -1;

        buf = save_termios;

        buf.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG); //echo off; canonical mode off
                                                         //extended processing off; signal chars off;

        buf.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
        //no SIGINT on BRKINT; CR-to-NL off; input parity check off; don't strip 8th bit on input; output flow controll off

        buf.c_cflag &= ~(CSIZE | PARENB); // clear size bits; parity checking off
        buf.c_cflag |= CS8; //set 8 bits/char

        buf.c_oflag &= ~(OPOST); //output processing off 
        
                // 1 byte at a time, no timer
                buf.c_cc[VMIN] = 1;
        buf.c_cc[VTIME] = 0;

        if(tcsetattr(fd, TCSAFLUSH, &buf) < 0)
                return -1;

        ttystate = RAW;
        ttysavefd = fd;

        return 0;
}

//restore terminal mode
int tty_reset(int fd)
{
        if(ttystate != CBREAK && ttystate != RAW)
                return 0;

        if(tcsetattr(fd, TCSAFLUSH, &save_termios) < 0)
                return -1;

        ttystate = RESET;
        return 0;
}

void tty_atexit(void)
{
        if(ttysavefd >= 0)
                tty_reset(ttysavefd);
}

struct termios *tty_termios(void)
{
        return &save_termios;
}

static void sig_catch(int signo)
{
        printf("signal caught\n");
        tty_reset(STDIN_FILENO);
        exit(0);
}

int main(void)
{
        int i;
        char c;
        //catch signals
        if (SIG_ERR == signal(SIGINT, sig_catch) )
                err_sys("signal(SIGINT) error");
        if (SIG_ERR == signal(SIGQUIT, sig_catch) )
                err_sys("signal(SIGQUIT) error");
        if (SIG_ERR == signal(SIGTERM, sig_catch) )
                err_sys("signal(SIGTERM) error");

        if(tty_raw(STDIN_FILENO) < 0)
                err_sys("tty_raw error");

        printf("Enter raw mode charcters, terminate with DELETE\n");
        while( 1 == (i = read(STDIN_FILENO, &c, 1)) ) {
                //0177 = ASCII DELETE
                if( 0177 == (c &= 255) )
                        break;

                printf("%o\n", c);
        }

        if(tty_reset(STDIN_FILENO) < 0)
                err_sys("tty_reset error");
        if(i <= 0)
                err_sys("read error");

        if(tty_cbreak(STDIN_FILENO) < 0)
                err_sys("tty_cbreak error");

        printf("\nEnter cbreak mode charcters, terminate with SIGINT\n");
        
        while( 1 == (i = read(STDIN_FILENO, &c, 1)) ) {
                c &= 255 ;
                printf("%o\n", c);
        }

        tty_reset(STDIN_FILENO);
        if(i <= 0)
                err_sys("read error");

        exit(0);
}
