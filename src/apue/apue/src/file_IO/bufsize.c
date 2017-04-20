#include <fcntl.h>
#include "apue.h"

#define BUFFZIZE 8192
extern void set_fl(int fd, int flags);

int main(void)
{
	set_fl(STDOUT_FILENO, O_SYNC);
	int n; 
	char buf[BUFFZIZE]; 

	while( (n = read(STDIN_FILENO, buf, BUFFZIZE)) > 0)
		if(write(STDOUT_FILENO, buf, n) != n)
			err_sys("write error");
		
	if(n < 0) 
		err_sys("read error");
	exit(0);

}
