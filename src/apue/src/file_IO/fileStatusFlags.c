#include <sys/types.h>
#include <fcntl.h>
#include "apue.h"

int main(int argc, char *argv[]) 
{
	int val;

	if(argc != 2) 
		err_quit("usage: fileStatusFlag <descriptor#>");
	if( (val = fcntl(atoi(argv[1]), F_GETFL, 0)) < 0) 
		err_sys("fcntl error for %d", atoi(argv[1])); 

	int accmode = val & O_ACCMODE; 
	switch(accmode) {
		case O_RDONLY: 
			printf("read only");
			break; 
		case O_WRONLY: 
			printf("write only");
			break; 
		case O_RDWR: 
			printf("read write"); 
			break; 
		default: 
			err_dump("unknown access mode"); 
	}

	if(val & O_APPEND) 
		printf(", append");
	if(val & O_NONBLOCK) 
		printf(", nonblocking"); 
#if !defined(_POSIX_SOURCE) && defined(O_SYNC) 
	if(val & O_SYNC) 
		printf(", synchronous writes"); 
#endif
	//putchar("\n");
	printf("\n");
	exit(0);
}
