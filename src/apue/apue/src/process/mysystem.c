#include	<sys/wait.h>
#include	<errno.h>
#include	<unistd.h>

/* 缺少信号处理 */
int mysystem(const char *cmdstring)	
{
        pid_t	pid;
        int		status;
        
        if (cmdstring == NULL)
                return(1); //返回1表示支持system函数	

        if ((pid = fork()) < 0) {
                status = -1;	//无法再创建新的进程
        } else if (pid == 0) { //子进程
                execl("/bin/sh", "sh", "-c", cmdstring, (char *)0);
                _exit(127);		//无法执行exec函数，返回127
        } else { //父进程等待子进程结束
                while (waitpid(pid, &status, 0) < 0) {
                        if (errno != EINTR) {
                                status = -1; /* waitpid的错误不是EINTR，返回-1 */
                                break;
                        }
                }
        }

        return(status); //返回子进程的终止状态
}
