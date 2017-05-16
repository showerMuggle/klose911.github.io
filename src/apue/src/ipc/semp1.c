#include <unistd.h>
#include <sys/sem.h>
#include <sys/wait.h>
#include "apue.h"

int main(void){

        //创建信号量集
        int semid = semget(IPC_PRIVATE, 1, 0666);
        int value = 0;

        //初始化信号量，设置信号量集的第一个信号量的当前值为0
        semctl(semid, 0, SETVAL, &value);

        pid_t pid=fork();
        if(pid==0){//child
                struct sembuf buf;
                buf.sem_num = 0;
                buf.sem_op = -1;
                printf("child wait to exit\n");
                //从信号量集的第一个信号量获取一个资源
                semop(semid, &buf, 1);
                printf("child about to exit\n");
                return 0;
        }
        
        sleep(2);
        struct sembuf buf;
        buf.sem_num = 0;
        buf.sem_op = 1;
        printf("tell child ready\n");
        //向信号量集的第一个信号量增加一个资源
        semop(semid,&buf,1);
        wait(NULL);

        //清理信号量集
        semctl(semid, 0, IPC_RMID);
        return 0;
}
