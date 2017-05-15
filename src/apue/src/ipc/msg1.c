#include <unistd.h>
#include <sys/msg.h>
#include <stdio.h>
#include "apue.h"

//消息结构
struct message{
        long int mtype;
        char mtext[512];
};

int main(void)
{
        //创建消息队列
        int msgid = msgget(IPC_PRIVATE,0666);
        //创建消息结构
        struct message snd;
        //消息类型
        snd.mtype = 911;
        //消息内容
        strcpy(snd.mtext,"help");
        //发送消息到队列
        if(msgsnd(msgid,&snd,5,0) == -1){
                printf("msgsnd %m\n");
                return -1;
        }
        
        //读取消息队列信息
        struct msqid_ds ds;
        if(msgctl(msgid, IPC_STAT, &ds) == -1){
                printf("msgctl IPC_STAT %m\n");
                return -1;
        }
        printf("current bytes:%d ,current number:%d ,max bytes:%d \n",
               ds.msg_cbytes, ds.msg_qnum, ds.msg_qbytes);

        struct message rcv;
        //非阻塞读取消息类型为910的消息，出错返回
        if(msgrcv(msgid, &rcv, 512, 910, IPC_NOWAIT) == -1){
                printf("msgrcv1 %m\n");
        }
        //阻塞读取消息类型为911的消息
        if(msgrcv(msgid, &rcv, 521, 911, 0) == -1){
                printf("msgrcv2 %m\n");
                return -1;
        }
        //打印读取的消息
        printf("%s\n",rcv.mtext);
        //清除消息队列
        if(msgctl(msgid,IPC_RMID,NULL)==-1){
                printf("msgctl IPC_RMID %m\n");
        }
        return 0;
}
