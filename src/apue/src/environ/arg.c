#include <stdio.h>

int main(int argc, char *argv[])
{
        int i;
        //echo all command line args
        for(i = 0; argv[i] != NULL; i++)
                printf("argv[%d]: %s\n", i, argv[i]);

        return 0;
        
}
