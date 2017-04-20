#include <sys/types.h>
#include <pwd.h>
#include <stddef.h>
#include <string.h>

static struct passwd *retrievePasswd(const char *name);

int main(void) 
{
        char name[] = "klose";
        struct passwd *ptr;
        ptr = retrievePasswd(name);

        if(NULL == ptr)
                err_sys("error retrieve passwd entry");

        printf("work directory %s\n", ptr->pw_dir);
        printf("login shell  %s\n", ptr->pw_shell);

        exit(0);
        
}

struct passwd *retrievePasswd(const char *name)
{
        struct passwd *ptr;
        setpwent();

        while((ptr = getpwent()) != NULL)
                if(0 == strcmp(name, ptr->pw_name) )
                        break;

        endpwent();
        return ptr;
        
}
