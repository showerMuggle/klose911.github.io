#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([FULL-PACKAGE-NAME], [VERSION], [BUG-REPORT-ADDRESS])
AM_INIT_AUTOMAKE(apue,1.0, klose911@gmail.com)
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC

# Checks for libraries.
AC_PROG_RANLIB

# Checks for header files.
AC_HEADER_MAJOR
AC_CHECK_HEADERS([arpa/inet.h fcntl.h inttypes.h limits.h netdb.h netinet/in.h shadow.h stddef.h stdlib.h string.h strings.h sys/ioctl.h sys/socket.h sys/time.h syslog.h termios.h unistd.h utime.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_CHECK_MEMBERS([struct stat.st_rdev])
AC_CHECK_HEADER_STDBOOL
AC_TYPE_UID_T
AC_TYPE_UINT32_T

# Checks for library functions.
AC_FUNC_CHOWN
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_MMAP
AC_FUNC_REALLOC
AC_CHECK_FUNCS([alarm atexit dup2 endpwent ftruncate getcwd gethostname getpass getspnam gettimeofday memset mkdir mkfifo pathconf select socket strchr strdup strerror strncasecmp strrchr utime])

AC_CONFIG_FILES([Makefile
                 src/charpter1/Makefile
                 src/datafiles/Makefile
                 src/environ/Makefile
                 src/file_IO/Makefile
                 src/file_directory/Makefile
                 src/lib/Makefile
                 src/process/Makefile
                 src/procgroup/Makefile
                 src/signal/Makefile
                 src/stdio/Makefile
                 src/termios/Makefile
                 src/aio/Makefile
                 src/ipc/Makefile
				 ])
AC_OUTPUT
