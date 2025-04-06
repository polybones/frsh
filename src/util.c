#include <unistd.h>
#include <limits.h>
#include <signal.h>

typedef void (*sighandler_t)(int);

int getcwd_helper(char *buf, size_t size) {
    return getcwd(buf, size) != buf;
}

int get_path_max() {
    return PATH_MAX;
}

void f_sig_handler(int signo, sighandler_t handler) {
  signal(signo, handler);
}
