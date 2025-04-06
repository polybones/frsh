#include <unistd.h>
#include <limits.h>

int getcwd_helper(char *buf, size_t size) {
    return getcwd(buf, size) != buf;
}

int get_path_max() {
    return PATH_MAX;
}
