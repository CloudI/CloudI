#ifndef OS_SPAWN_H
#define OS_SPAWN_H

#include <stdint.h>
int32_t spawn(char protocol, uint32_t * ports, uint32_t ports_len,
              char * filename, uint32_t filename_len,
              char * argv, uint32_t argv_len,
              char * env, uint32_t env_len);

#endif // OS_SPAWN_H
