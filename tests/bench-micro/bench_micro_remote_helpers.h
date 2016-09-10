#ifndef BENCH_MICRO_REMOTE_HELPERS_H
#define BENCH_MICRO_REMOTE_HELPERS_H

#ifndef _BSD_SOURCE
#define _BSD_SOURCE
#endif

#include <semaphore.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

#include <stdio.h>

/* warning: gnuish braced group */
#define cstubs_initialize_shared_memory(mem) \
 ((void)({ \
 int fd_ ## __LINE__ = shm_open(BENCH_MICRO_SHMEM_NAME, O_CREAT|O_RDWR, 0666); \
 ftruncate(fd_ ## __LINE__, bench_micro_remote_frame_size); \
 mem = mmap(NULL, bench_micro_remote_frame_size, PROT_READ|PROT_WRITE, \
 MAP_SHARED, fd_##__LINE__, 0); \
 }))

#define cstubs_start_remote_process() \
 ((fork() == 0 \
 ? (void)execl("bench_micro_remote_server", "bench_micro_remote_server", NULL) \
 : (void)0))

#define cstubs_initialize_arg_lock(lock) \
 ((void)(lock = sem_open(BENCH_MICRO_SEMAPHORE_ARG_NAME, O_CREAT|O_WRONLY, 0666, 0)))
#define cstubs_initialize_ret_lock(lock) \
 ((void)(lock = sem_open(BENCH_MICRO_SEMAPHORE_RET_NAME, O_CREAT|O_RDONLY, 0666, 0)))
#define cstubs_acquire_lock(x) \
 (sem_wait(x))
#define cstubs_release_lock(x) \
 (sem_post(x))

extern void *bench_micro_remote_global_buffer;
extern void *bench_micro_remote_global_arg_lock;
extern void *bench_micro_remote_global_ret_lock;
#define CSTUBS_ARG_LOCK bench_micro_remote_global_arg_lock
#define CSTUBS_RET_LOCK bench_micro_remote_global_ret_lock
#define CSTUBS_BUFFER bench_micro_remote_global_buffer

#endif /* BENCH_MICRO_REMOTE_HELPERS_H */
