#include <semaphore.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>

#include "bench_micro_shared.h"

void bench_micro_remote_dispatch(int *call_buffer, void *arglock, void *retlock);

int main(int argc, char **argv)
{
 /* Initialize the semaphores */
 sem_t *argsem = sem_open(BENCH_MICRO_SEMAPHORE_ARG_NAME, O_RDONLY);
 if (argsem == NULL) {
 perror("sem_open");
 return EXIT_FAILURE;
 }
 sem_t *retsem = sem_open(BENCH_MICRO_SEMAPHORE_RET_NAME, O_WRONLY);
 if (retsem == NULL) {
 perror("sem_open");
 return EXIT_FAILURE;
 }

 /* initialize the shared memory */
 int shm_fd = shm_open(BENCH_MICRO_SHMEM_NAME, O_RDWR, 0666);
 void *bench_micro_buffer = mmap(NULL, bench_micro_remote_frame_size, PROT_READ|PROT_WRITE,
 MAP_SHARED, shm_fd, 0);
 if (bench_micro_buffer == NULL) {
 perror("mmap");
 return EXIT_FAILURE;
 }

 bench_micro_remote_dispatch(bench_micro_buffer, argsem, retsem);
 return 0;
}
