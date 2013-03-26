
#ifndef _LAB4_H_
#define _LAB4_H_

#define NULL (void *)0x0
#define INVALID_SEM	-1

typedef unsigned int uint32;
typedef int sem_t;

//Related to processes
uint32 getpid();			//trap 0x431
void process_create(char *arg1, ...);  	//trap 0x432


//Related to semaphores
sem_t sem_create(int count);		//trap 0x450
int sem_wait(sem_t sem);		//trap 0x451
int sem_signal(sem_t sem);		//trap 0x452

#endif _LAB4_H_
