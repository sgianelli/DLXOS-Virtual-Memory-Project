//
//	synch.h
//
//	Include file for synchronization stuff.  The synchronization
//	primitives include:
//	Semaphore
//
//	Semaphores are the only "native" synchronization primitive.

#ifndef	_synch_h_
#define	_synch_h_

#include "queue.h"

#define MAX_SEMS	32	//Maximum 32 semaphores allowed in the system

typedef int sem_t;

#define INVALID_SEM -1
#define INVALID_PROC -1
typedef struct Sem {
    Queue	waiting;
    int		count;
    uint32	inuse; 		//indicates whether the semaphore is being
    				//used by any process
} Sem;

extern void	SemInit (Sem *, int);
extern void	SemWait (Sem *);
extern void	SemSignal (Sem *);

#endif	//_synch_h_
