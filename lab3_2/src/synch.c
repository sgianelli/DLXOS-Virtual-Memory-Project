//
//	synch.c
//
//	Routines for synchronization
//
//

#include "dlxos.h"
#include "process.h"
#include "synch.h"

static Sem sems[MAX_SEMS]; 	//All semaphores in the system

extern struct PCB *currentPCB; 
//----------------------------------------------------------------------
//	SynchModuleInit
//
//	Initializes the synchronization primitives: the semaphores
//----------------------------------------------------------------------
void 
SynchModuleInit()
{
    int           i;
    dbprintf ('p', "Entering SynchModuleInit\n");
    for(i=0; i<MAX_SEMS; i++)
      sems[i].inuse = 0;
    dbprintf ('p', "Leaving SynchModuleInit\n");
}
//---------------------------------------------------------------------
//
//	SemInit
//
//	Initialize a semaphore to a particular value.  This just means
//	initting the process queue and setting the counter.
//
//----------------------------------------------------------------------
void
SemInit (Sem *sem, int count)
{
    QueueInit (&sem->waiting);
    sem->count = count;
}

//----------------------------------------------------------------------
// 	SemCreate
//
//	Grabs a Semaphore, initializes it and returns a handle to this
//	semaphore. All subsequent accesses to this semaphore should be made
//	through this handle
//----------------------------------------------------------------------
sem_t 
SemCreate(int count)
{
    sem_t sem;
    uint32 intrval;

    // grabbing a semaphore should be an atomic operation
    intrval = DisableIntrs();
    for(sem=0; sem<MAX_SEMS; sem++)
    {
      if(sems[sem].inuse==0)
      {
        sems[sem].inuse = 1;
	break;
      }
    }
    RestoreIntrs(intrval);
    
    if(sem==MAX_SEMS)
      return INVALID_SEM;

    SemInit(&sems[sem], count);

    return sem;
}


//----------------------------------------------------------------------
//
//	SemWait
//
//	Wait on a semaphore.  As described in Section 6.4 of _OSC_,
//	we decrement the counter and suspend the process if the
//	semaphore's value is less than 0.  To ensure atomicity,
//	interrupts are disabled for the entire operation.  Note that,
//	if the process is put to sleep, interrupts will be OFF when
//	it returns from sleep.  Thus, we enable interrupts at the end of
//	the routine.
//
//----------------------------------------------------------------------
void
SemWait (Sem *sem)
{
    Link	*l;
    int		intrval;

    intrval = DisableIntrs ();
    dbprintf ('I', "Old interrupt value was 0x%x.\n", intrval);
    dbprintf ('s', "Proc 0x%x waiting on sem 0x%x, count=%d.\n", currentPCB,
	      sem, sem->count);
    sem->count -= 1;
    if (sem->count < 0) {
	l = QueueAllocLink ();
	QueueLinkInit (l, (void *)currentPCB);
	dbprintf ('s', "Suspending current proc (0x%x).\n", currentPCB);
	QueueInsertLast (&sem->waiting, l);
	ProcessSleep ();
    }
    RestoreIntrs (intrval);
}
int SemHandleWait(sem_t sem)
{
  if(sem>=0&&sem<MAX_SEMS)
  {
    if(sems[sem].inuse)
    {
      SemWait(&sems[sem]);
      return 0;
    }
    return 1;
  }
  else
  {
    return 1;
  }
}

//----------------------------------------------------------------------
//
//	SemSignal
//
//	Signal on a semaphore.  Again, details are in Section 6.4 of
//	_OSC_.
//
//----------------------------------------------------------------------
void
SemSignal (Sem *sem)
{
    Link	*l;
    int		intrs;

    intrs = DisableIntrs ();
    dbprintf ('s', "Signalling on sem 0x%x, count=%d.\n", sem, sem->count);
    sem->count += 1;
    if (sem->count <= 0) {
	l = QueueFirst (&sem->waiting);
	QueueRemove (l);
	dbprintf ('s', "Waking up PCB 0x%x.\n", l->object);
	ProcessWakeup ((PCB *)(l->object));
	QueueFreeLink (l);
    }
    RestoreIntrs (intrs);
}
int SemHandleSignal(sem_t sem)
{
  if(sem>=0&&sem<MAX_SEMS)
  {
    if(sems[sem].inuse)
    {
      SemSignal(&sems[sem]);
      return 0;
    }
    return 1;
  }
  else
  {
    return 1;
  }
}

