//
//  dlxsim.cc
//
//  DLX simulator code.  This is a barebones simulator, but it does
//  include code to handle OS issues such as virtual memory translation.
//  This allows it to be used as a simulator for operating systems
//  classes.
//
//  This file contains the routines that manage higher-level
//  functionality.  Instruction execution is in a separate file.
//

/* Copyright (c) 1999-2004 by Ethan L. Miller

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

static char rcsid[] =
	"$Id: dlxsim.cc,v 1.16 2004/10/01 18:43:40 elm Exp $";
char rcsDlxsimDate[] = "$Date: 2004/10/01 18:43:40 $";

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <signal.h>
#include <fcntl.h>
#include "dlx.h"
#include "dlx-inlines.h"

extern int errno;
char debug[100];
//----------------------------------------------------------------------
//
//  Cpu::Cpu
//
//  Create a new CPU.
//
//----------------------------------------------------------------------
Cpu::Cpu(int msize):debugger(NULL)
{
	int i;
	struct timeval t;

	flags = 0;
	usElapsed = 0;
	tickCounter = 0;
	instrsExecuted = 0;
	instPerUs = 1;
	for (i = 0; i < 32; i++) {
		sreg[i] = 0;
		ireg[i] = 0;
		freg[i] = 0;
	}
	SetStatusBit(DLX_STATUS_PAGE_TABLE);
	SetStatusBit(DLX_STATUS_SYSMODE);
	EnableInterrupts();
	timerInterrupt = DLX_TIMER_NOT_ACTIVE;
	memSize = msize;
	memory = new uint32[msize / sizeof(uint32)];
	basicBlockStart = 1;		// basic block can never start at address 1!
	// Initialize the keyboard I/O stuff.
	kbdbufferedchars = 0;
	kbdrpos = kbdwpos = 0;
	kbdcounter = 0;
	diskIntrTime = 1e60;
	SetupRawIo();
	gettimeofday(&t, (struct timezone *) 0);
	realElapsed = (double) t.tv_sec + ((double) t.tv_usec) * 1e-6;
}

//----------------------------------------------------------------------
//
//  TraceFile
//
//  Open the passed file name for tracing.  If the file is NULL,
//  open stdout.
//
//----------------------------------------------------------------------
int
 Cpu::TraceFile(char *name)
{
	if ((name == NULL) || (!strcmp(name, "-"))) {
		tracefp = stdout;
		return (1);
	} else if ((tracefp = fopen(name, "w")) == NULL) {
		return (0);
	} else {
		return (1);
	}
}


//----------------------------------------------------------------------
//
//  Cpu::CauseException
//
//  Cause an exception.  This loads the CAUSE register with the
//  exception cause, loads the IAR with the proper pointer, and
//  sets things up so that the exception will be taken after the
//  current instruction completes.
//
//----------------------------------------------------------------------
int Cpu::CauseException(int excType)
{
	uint32 ivec;

	DBPRINTF('t', "Exception being done (cause=0x%x @ pc=0x%x).\n",
			 excType, PC() - 4);
	ivec = GetSreg(DLX_SREG_INTRVEC);
	OutputBasicBlock(ivec);
	if (flags & (DLX_TRACE_INSTRUCTIONS | DLX_TRACE_MEMORY)) {
		fprintf(tracefp, "X %x %x\n", excType, PC() - 4);
	}
	PutSreg(DLX_SREG_CAUSE, excType);
	// PC has already been incremented, so decrement it first.  If this
	// is a trap or interrupt, the PC will have already been incremented
	// (if necessary) so the IAR points to the next instruction to
	// execute.
	PutSreg(DLX_SREG_IAR, PC() - 4);
	// Save the current status register
	PutSreg(DLX_SREG_ISR, GetSreg(DLX_SREG_STATUS));
	// Save the current value of register 31.  This is necessary to give
	// the interrupt handler a temporary register that can be used to
	// switch to a system stack (rather than user stack)
	PutSreg(DLX_SREG_IR31, GetIreg(31));
	// Set the next instruction to be run to be the interrupt vector.
	SetPC(ivec);
	// Set the status register to be system mode
	PutSreg(DLX_SREG_STATUS,
			GetSreg(DLX_SREG_STATUS) | DLX_STATUS_SYSMODE);
	// Turn off interrupts
	DisableInterrupts();
	return (1);
}


//----------------------------------------------------------------------
//
//  Cpu::VaddrToPaddr
//
//  Read a single word from memory.  This involves translating
//  the virtual address to a physical address, checking that the
//  access is allowed, and ensuring that the address itself is
//  valid (ie, aligned).  An exception is caused (and 0 returned)
//  if the access fails for any reason.
//
//----------------------------------------------------------------------
inline
	int
	Cpu::VaddrToPaddr(uint32 vaddr, uint32 & paddr, uint32 op,
					  uint32 pteflags)
{

	if ((vaddr & 0x3) != 0) {
		CauseException(DLX_EXC_ADDRESS);
		return (0);
	}
	// For system references, physical address is the same
	// as virtual address.  Also, if no translation bits are set, physical
	// address is set to virtual address.
	paddr = vaddr;
	if (UserMode()) {
		// For user mode addresses, translate using 
		if (StatusBit(DLX_STATUS_PAGE_TABLE)) {
			uint32 pt1base, pt2base, pt1pagebits, pt2pagebits;
			uint32 pteaddr;
			uint32 offsetinpage, entrynum;
			uint32 pagemask;

			DBPRINTF('m', "Translating 0x%x\n", vaddr);
			pt1base = GetSreg(DLX_SREG_PGTBL_BASE);
			pt1pagebits = GetSreg(DLX_SREG_PGTBL_BITS);
			pt2pagebits = (pt1pagebits >> 16) & 0xffff;
			pt1pagebits &= 0xffff;
			pagemask = (1 << pt2pagebits) - 1;
			offsetinpage = vaddr & pagemask;
			// Mask off the low bits
			vaddr &= ~pagemask;
			if ((entrynum = (vaddr >> pt1pagebits)) >=
				GetSreg(DLX_SREG_PGTBL_SIZE)) {
				DBPRINTF('m',
						 "Out of range (L1 = %db, L2 = %db size=%d entry=%d)\n",
						 pt1pagebits, pt2pagebits,
						 GetSreg(DLX_SREG_PGTBL_SIZE), entrynum);
				CauseException(DLX_EXC_ACCESS);
				return (0);
			}
			pteaddr = pt1base + 4 * entrynum;
			paddr = Memory(pteaddr);
			// If the L2 page size is the same as the L1 page size, there's
			// no L2 page table!
			if (pt1pagebits != pt2pagebits) {
				pt2base = paddr;
				if (pt2base == 0) {
					DBPRINTF('m',
							 "No L2 table at entry %d! (base = 0x%x)\n",
							 entrynum, pt1base);
					CauseException(DLX_EXC_PAGEFAULT);
					return (0);
				}
				pteaddr = pt2base + 4 * ((vaddr >> pt2pagebits) &
										 ((1 <<
										   (pt1pagebits - pt2pagebits)) -
										  1));
				paddr = Memory(pteaddr);
			}
			DBPRINTF('M', "Using PTE 0x%08x\n", paddr);
			if (!(paddr & DLX_PTE_VALID)) {
				DBPRINTF('m', "PTE invalid (0x%08x)\n", paddr);
				PutSreg(DLX_SREG_FAULT_ADDR, vaddr);
				CauseException(DLX_EXC_PAGEFAULT);
				return (0);
			}
			if (pteflags & (DLX_PTE_DIRTY | DLX_PTE_REFERENCED)) {
				SetMemory(pteaddr,
						  paddr | (pteflags &
								   (DLX_PTE_DIRTY | DLX_PTE_REFERENCED)));
			}
			paddr &= ~(pagemask | DLX_PTE_MASK);
			paddr |= offsetinpage;
			DBPRINTF('m',
					 "0x%x => 0x%x (=%08x) using base1=0x%x/%d, entry %d\n",
					 vaddr | offsetinpage, paddr,
					 Memory(paddr), pt1base, pt1pagebits, entrynum);
			// Fall through for address range check
		} else if (StatusBit(DLX_STATUS_TLB)) {
			// TLB is fully associative
			int tlbLine, gotAddr = 0;
			uint32 tlbVpage, tlbPpage, pageSize, pageMask, entryBits;

			DBPRINTF('m', "Translating addr 0x%x with TLB.\n", vaddr);
			for (tlbLine = 0; tlbLine < DLX_TLB_NENTRIES; tlbLine++) {
				GetTlb(tlbLine, tlbVpage, tlbPpage);
				pageSize = 1 << (tlbPpage & DLX_TLB_ENTRY_PAGESIZE_MASK);
				pageMask = ~(pageSize - 1);
				entryBits = tlbVpage & DLX_PTE_MASK;
				tlbVpage &= pageMask;
				if ((vaddr >= (tlbVpage & pageMask)) &&
					(vaddr < (tlbVpage + pageSize)) &&
					(entryBits & DLX_PTE_VALID)) {
					// Got a match!
					gotAddr = 1;
					break;
				}
			}
			if (!gotAddr) {
				DBPRINTF('m', "No TLB entry for vaddr 0x%x.\n", vaddr);
				// Cause a TLB exception if we didn't find the page
				PutSreg(DLX_SREG_FAULT_ADDR, vaddr);
				CauseException(DLX_EXC_TLBFAULT);
				return (0);
			}
			AccessTlb(tlbLine, DLX_PTE_REFERENCED |
					  ((op == DLX_MEM_WRITE) ? DLX_PTE_DIRTY : 0));
			paddr = (tlbPpage & pageMask) | (vaddr & ~pageMask);
			DBPRINTF('m',
					 "0x%x => 0x%x (=%08x) using tlb entry %d (v=%08x,p=%08x)\n",
					 vaddr, paddr, Memory(paddr), tlbLine, tlbVpage,
					 tlbPpage);
			// Fall through for address range check
		}
	}
	if ((paddr <= memSize) || ((paddr >= DLX_IO_BASE) &&
							   (paddr <= (DLX_IO_BASE + DLX_IO_SIZE)))) {
		return (1);
	} else {
		DBPRINTF('t', "Illegal system address: 0x%x.\n", vaddr);
		CauseException(DLX_EXC_ACCESS);
		return (0);
	}
}

//----------------------------------------------------------------------
//
//  Cpu::ReadWord
//
//  Read a word from memory.  This can either be a regular memory
//  address or an I/O address.
//
//----------------------------------------------------------------------
int
 Cpu::ReadWord(uint32 vaddr, uint32 & val, uint32 op)
{
	uint32 paddr;
	int i;
	int done = 0;

	DBPRINTF('l', "Trying to read virtual address: 0x%x.\n", vaddr);
	if (!VaddrToPaddr(vaddr, paddr, op, DLX_PTE_REFERENCED)) {
		return (0);
	}
	if (paddr <= memSize) {
		val = Memory(paddr);
	} else if ((paddr >= DLX_DISK_ADDR_MIN) &&
			   (paddr <= DLX_DISK_ADDR_MAX)) {
		for (i = 0; i < DLX_DISK_MAX_DISKS; i++) {
			if (paddr == DLX_DISK_STATUS + (i * 0x10)) {
				if (disk[i] != NULL) {
					val = disk[i]->GetStatus();
				} else {
					val = 0;
				}
				done = 1;
				break;
			}
		}
		if (!done) {
			DBPRINTF('x', "Illegal disk address: 0x%x.\n", paddr);
			CauseException(DLX_EXC_ACCESS);
		}
	} else {
		DBPRINTF('l', "Trying to load special address: 0x%x.\n", paddr);
		switch (paddr) {
		case DLX_KBD_NCHARSIN:
			val = KbdNumInChars();
			break;
		case DLX_KBD_NCHARSOUT:
			val = KbdNumOutChars();
			break;
		case DLX_KBD_GETCHAR:
			val = KbdGetChar();
			break;
		case DLX_GETMEMSIZE:
			val = memSize;
			break;
		case DLX_TIMER_GETTIMER:
			val = GetTimer();
			break;
		case DLX_TIME_MICROSECONDS:
			val = (int) (usElapsed % 1000000L);
			break;
		case DLX_TIME_SECONDS:
			val = (int) (usElapsed / 1000000L);
			break;
		default:
			DBPRINTF('x', "Illegal address: 0x%x.\n", paddr);
			CauseException(DLX_EXC_ACCESS);
			break;
		}
	}
	return (1);
}

int Cpu::WriteWord(uint32 vaddr, uint32 val)
{
	uint32 paddr;
	int done = 0;
	int i;

	if (!VaddrToPaddr(vaddr, paddr, DLX_MEM_WRITE,
					  DLX_PTE_DIRTY | DLX_PTE_REFERENCED)) {
		return (0);
	}
	if (paddr <= memSize) {
		SetMemory(paddr, val);
	} else if ((paddr >= DLX_DISK_ADDR_MIN) &&
			   (paddr <= DLX_DISK_ADDR_MAX)) {
		for (i = 0; i < DLX_DISK_MAX_DISKS; i++) {
			if (paddr == DLX_DISK_REQ + (i * 0x10)) {
				if ((disk[i] != NULL)) {
					val = disk[i]->StartIo(val, this);
					if (disk[i]->FinishTime() < diskIntrTime) {
						diskIntrTime = disk[i]->FinishTime();
					}
				}
				done = 1;
				break;
			}
		}
		if (!done) {
			DBPRINTF('x', "Illegal disk address: 0x%x.\n", paddr);
			CauseException(DLX_EXC_ACCESS);
		}
	} else {
		switch (paddr) {
		case DLX_KBD_PUTCHAR:
			KbdPutChar(val);
			break;
		case DLX_KBD_INTR:
			if (val == 0) {
				flags &= ~DLX_FLAG_KBD_INTERRUPT;
			} else {
				flags |= DLX_FLAG_KBD_INTERRUPT;
			}
			break;
		case DLX_TIMER_SETTIMER:
			DBPRINTF('o', "Setting timer to %d us.\n", val);
			SetTimer(val);
			break;
#if 0
		case DLX_DISK_REQUEST:
			diskReq = val;
			StartDiskIo();
			break;
		case DLX_DISK_BLOCK:
			diskBlock = val;
			break;
		case DLX_DISK_ADDR:
			diskAddr = val;
			break;
#endif
		default:
			DBPRINTF('x', "Illegal address: 0x%x.\n", paddr);
			CauseException(DLX_EXC_ACCESS);
			break;
		}
	}
	return (1);
}

int Cpu::TestWriteWord(uint32 vaddr)
{
	uint32 paddr;
	if (!VaddrToPaddr(vaddr, paddr, DLX_MEM_WRITE)) {
		return (0);
	} else {
		return (1);
	}
}

//----------------------------------------------------------------------
//
//  Cpu::Open
//
//  Open a file in the "real" file system.  All parameters (in
//  particular, the file name) must be passed by physical address in
//  the simulator's address space.
//  accessType = 1 for read, 2 for write, 3 for both.
//
//  The trap is called as:
//  Open (name, accesstype)
//
//----------------------------------------------------------------------
void Cpu::Open()
{
	uint32 name;
	char nameBuf[100];
	char *tp;
	int accessType;
	int i;

	name = GetParam(0);
	accessType = GetParam(1);
	DBPRINTF('F', "Opening file %s (mode=%d).\n", (char *) memory + name,
			 accessType);
	switch (accessType) {
	case 1:
		tp = "r";
		break;
	case 2:
		tp = "w";
		break;
	case 3:
		tp = "r+";
		break;
	default:
		SetResult(0xffffffff);
		return;
		break;
	}
	for (i = 0; i < DLX_MAX_FILES; i++) {
		if (fp[i] == NULL) {
			if (!CheckAddr(name)) {
				SetResult(0xffffffff);
				return;
			}
			strncpy(nameBuf, (char *) memory + name, 98);
			// If fopen fails, it returns NULL, so it looks like no open
			// was done.
			fp[i] = fopen(nameBuf, tp);
			break;
		}
	}
	if (i >= DLX_MAX_FILES) {
		i = -1;
	} else if (fp[i] == NULL) {
		i = -errno;
	}
	DBPRINTF('F', "Open returns file descriptor %d\n", i);
	SetResult(i);
}

//----------------------------------------------------------------------
//
//  Cpu::Read
//  Cpu::Write
//
//  Read data from a previously opened file in the real world.
//  Called as:
//  Read (int desc, void* buf, int size)
//  Write (int desc, void* buf, int size)
//  Returns number of bytes read if there were more than 0, 0 on
//  end of file, and -1 otherwise.
//
//----------------------------------------------------------------------
void Cpu::Read()
{
	FileIo(DLX_FILE_READ);
}

void Cpu::Write()
{
	FileIo(DLX_FILE_WRITE);
}

void Cpu::FileIo(int kind)
{
	int fd;
	uint32 buf;
	int size;
	int n;

	fd = GetParam(0);
	buf = GetParam(1);
	size = GetParam(2);
	DBPRINTF('F', "FileIo (%s) on fd %d, size %d, buffer=0x%x\n",
			 (kind == DLX_FILE_WRITE) ? "write" : "read", fd, size, buf);
	if (!CheckAddr(buf) || (!CheckFd(fd))) {
		SetResult(0xffffffff);
		return;
	}
	if (kind == DLX_FILE_WRITE) {
		n = fwrite((unsigned char *) memory + buf, 1, size, fp[fd]);
	} else {
		n = fread((unsigned char *) memory + buf, 1, size, fp[fd]);
	}
	if (n > 0) {
		SetResult(n);
	} else if (feof(fp[fd])) {
		SetResult(0);
	} else {
		SetResult(-errno);
	}
}

//----------------------------------------------------------------------
//
//  Cpu::Seek
//
//  Seek to a position in the file.  Call it like:
//  Seek (int fd, int offset, int from)
//  Returns the new file position, or -1 if it fails.
//
//----------------------------------------------------------------------
void Cpu::Seek()
{
	int fd;
	int offset;
	int whence;
	int rv;

	fd = GetParam(0);
	offset = GetParam(1);
	whence = GetParam(2);
	if (!CheckFd(fd)) {
		SetResult(0xffffffff);
		return;
	}
	if ((rv = fseek(fp[fd], offset, whence)) < 0) {
		SetResult(rv);
	} else {
		SetResult(ftell(fp[fd]));
	}
}

//----------------------------------------------------------------------
//
//  Cpu::Close
//
//  Close a file and release its resources.
//
//----------------------------------------------------------------------
void Cpu::Close()
{
	int fd;
	uint32 retval;

	fd = GetParam(0);
	if (!CheckFd(fd)) {
		retval = 0xffffffff;
	} else {
		retval = fclose(fp[fd]);
	}
	DBPRINTF('F', "Closing file %d.\n", fd);
	fp[fd] = NULL;
	SetResult(retval);
}

//----------------------------------------------------------------------
//
//  Cpu::Random
//
//  Return a random number using the underlying random() function
//  call.
//
//----------------------------------------------------------------------
void Cpu::Random()
{
	int n;

	n = random();
	SetResult(n);
}

//----------------------------------------------------------------------
//
//  Cpu::Srandom
//
//  Set the random number generator to the number passed.
//
//----------------------------------------------------------------------
void Cpu::Srandom()
{
	uint32 seed;

	seed = GetParam(0);
	srandom(seed);
	SetResult(0);
}

//----------------------------------------------------------------------
//
//  Cpu::GetTime
//
//  Get the current (real world) time.
//
//----------------------------------------------------------------------
void Cpu::GetTime()
{
	SetResult(time((time_t *) 0));
}

//----------------------------------------------------------------------
//
//  Cpu::GetParam
//
//  Parameters are stored on the stack (r29), with the first parameter
//  stored at [r29].  Other parameters are stored at [r29+4], [r29+8],
//  etc.  Parameters are passed on word boundaries.
//
//----------------------------------------------------------------------
uint32 Cpu::GetParam(int p)
{
	uint32 stackPtr;

	stackPtr = GetIreg(29);
	return (Memory(stackPtr + (p << 2)));
}

//----------------------------------------------------------------------
//
//  Cpu::SetResult
//
//  Results are stored in register 1.
//
//----------------------------------------------------------------------
void Cpu::SetResult(uint32 r)
{
	PutIreg(1, r);
}

//----------------------------------------------------------------------
//
//  Cpu::Printf
//
//  This is the actual printf routine because we have to handle strings
//  differently because of the pointers involved.  Everything else works
//  as would be expected.
//
//  IMPORTANT: floating point numbers aren't supported.
//
//  IMPORTANT: all pointers must be passed as addresses in physical
//  simulated memory, not as virtual addresses.  This basically means
//  that you don't want to call printf from user level because the
//  format string itself is in user space.  Of course, you can always
//  copy it into kernel space, but it's harder to copy the other string
//  arguments.
//
//----------------------------------------------------------------------
void Cpu::Printf()
{
	uint32 fmtaddr;
	char *c;
	uint32 args[10];
	int nargs = 0;

	fmtaddr = GetParam(0);
	// 
	for (c = fmtaddr + (char *) memory; *c != '\0'; c++) {
		if (*c == '%') {
			// if this is a %%, skip past second %
			if (*(c + 1) == '%') {
				c++;
				continue;
			}
			// Get the current argument off the stack
			args[nargs] = GetParam(nargs + 1);
			DBPRINTF('p', "Argument %d at 0x%x is %d (0x%x).\n", nargs,
					 args[nargs], args[nargs]);
			while (1) {
				c++;
				if (*c == 's') {
					// If it's a string, the address is relative to the
					// start of emulated memory.
					args[nargs] += (uint32) memory;
					break;
				} else if (*c == 'l') {
					continue;
				} else if ((*c == 'f') || (*c == 'g') || (*c == 'e')) {
					// If it's a floating point number, it'll be passed as
					// a double, so grab the second word also.
					nargs += 1;
					args[nargs] = GetParam(nargs + 1);
					break;
				} else if ((*c >= 'a') && (*c <= 'z')) {
					// If it's another formatting character, it's not
					// a string, but we can leave the loop anyway.
					break;
				}
			}
			nargs += 1;
		}
	}
	printf(fmtaddr + (char *) memory,
		   args[0], args[1], args[2], args[3],
		   args[4], args[5], args[6], args[7]);
	fflush(stdout);
}

//----------------------------------------------------------------------
//
//  Cpu::Exit
//
//  Exit the CPU altogether.  Before doing so, print statistics on
//  instructions executed and time taken.
//
//----------------------------------------------------------------------
void Cpu::Exit()
{
	struct timeval t;

	printf("Exiting at program request.\n");
	printf("Instructions executed: %.0lf\n", (double) instrsExecuted);
	printf("Time simulated: %.03lf secs\n", (double) usElapsed / 1e6);
	gettimeofday(&t, (struct timezone *) 0);
	realElapsed =
		((double) t.tv_sec + ((double) t.tv_usec) * 1e-6) - realElapsed;
	printf("Real time elapsed: %.03lf secs\n", realElapsed);
	printf
		("Execution rate: %.2lfM simulated instructions per real second.\n",
		 (double) instrsExecuted * 1e-6 / realElapsed);
	ClearRawIo();
	exit(0);
}

//----------------------------------------------------------------------
//
//  Cpu::ExecOne
//
//  Execute a single CPU instruction in the simulator.
//
//----------------------------------------------------------------------
int Cpu::ExecOne()
{
	int i;
	uint32 curInst;
	uint32 tmpPc;
	uint32 curOp;
	uint32 retval;
	uint32 funcCode;			// subcode for RRR & FP ops
	static int kbdIntrPending = 0;

	if (++tickCounter >= instPerUs) {
		tickCounter = 0;
		usElapsed += 1;
		instrsExecuted += instPerUs;
	}
	// Increment PC before checking for interrupts because CauseException
	// will subtract 4 off the PC before placing the value into the IAR.
	// By incrementing here, we ensure that the current instruction is
	// the one whose address goes into the IAR.
	SetPC(PC() + 4);
	// Check for an input character.  If we got one, remember it so that
	// we can interrupt next time interrupts are enabled.
	if (kbdcounter++ > DLX_KBD_FREQUENCY) {
		kbdcounter = 0;
		kbdIntrPending |= GetCharIfAvail();
		stopnext = (debugger && debugger->HasData());
	}

	if (IntrLevel() < 8) {
		// Check for previous keyboard interrupt
		if (kbdIntrPending) {
			DBPRINTF('t', "Keyboard interrupt at PC=0x%x, t=%.0fus\n",
					 PC() - 4, usElapsed);
			kbdIntrPending = 0;	// Reset to no pending interrupt
			CauseException(DLX_EXC_KBD);
			return (0);
		} else if (diskIntrTime < usElapsed) {
			int nextDisk = -1;
			DBPRINTF('t', "Disk interrupt at PC=0x%x, t=%.0fus\n",
					 PC() - 4, usElapsed);
			CauseException(DLX_EXC_DISK);
			for (i = 0; i < DLX_DISK_MAX_DISKS; i++) {
				if ((disk[i] != NULL)
					&& (disk[i]->FinishTime() < usElapsed)) {
					disk[i]->FinishIo(this);
				}
			}
			diskIntrTime = 1e60;
			for (i = 0; i < DLX_DISK_MAX_DISKS; i++) {
				if ((disk[i] != NULL)
					&& (disk[i]->FinishTime() < diskIntrTime)) {
					diskIntrTime = disk[i]->FinishTime();
					nextDisk = i;
				}
			}
			DBPRINTF('K', "Disk interrupt time (disk %d) is %g.\n",
					 nextDisk, diskIntrTime);
			return (0);
		} else if (timerInterrupt < usElapsed) {
			DBPRINTF('t',
					 "Timer interrupt at PC=0x%x, t=%.0fus, intr@%.0fus\n",
					 PC() - 4, (double) usElapsed,
					 (double) timerInterrupt);
			timerInterrupt = DLX_TIMER_NOT_ACTIVE;
			CauseException(DLX_EXC_TIMER);
			return (0);
		}
	}

	if (!ReadWord(PC() - 4, curInst, DLX_MEM_INSTR)) {
		DBPRINTF('I', "Instruction fetch at 0x%x failed!\n", PC() - 4);
		return (0);
	}
	curOp = (curInst >> DLX_OPCODE_SHIFT) & DLX_OPCODE_MASK;
	DBPRINTF('I', "Instr %06d: %08x : %08x (main=%02x, aux=%02x)\n",
			 (int) instrsExecuted % 1000000,
			 curInst, PC() - 4, curOp,
			 (curInst >> DLX_ALU_FUNC_CODE_SHIFT) &
			 DLX_ALU_FUNC_CODE_MASK);
	switch (curOp) {
	case 0x00:					// ALU and other R-R operations
		funcCode = ((curInst >> DLX_ALU_FUNC_CODE_SHIFT) &
					DLX_ALU_FUNC_CODE_MASK);
		retval = (rrrInstrs[funcCode].handler) (curInst, this);
		break;
	case 0x01:					// FP operations
		funcCode = ((curInst >> DLX_FPU_FUNC_CODE_SHIFT) &
					DLX_FPU_FUNC_CODE_MASK);
		retval = (fpInstrs[funcCode].handler) (curInst, this);
		break;
	default:
		retval = (regInstrs[curOp].handler) (curInst, this);
		break;
	}
	if (stopnext) {
		stopnext = false;
		DebugControl();
	}
	return (retval);
}

//----------------------------------------------------------------------
//
//  Cpu::LoadMemory
//
//  Load a file into memory.  The file format is elf.
//  basically hands just hands control to the elf stuff
//
//----------------------------------------------------------------------


extern "C" {
#include "elf.h"
	int Cpu::LoadMemory(const char *file, uint32 & startAt) {
		Elf32_EHdr hdr;
		FILE *fp;

		if ((fp = fopen(file, "r")) == NULL) {
			fprintf(stderr, "Couldn't open file\n");
			return (0);
		}

		if (-1 == read_elfheader(fp, &hdr)) {
			fclose(fp);
			fprintf(stderr, "Failed to readelf header\n\n\n");
			return (0);
		}
		startAt = ntohl(hdr.e_entry);

		if (-1 ==
			load_program(fp, ((unsigned char *) ((void *) memory)),
						 &hdr)) {
			fclose(fp);
			fprintf(stderr, "Failed to load program\n\n\n");
			return (0);
		}

		fclose(fp);
		return (1);
	}
}


//----------------------------------------------------------------------
//
//  Cpu::SetTimer
//
//  Set the CPU timer.  This simply causes an interrupt after the
//  specified time (in microseconds) has elapsed.
//
//----------------------------------------------------------------------
void Cpu::SetTimer(uint32 usecs)
{
	timerInterrupt = usElapsed + usecs;
}

//----------------------------------------------------------------------
//
//  Cpu::GetTimer
//
//----------------------------------------------------------------------
uint32 Cpu::GetTimer()
{
	return ((uint32) (timerInterrupt - usElapsed));
}


//----------------------------------------------------------------------
//
//  Cpu::KbdGetChar
//
//  Get a single character from the keyboard buffer.
//
//----------------------------------------------------------------------
uint32 Cpu::KbdGetChar()
{
	uint32 v;

	if (kbdbufferedchars == 0) {
		return (0);
	}
	v = kbdbuffer[kbdrpos++];
	kbdrpos %= DLX_KBD_BUFFER_SIZE;
	kbdbufferedchars--;
	return (v);
}

//----------------------------------------------------------------------
//
//  Cpu::IgnoreExit
//
//----------------------------------------------------------------------
void Cpu::IgnoreExit(int ignore)
{
	flags &= ~DLX_FLAG_IGNORE_EXIT;
	if (ignore) {
		flags |= DLX_FLAG_IGNORE_EXIT;
	}
}

//----------------------------------------------------------------------
//
//  Cpu::OutputBasicBlock
//
//  Print a basic block and reset statistics gathering.
//
//----------------------------------------------------------------------
void Cpu::OutputBasicBlockActual()
{
	int i, ninstrs;

	ninstrs = (PC() - basicBlockStart) >> 2;
	// Print out the basic block information here
	if (flags & DLX_TRACE_INSTRUCTIONS) {
		fprintf(tracefp, "I %x %d\n", basicBlockStart, ninstrs);
	}
	if (flags & DLX_TRACE_MEMORY) {
		for (i = 0; i < naccesses; i++) {
			fprintf(tracefp, "%s r%d %x %x\n", accesses[i].inst,
					accesses[i].reg, accesses[i].addr, accesses[i].value);
		}
	}
	naccesses = 0;
}

//----------------------------------------------------------------------
//
//  Cpu::DebugControl
//
//  Hands control to the debugger
//
//----------------------------------------------------------------------
void Cpu::DebugControl()
{
	if (debugger)
		stopnext = debugger->TakeControl();
}
