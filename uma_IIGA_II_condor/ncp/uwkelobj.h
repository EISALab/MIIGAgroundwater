//the file is defined to provide a uniform interface for windows, unix and linux process and thread support.
/*---------------------------------interfaces provided------------------------------------------------
void MilliSleep( int millisec );		//Sleep milliseconds
HANDLE CreateMutex();					//Create a mutex. NULL if failed
BOOL LockMutex( HANDLE hMutex );		//Lock and Unlock the mutex
BOOL UnlockMutex( HANDLE hMutex );

HANDLE CreateSemaphore( int initCount=0 );	//Create a semaphore with initCount as initial count.
BOOL WaitForSemaphore( HANDLE hSema );		//Wait on a semaphore object
BOOL ReleaseSemaphore( HANDLE hSema );		//Insease the smeaphore count by 1.

HANDLE CreateEvent( BOOL bManualReset, BOOL bInitState );	//Create a Event object. The event could be manual reset, or auto reset. bInitState is the initial state of the event.
BOOL WaitForEvent( HANDLE hEvent );							//Wait for the event.
BOOL SignalEvent( HANDLE hEvent );							//Set the event to signal state
BOOL ClearEvent( HANDLE hEvent );							//Clear the event to non-signal state

HANDLE CreateThread( THREAD_ROUTINE threadStart, void* threadParam, unsigned initFlags );	//Create a thread from threadStart. initFlags is reserved, should always be 0.
BOOL WaitForThread( HANDLE hThread, LPDWORD lpExitCode=NULL );								//Wait for a thread and retrieve the exit code of thread.
void ExitThread( DWORD dwExitCode );														//normal exit of a thread.
BOOL TerminateThread( HANDLE hThread );														//request termination of a thread. Note: thread may not terminate or immediately terminate in Unix version.
void ThreadTestCancel();																	//Thread should call the function to check the terminating signal so that it can terminate.

BOOL CreateProcess( const char* lpCmdLine, const char* lpCurDir, HANDLE* phProc );			//Create a process
BOOL WaitForProcess( HANDLE hProcess, LPDWORD lpExitCode=NULL );														//wait for a process to terminate.
BOOL SetProcessPriority( HANDLE hProcess, DWORD dwPriorityClass );							//set the priority of a process.
BOOL KillProcess( HANDLE hProcess );														//terminate a process

BOOL WaitForObject( HANDLE hObject );	//Wait for a single kernel object. Could be mutex, semaphore, event, thread or process
BOOL CloseHandle( HANDLE hObject );		//every kernel object should call it to release the kernel object resource.													

										By Shengquan Yan  EMAIL:smyan@uiuc.edu, sq_yan@hotmail.com
-------------------------------------------------------------------------------------------------------------*/


#ifndef _UWKEL_H_
#define _UWKEL_H_

//included head files
#ifdef _WIN32
#include <windows.h>

#else
#include <strings.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/sem.h>
#include <sys/time.h> 
#include <sys/resource.h> 
#include <pthread.h>
#include <semaphore.h>
#include <errno.h>

typedef unsigned long	DWORD;
typedef DWORD*			LPDWORD;

#endif


/************************** Kernel Object Definition ***********************************/
#ifdef _WIN32
#define THREAD_ROUTINE	LPTHREAD_START_ROUTINE
#else

#define KERNEL_EMPTY		0
#define	KERNEL_MUTEX		1
#define KERNEL_SEMAPHORE	2
#define KERNEL_EVENT		3
#define KERNEL_THREAD		4
#define KERNEL_PROCESS		5

#define REALTIME_PRIORITY_CLASS			-20		//Specify this class for a process that has the highest possible priority. The threads of the process preempt the threads of all other processes, including operating system processes performing important tasks. For example, a real-time process that executes for more than a very brief interval can cause disk caches not to flush or cause the mouse to be unresponsive.
#define HIGH_PRIORITY_CLASS				-15		//Specify this class for a process that performs time-critical tasks that must be executed immediately. The threads of the process preempt the threads of normal or idle priority class processes. An example is the Task List, which must respond quickly when called by the user, regardless of the load on the operating system. Use extreme care when using the high-priority class, because a high-priority class application can use nearly all available CPU time.
#define ABOVE_NORMAL_PRIORITY_CLASS		-10		//Windows 2000/XP: Indicates a process that has priority above NORMAL_PRIORITY_CLASS but below HIGH_PRIORITY_CLASS. 
#define	NORMAL_PRIORITY_CLASS			0		//Specify this class for a process with no special scheduling needs. 
#define BELOW_NORMAL_PRIORITY_CLASS		10		//Windows 2000/XP: Indicates a process that has priority above IDLE_PRIORITY_CLASS but below NORMAL_PRIORITY_CLASS. 
#define IDLE_PRIORITY_CLASS				20		//Specify this class for a process whose threads run only when the system is idle. The threads of the process are preempted by the threads of any process running in a higher priority class. An example is a screen saver. The idle-priority class is inherited by child processes. 

typedef void* (*THREAD_ROUTINE)(void *);

typedef struct _event_t
{
	bool				manual;
	sem_t				sema;
	pthread_mutex_t		mutex;
}event_t;

typedef struct _kernel_t
{
	int type;
	union {
		pthread_mutex_t *	mutex;
		sem_t *				sema;
		event_t *			event;
		pthread_t			thread;
		pid_t				pid;
	}k_un;
}kernel_t;

typedef kernel_t*	HANDLE;

inline int event_init( event_t* event, bool manual_reset, bool init_state  )
{
	pthread_mutex_init( &(event->mutex), NULL );
	sem_init( &(event->sema), 0/*shared by threads*/, 0 );
	event->manual = manual_reset;
	if( init_state ){
		sem_post( &(event->sema) );
	}
	return 0;
}

inline int event_destroy( event_t* event )
{
	pthread_mutex_destroy( &(event->mutex) );
	sem_destroy( &(event->sema) );
	return 0;
}

inline int event_set( event_t* event )
{
	pthread_mutex_lock( &(event->mutex) );
	int value;
	sem_getvalue( &(event->sema), &value );
	if( value==0 ){
		sem_post( &(event->sema) );
	}
	pthread_mutex_unlock( &(event->mutex) );

	return 0;
}

inline int event_reset( event_t* event )
{
	pthread_mutex_lock( &(event->mutex) );
	int status;
	while( (status=sem_trywait( &(event->sema) ))==0 );
	status = errno;			//save the errno.
	pthread_mutex_unlock( &(event->mutex) );
	return status==EAGAIN;
}

inline int event_wait( event_t* event )
{
	sem_wait( &(event->sema) );
	//a manual reset event should keep the state to signal status.
	if( event->manual ){
		event_set( event );
	}
	return 0;
}

#ifndef SEM_R
#define SEM_R	0400
#endif

#ifndef SEM_A
#define SEM_A	0200
#endif

union semun {
    int val;
    struct semid_ds *buf;
    u_short *array;
};

#endif //_WIN32

/*************************** functions for others *****************************/

inline void MilliSleep( int millisec )
{
#ifdef _WIN32
	Sleep( millisec );
#else
	usleep( millisec * 1000 );
#endif
}


/*************************** functions for mutex, semaphore and event *****************************/

//return:	the handle of the mutex. NULL if failed
inline HANDLE CreateMutex()
{
#ifdef _WIN32
	return CreateMutex( NULL, FALSE, NULL );
#else
	kernel_t * pKelObj = new kernel_t();
	pKelObj->type = KERNEL_MUTEX;
	pKelObj->k_un.mutex = new pthread_mutex_t();
	pthread_mutex_init( pKelObj->k_un.mutex, NULL );
	return pKelObj;
#endif
}

//hMutex(in): the mutex handle
//return:	true if successful
inline BOOL LockMutex( HANDLE hMutex )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hMutex, INFINITE );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hMutex;
	ASSERT( pKelObj->type==KERNEL_MUTEX );
	int status = pthread_mutex_lock( pKelObj->k_un.mutex );
	return status==0;
#endif
}

inline BOOL UnlockMutex( HANDLE hMutex )
{
#ifdef _WIN32
	return ReleaseMutex( hMutex );
#else
	kernel_t* pKelObj = hMutex;
	ASSERT( pKelObj->type==KERNEL_MUTEX );
	int status = pthread_mutex_unlock( pKelObj->k_un.mutex );
	return status==0;
#endif
}

inline HANDLE CreateSemaphore( int initCount=0 )
{
#ifdef _WIN32
	return CreateSemaphore( NULL, initCount, 0xffff, NULL );
#else
	kernel_t * pKelObj = new kernel_t();
	pKelObj->type = KERNEL_SEMAPHORE;
	pKelObj->k_un.sema = new sem_t();
	sem_init( pKelObj->k_un.sema, 0/*shared by threads*/, initCount );
	return pKelObj;
#endif
}

inline BOOL WaitForSemaphore( HANDLE hSema )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hSema, INFINITE );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hSema;
	ASSERT( pKelObj->type==KERNEL_SEMAPHORE );
	int status = sem_wait( pKelObj->k_un.sema );
	return status==0;
#endif
}

inline BOOL ReleaseSemaphore( HANDLE hSema )
{
#ifdef _WIN32
	return ReleaseSemaphore( hSema, 1, NULL );
#else
	kernel_t* pKelObj = hSema;
	ASSERT( pKelObj->type==KERNEL_SEMAPHORE );
	int status = sem_post( pKelObj->k_un.sema );
	return status==0;
#endif
}

inline HANDLE CreateEvent( BOOL bManualReset, BOOL bInitState )
{
#ifdef _WIN32
	return ::CreateEvent( NULL, bManualReset, bInitState, NULL );
#else
	kernel_t * pKelObj = new kernel_t();
	pKelObj->type = KERNEL_EVENT;
	pKelObj->k_un.event = new event_t();
	event_init( pKelObj->k_un.event, bManualReset, bInitState );
	return pKelObj;
#endif
}

inline BOOL WaitForEvent( HANDLE hEvent )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hEvent, INFINITE );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hEvent;
	ASSERT( pKelObj->type==KERNEL_EVENT );
	int status = event_wait( pKelObj->k_un.event );
	return status==0;
#endif
}

inline BOOL SignalEvent( HANDLE hEvent )
{
#ifdef _WIN32
	return ::SetEvent( hEvent );
#else
	kernel_t* pKelObj = hEvent;
	ASSERT( pKelObj->type==KERNEL_EVENT );
	int status = event_set( pKelObj->k_un.event );
	return status==0;
#endif
}

inline BOOL ClearEvent( HANDLE hEvent )
{
#ifdef _WIN32
	return ::ResetEvent( hEvent );
#else
	kernel_t* pKelObj = hEvent;
	ASSERT( pKelObj->type==KERNEL_EVENT );
	int status = event_reset( pKelObj->k_un.event );
	return status==0;
#endif
}

/*************************** functions for thread and process *****************************/

//Create a thread from threadStart. 
//threadStart(in): the thread starting function. 
//threadParam(in): the parameter passed to the thread function.
//initFlags is reserved, should always be 0.
inline HANDLE CreateThread( THREAD_ROUTINE threadStart, void* threadParam, unsigned initFlags )
{
#ifdef _WIN32
	return CreateThread( NULL, 0, (LPTHREAD_START_ROUTINE)threadStart, threadParam, initFlags, (LPDWORD)NULL );
#else
	pthread_attr_t attr;
	//initialize the thread attr.
	pthread_attr_init (&attr);

	pthread_t thread_id;
	int status = pthread_create( &thread_id, &attr, threadStart, threadParam );
	if( status!=0 )return NULL;

	kernel_t* pKelObj = new kernel_t();
	pKelObj->type = KERNEL_THREAD;
	pKelObj->k_un.thread = thread_id;
	return pKelObj;
#endif
}

/*
inline void SuspendThread( THREAD_ID threadId )
{

#ifdef _WIN32
	HANDLE hThread = OpenThread( THREAD_ALL_ACCESS,FALSE, threadId );
	if( hThread!=NULL ){
		SuspendThread( hThread );
		CloseHandle( hThread );
	}
#endif
}

inline void ResumeThread( THREAD_ID threadId )
{
#ifdef _WIN32
	HANDLE hThread = OpenThread( THREAD_ALL_ACCESS,FALSE, threadId );
	if( hThread!=NULL ){
		ResumeThread( hThread );
		CloseHandle( hThread );
	}
#endif
}*/

//Thread can only be wait once in Unix and Linux. 
//After waiting the handle should not be used except for CloseHandle().
inline BOOL WaitForThread( HANDLE hThread, LPDWORD lpExitCode=NULL )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hThread, INFINITE );
	if( status!=WAIT_OBJECT_0 )return FALSE;
	if( lpExitCode!=NULL ){
		return GetExitCodeThread( hThread, lpExitCode );
	}
	return TRUE;
#else
	kernel_t* pKelObj = hThread;
	ASSERT( pKelObj->type == KERNEL_THREAD );
	int status = pthread_join( pKelObj->k_un.thread, (void**)lpExitCode );

	//POSIX stardard can only wait thread once, the thread is immediately recycled after the waiting.
	pKelObj->type = KERNEL_EMPTY;
	return status==0;
#endif
}

#ifndef _WIN32
inline void ExitThread( DWORD dwExitCode )
{
	pthread_exit( (void*)dwExitCode );
}
#endif

inline BOOL TerminateThread( HANDLE hThread )
{
#ifdef _WIN32
	return TerminateThread( hThread, 0 );
#else
	kernel_t* pKelObj = hThread;
	ASSERT( pKelObj->type == KERNEL_THREAD );
	int status = pthread_cancel( pKelObj->k_un.thread );

	//POSIX stardard can only wait thread once, the thread is immediately recycled after the waiting.
	//pKelObj->type = KERNEL_EMPTY;
	return status==0;
#endif
}

inline void ThreadTestCancel()
{
#ifndef _WIN32
	pthread_testcancel();
#endif
}

inline BOOL CreateProcess( const char* lpCmdLine, const char* lpCurDir, HANDLE* phProc )
{
#ifdef _WIN32
	STARTUPINFO si;
	memset( &si, 0, sizeof(si) );
	si.cb = sizeof( si );

	PROCESS_INFORMATION pi;
	memset( &pi, 0, sizeof(pi) );
	BOOL suc = CreateProcess( NULL, (char*)lpCmdLine, NULL, NULL, FALSE, 0, NULL, lpCurDir, &si, &pi );
	if( suc ){
		CloseHandle( pi.hThread );
		if( phProc!=NULL ){
			*phProc = pi.hProcess;
		}else{
			CloseHandle( pi.hProcess );
		}
	}
	return suc;
#else
	pid_t pid;

	//create a semaphore to avoid zombie if no sync.
	int semid=0;
	if( phProc==NULL ){
		semget( IPC_PRIVATE, 1, SEM_R|SEM_A );

		union semun semarg;
		semarg.val = 0;
		//set the semaphore to 0. the 0 is for the union semun.val.
		semctl( semid, 0, SETVAL, semarg );
	}

	if( (pid=fork())<0 ){
		return FALSE;
	}else if( pid==0 ){		//this is the child
		if( phProc==NULL ){	//no sync is needed, fork again to avoid the defunct process.
			//using IPC semaphore to synchronize the second and the parent.
            pid = fork();
			if( pid>0 ){
				//the first child exit immediately so that the second child's parent be init.
				exit(0);
			}else{			//the second child
				//the second child wait the running signal of the parent
				struct sembuf opbuf;
				opbuf.sem_num = 0; opbuf.sem_op = -1; opbuf.sem_flg = 0;
				semop( semid, &opbuf, 1 );

				//now delete the semaphore, it's a system resource.
				semctl( semid, 0, IPC_RMID, 0 );

				//run it now.
				chdir( lpCurDir );	//set the current directory and run the child process
				execl( "/bin/sh", "sh", "-c", lpCmdLine, (char*)0 );
				_exit( -1 );		//shouldn't do it because execl doesn't return
			}
		}else{	//need sync, just do it on the first child
			chdir( lpCurDir );	//set the current directory and run the child process
			execl( "/bin/sh", "sh", "-c", lpCmdLine, (char*)0 );
			_exit( -1 );		//shouldn't do it because execl doesn't return
		}
	}else{
		if( phProc!=NULL ){
			//create a kernel object and send it back through phProc
			kernel_t* pKelObj = new kernel_t();
			pKelObj->type = KERNEL_PROCESS;
			pKelObj->k_un.pid = pid;
			*phProc = pKelObj;
		}else{
			//no sync is needed, to avoid a zombie defunc process, wait the pid of the first child
			waitpid( pid, NULL, 0 );

			//signal the second child to run.
			struct sembuf opbuf;
			opbuf.sem_num = 0; opbuf.sem_op = 1; opbuf.sem_flg = 0;
			semop( semid, &opbuf, 1 );
		}
		return TRUE;
	}
#endif
}

//Process can only be wait once in Unix and Linux. 
//After waiting the handle should not be used except for CloseHandle().
inline BOOL WaitForProcess( HANDLE hProcess, LPDWORD lpExitCode=NULL )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hProcess, INFINITE );
	if( (status==WAIT_OBJECT_0) && (lpExitCode!=NULL) )GetExitCodeProcess( hProcess, lpExitCode );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hProcess;
	ASSERT( pKelObj->type == KERNEL_PROCESS );
	int status;
	BOOL suc =( pKelObj->k_un.pid == waitpid( pKelObj->k_un.pid, &status, 0 ) );
	if( suc && (lpExitCode!=NULL) ){
		*lpExitCode = status;
	}

	//unix stardard can only wait process once, the process is immediately recycled after the waiting.
	pKelObj->type = KERNEL_EMPTY;
	return suc;
#endif
}

inline BOOL SetProcessPriority( HANDLE hProcess, DWORD dwPriorityClass )
{
#ifdef _WIN32
	return SetPriorityClass( hProcess, dwPriorityClass );
#else
	kernel_t* pKelObj = hProcess;
	ASSERT( pKelObj->type == KERNEL_PROCESS );
	int status = setpriority( PRIO_PROCESS, pKelObj->k_un.pid, dwPriorityClass );
	return status==0;
#endif
}

inline BOOL KillProcess( HANDLE hProcess )
{
#ifdef _WIN32
	return TerminateProcess( hProcess, 0 );
#else
	kernel_t* pKelObj = hProcess;
	ASSERT( pKelObj->type == KERNEL_PROCESS );
	int status = kill( pKelObj->k_un.pid, SIGABRT );
	return status==0;
#endif
}

inline BOOL WaitForObject( HANDLE hObject )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hObject, INFINITE );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hObject;
	switch( pKelObj->type ){
	case KERNEL_MUTEX:
		return LockMutex( hObject );
		break;
	case KERNEL_SEMAPHORE:
		return WaitForSemaphore( hObject );
		break;
	case KERNEL_EVENT:
		return WaitForEvent( hObject );
		break;
	case KERNEL_THREAD:
		return WaitForThread( hObject, NULL );
		break;
	case KERNEL_PROCESS:
		return WaitForProcess( hObject, NULL );
		break;
	default:
		ASSERT( FALSE );
		return FALSE;
	}
	return FALSE;
#endif
}


#ifndef _WIN32
inline BOOL CloseHandle( HANDLE hObject )
{
	kernel_t* pKelObj = hObject;
	int status = 0;
	switch( pKelObj->type ){
	case KERNEL_MUTEX:
		status = pthread_mutex_destroy( pKelObj->k_un.mutex );
		delete pKelObj->k_un.mutex;
		break;
	case KERNEL_SEMAPHORE:
		status = sem_destroy( pKelObj->k_un.sema );
		delete pKelObj->k_un.sema;
		break;
	case KERNEL_EVENT:
		status = event_destroy( pKelObj->k_un.event );
		delete pKelObj->k_un.event;
		break;
	case KERNEL_THREAD:
		status = pthread_detach( pKelObj->k_un.thread );
		break;
	case KERNEL_PROCESS:
		//WaitForProcess( hObject ); can't really remove the zombie process without waiting...
		break;
	case KERNEL_EMPTY:
		break;
	default:
		ASSERT( FALSE );
		return FALSE;
	}
	delete pKelObj;
	return status==0;
}
#endif _WIN32

#endif	//_UWKEL_H
