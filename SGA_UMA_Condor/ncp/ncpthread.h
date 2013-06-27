#pragma once

#ifdef _WIN32
#include <windows.h>

#else

#include <strings.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/sem.h>
#include <pthread.h>
#include <semaphore.h>

#endif


#ifdef _WIN32
#define HMUTEX		HANDLE
#define HSEMAPHORE	HANDLE
#define	HEVENT		HANDLE
#define HTHREAD		HANDLE
#define HPROCESS	HANDLE
#define THREAD_ID	DWORD
#define THREAD_FUNC	LPTHREAD_START_ROUTINE

#else
typedef pthread_mutex_t* HMUTEX;
typedef sem_t* HSEMAPHORE;
typedef HMUTEX HEVENT;
//#define	HEVENT		HANDLE
#define HTHREAD		pthread_t
#define HPROCESS	pid_t
#define THREAD_ID	pthread_t
typedef void * (*THREAD_FUNC)(void *);

#ifndef SEM_R
#define SEM_R	0400
#endif

#ifndef SEM_A
#define SEM_A	0200
#endif

#define NORMAL_PRIORITY_CLASS	1
#define IDLE_PRIORITY_CLASS		2

union semun {
    int val;
    struct semid_ds *buf;
    u_short *array;
};

#endif

inline HMUTEX CreateMutex()
{
#ifdef _WIN32
	return CreateMutex( NULL, FALSE, NULL );
#else
	HMUTEX hmutex = new pthread_mutex_t();
	pthread_mutex_init( hmutex, NULL );
	return hmutex;
#endif
}

inline void DestroyMutex( HMUTEX hMutex )
{
#ifdef _WIN32
	CloseHandle( hMutex );
#else
	pthread_mutex_destroy( hMutex );
	delete hMutex;
#endif
}

inline void LockMutex( HMUTEX hMutex )
{
#ifdef _WIN32
	WaitForSingleObject( hMutex, INFINITE );
#else
	pthread_mutex_lock( hMutex );
#endif
}

inline void UnlockMutex( HMUTEX hMutex )
{
#ifdef _WIN32
	ReleaseMutex( hMutex );
#else
	pthread_mutex_unlock( hMutex );
#endif
}

inline HSEMAPHORE CreateSemaphore( int initCount=0 )
{
#ifdef _WIN32
	return CreateSemaphore( NULL, initCount, 0xffff, NULL );
#else
	HSEMAPHORE hsema = new sem_t();
	sem_init( hsema, 0/*shared by threads*/, initCount );
	return hsema;
#endif
}

inline void DestroySemaphore( HSEMAPHORE hsema )
{
#ifdef _WIN32
	CloseHandle( hsema );
#else
	sem_destroy( hsema );
#endif
}

inline void WaitSemaphore( HSEMAPHORE hsema )
{
#ifdef _WIN32
	WaitForSingleObject( hsema, INFINITE );
#else
	sem_wait( hsema );
#endif
}

inline void ReleaseSemaphore( HSEMAPHORE hsema )
{
#ifdef _WIN32
	ReleaseSemaphore( hsema, 1, NULL );
#else
	sem_post( hsema );
#endif
}

inline HEVENT CreateEvent( BOOL bInitState )
{
#ifdef _WIN32
	return ::CreateEvent( NULL, TRUE, bInitState, NULL );
#else
	return 0;
#endif
}

inline void WaitEvent( HEVENT hEvent )
{
#ifdef _WIN32
	WaitForSingleObject( hEvent, INFINITE );
#endif
}

inline void ClearEvent( HEVENT hEvent )
{
#ifdef _WIN32
	::ResetEvent( hEvent );
#endif
}

inline void SignalEvent( HEVENT hEvent )
{
#ifdef _WIN32
	::SetEvent( hEvent );
#endif
}

inline void DestroyEvent( HEVENT hEvent )
{
#ifdef _WIN32
	CloseHandle( hEvent );
#endif
}

inline bool CreateThread( THREAD_FUNC threadStart, void* threadParam, unsigned initFlags, THREAD_ID* pid )
{
#ifdef _WIN32
	HANDLE hThread = CreateThread( NULL, 0, (LPTHREAD_START_ROUTINE)threadStart, threadParam, initFlags, (LPDWORD)pid );
	CloseHandle( hThread );
#else
	pthread_attr_t attr;

	//initialize the thread attr.
	pthread_attr_init (&attr);

	pthread_t thread_id;
	pthread_create( &thread_id, &attr, threadStart, threadParam );
	if( pid!=NULL )*pid = thread_id;
#endif
	return true;
}

inline HTHREAD GetThreadHandle( THREAD_ID threadId )
{
#ifdef _WIN32
	return OpenThread( THREAD_ALL_ACCESS, FALSE, threadId );
#else
	return threadId;
#endif
}

inline void CloseThreadHandle( HTHREAD hThread )
{
#ifdef _WIN32
	CloseHandle( hThread );
#endif
}

inline void WaitThread( HTHREAD hThread )
{
#ifdef _WIN32
	WaitForSingleObject( hThread, INFINITE );
#else
	pthread_join( hThread, NULL );
#endif
}

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
}

inline bool CreateProcess( const char* lpCmdLine, const char* lpCurDir, int nPrority, HPROCESS* phProc )
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
	return (bool)suc;
#else
	HPROCESS pid;

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
		return false;
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
				_exit( 127 );
			}
		}else{	//need sync, just do it on the first child
			chdir( lpCurDir );	//set the current directory and run the child process
			execl( "/bin/sh", "sh", "-c", lpCmdLine, (char*)0 );
			_exit( 127 );
		}
	}else{
		if( phProc!=NULL ){
			*phProc = pid;
		}else{
			//no sync is needed, to avoid a zombie defunc process, wait the pid of the first child
			waitpid( pid, NULL, 0 );

			//signal the second child to run.
			struct sembuf opbuf;
			opbuf.sem_num = 0; opbuf.sem_op = 1; opbuf.sem_flg = 0;
			semop( semid, &opbuf, 1 );
		}
		return true;
	}
#endif
}

inline void WaitProcess( HPROCESS hProcess )
{
#ifdef _WIN32
	WaitForSingleObject( hProcess, INFINITE );
#else
	int status;
	waitpid( hProcess, &status, 0 );
#endif
}

inline void CloseProcessHandle( HPROCESS hProcess )
{
#ifdef _WIN32
	CloseHandle( hProcess );
#endif
}
