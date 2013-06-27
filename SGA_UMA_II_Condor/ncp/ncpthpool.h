#pragma once
#include "uwkelobj.h"

class CThreadPool
{
public:
//	list<HTHREAD> m_lstThreads;
	int		m_nRunningThreads;
	HANDLE	m_mxCounter;

	CThreadPool(){
		m_nRunningThreads=0;
		m_mxCounter = CreateMutex();
	}
	~CThreadPool(){
		//DestroyMutex( m_mxCounter );
		CloseHandle( m_mxCounter );
	}

	int LockedInc(){
		int nRet = m_nRunningThreads;
		LockMutex( m_mxCounter );
		m_nRunningThreads++;
		UnlockMutex( m_mxCounter );
		return nRet;
	}

	int LockedDec()
	{
		LockMutex( m_mxCounter );
		m_nRunningThreads--;
		int nRet = m_nRunningThreads;
		UnlockMutex( m_mxCounter );
		return nRet;
	}

	void AddThread( HANDLE hThread ){};
};