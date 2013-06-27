#pragma once
#include <list>
using namespace std;
#include <treetmpl.h>

#include "uwkelobj.h"
#include "ncphost.h"
#include "ncpthpool.h"

//return 0 if job is successfully finished.
typedef int (*FUNCJOB)(int nId, CClientSession* pSession, void* pParam);

class CJobObject;
class CJobSet;

template < typename Tx, typename Ax=allocator<Tx> >
class CFifoSet : public list<Tx, Ax>
{
public:
	void Push( Tx& val )
	{	push_back( val );	}
	Tx Pop()
	{	Tx val = front(); pop_front(); return val;	}

	bool IsEmpty()
	{	return empty();	}

	CFifoSet::iterator Begin()
	{	return begin();	}
	CFifoSet::const_iterator Begin() const
	{	return begin();	}
	CFifoSet::iterator End()
	{	return end();	}
	CFifoSet::const_iterator End() const
	{	return end();	}
};

class CJobObject
{
public:
	int			m_nId;			//job identification number
	FUNCJOB		m_pfnExc;		//the execution function pointer
	void*		m_pParam;		//the parameter for the job execution func
	double		m_rCpuHint;		//hint for how long the job needs.

	int			m_nFailCount;	//failed count.
	double		m_tmStart;		//starting and stopping time for the job
	double		m_tmStop;
	IN_ADDR		m_addrHost;		//running host address

	CJobSet*	m_pJobSet;		//point back to the set.
public:
	//constructor
	CJobObject( int nId, FUNCJOB pfnExc, void* pParam, double rCpuHint, CJobSet* pJobSet );

	//properties
	int GetId(){ return m_nId; }
	CJobSet* GetJobSet(){	return m_pJobSet; }
	void SetJobSet( CJobSet* pJobSet ){ m_pJobSet = pJobSet; }

	//the launching function
	bool Launch( CHostObject* pHostObj );		//launch the job on the host object.
};

class CJobSet
{
private:
	CFifoSet<CJobObject*>	m_PendingSet;			//pending jobs
	CFifoSet<CJobObject*>	m_ZombieSet;			//finished jobs
	CThreadPool				m_ThreadPool;			//thread pool for performance improvement in the future.

	HANDLE					m_mxLock;				//lock mutex
	HANDLE					m_evMoreJob;			//event to wake up a waiting router for more jobs in the pending set
	HANDLE					m_evLastJob;			//event to wake up a waiting router for the last job is done
//	HANDLE					m_evWakeRouter;			//event to wake up a waited routing subroutine.
public:
	CJobSet();
	~CJobSet();

	void Lock();
	void Unlock();

	//add a job object into the set.
	void Add( int nId, FUNCJOB pfnExc, void* pParam, double rCpuHint );

	//clear all the pending and zombie jobs.
	void Clear();

	//the function will be blocked until all the jobs are finished.
	void RouteJobs( CHostSet* pHostSet );

	//the true working thread for each job
	static void* WorkThread( void* pParam );

	void DumpPendingJobs();
	void CancelPendingJobs();
};

typedef struct
{
public:
	CJobObject	* m_pJobObj;
	CHostObject * m_pHostObj;
}WORKTHREADPARAM;
