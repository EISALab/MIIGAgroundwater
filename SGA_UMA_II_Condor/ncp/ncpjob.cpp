#include "ncphead.h"
#include "netstd.h"
#include "uwkelobj.h"
#include "ncphost.h"
#include "ncpjob.h"
#include "ofdump.h"

double GetTime()
{
	static bool bInit = false;
	static double ticks_per_ms;

	if( !bInit ){
		LARGE_INTEGER freq;
		QueryPerformanceFrequency(&freq);				//get ticks per second
		ticks_per_ms = (double)freq.QuadPart/1000.0;	//convert to milliseconds
	}
	LARGE_INTEGER counter;
	QueryPerformanceCounter(&counter);				
	return (double)counter.QuadPart/ticks_per_ms;//in milliseconds
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////
//											CJobObject
///////////////////////////////////////////////////////////////////////////////////////////////////////////

CJobObject::CJobObject( int nId, FUNCJOB pfnExc, void* pParam, double rCpuHint, CJobSet* pJobSet )
{
	m_nId = nId;
	m_pfnExc = pfnExc;
	m_pParam = pParam;
	m_rCpuHint = rCpuHint;

	m_nFailCount = 0;
	m_tmStart = m_tmStop = 0;
	bzero( &m_addrHost, sizeof(IN_ADDR) );
	m_pJobSet = pJobSet;
}

bool CJobObject::Launch( CHostObject* pHostObj )
{
	m_addrHost = pHostObj->GetAddr();

	//create the session if it not exists.
	if( pHostObj->m_pSession==NULL ){
		int nRet = pHostObj->GetHostSet()->CreateSession( pHostObj, &pHostObj->m_pSession );
		if( FAILED(nRet) )return false;		//connection failed.
	}
	try{
		SYSINFO sysinfo;
		pHostObj->m_pSession->GetSysInfo( &sysinfo );
		if( IsSet(sysinfo.nSysStat, SYSTEM_SUSPEND_SERVICE) ){
			return false;
		}

		return (*m_pfnExc)( m_nId, pHostObj->m_pSession, m_pParam );
	}catch( CSockException* e){
		IN_ADDR addr = pHostObj->GetAddr();
		char* pname = GetHostByAddr( addr );
		cdump<<lock<<"sock exception:"<<pname<<"--"<<e->m_nErrorCode<<unlock<<endl;

		e->Delete();
		perror( "sock error launcing job" );
		//if there is socket error, delete the session.
		if( pHostObj->m_pSession ){
			//pHostObj->m_pSession->Disconnect();
			delete pHostObj->m_pSession;
		}
		pHostObj->m_pSession = NULL;
		return false;
	}

	return true;
}


///////////////////////////////////////////////////////////////////////////////////////////////////////////
//											CJobSet
///////////////////////////////////////////////////////////////////////////////////////////////////////////

CJobSet::CJobSet()
{
	m_mxLock = CreateMutex();
	m_evMoreJob = CreateEvent( FALSE, FALSE );
	m_evLastJob = CreateEvent( FALSE, FALSE );
	//m_evWakeRouter = CreateEvent( TRUE, FALSE );
}

CJobSet::~CJobSet()
{
	Clear();
	CloseHandle( m_mxLock );
	CloseHandle( m_evMoreJob );
	CloseHandle( m_evLastJob );
	//CloseHandle( m_evWakeRouter );
}

void CJobSet::Lock()
{
	LockMutex( m_mxLock );
}

void CJobSet::Unlock()
{
	UnlockMutex( m_mxLock );
}

void CJobSet::Add( int nId, FUNCJOB pfnExc, void* pParam, double rCpuHint )
{
	CJobObject* pJobObj = new CJobObject( nId, pfnExc, pParam, rCpuHint, this );

	Lock();
	m_PendingSet.Push( pJobObj );
	Unlock();
}

void CJobSet::Clear()
{
	Lock();
	while( !m_PendingSet.IsEmpty() ){
		delete m_PendingSet.Pop();
	}
	while( !m_ZombieSet.IsEmpty() ){
		delete m_ZombieSet.Pop();
	}
	Unlock();
}

int timeout = 6*60*1000;

void CJobSet::RouteJobs( CHostSet* pHostSet )
{
	timeout = 70*1000;
//	while( !m_PendingSet.IsEmpty() ){
	while( true ){
		while( !m_PendingSet.IsEmpty() ){

			//wait for a host tocken to run a job
			CHostObject* pHostObj = pHostSet->RetrieveTocken();
			ASSERT( pHostObj!=NULL );

			//get the job object from the job set.
			Lock();
			CJobObject* pJobObj = m_PendingSet.Pop();
			Unlock();
			ASSERT( pJobObj!=NULL );

			IN_ADDR addr = pHostObj->GetAddr();
			char* pname = GetHostByAddr( addr );
			cdump/*<<"("<<pHostObj->m_rSpeedHint<<")"*/<<lock<<pname<<"("<<pHostObj->GetSpeedHint()<<")"<<"\trunning job:"<<pJobObj->GetId()<<endl<<unlock;
			//cout<<inet_ntoa( addr )<<"\t"<<phent->h_name<<endl;

			//set the parameters
			WORKTHREADPARAM* pParam = new WORKTHREADPARAM();
			pParam->m_pJobObj = pJobObj;
			pParam->m_pHostObj = pHostObj;

			//increate the running thread counter.
			m_ThreadPool.LockedInc();

			//create a worker thread
			HANDLE hThread = 0;
			hThread=CreateThread( (THREAD_ROUTINE)WorkThread, (void*)pParam, 0 );

			//may need to buffer the thread into the thread pool in the future.
			m_ThreadPool.AddThread( hThread );
			CloseHandle( hThread );
		}

		//if the thread is waken up, there may be failed jobs back to the pending set.
		cdump<<lock<<"waiting for hosts... "<<timeout<<endl<<unlock;
//		pHostSet->DumpAllHosts();
		HANDLE arrEvents[2];
		arrEvents[0] = m_evMoreJob;
		arrEvents[1] = m_evLastJob;
		DWORD status = WaitForMultipleObjects( 2, arrEvents, FALSE, timeout );
		if( status==WAIT_TIMEOUT ){
		//if( WaitForSingleObject( m_evWakeRouter, timeout )==WAIT_TIMEOUT ){
			timeout = 1.8*timeout;
			timeout = min( 30*60*1000, timeout );

			pHostSet->DumpRunningHosts();
			cdump<<lock<<"canceling running hosts..."<<endl<<unlock;

			int nAvailHosts = pHostSet->GetAliveHostCount();
			pHostSet->CancelRunningHosts( nAvailHosts );
			status = WaitForMultipleObjects( 2, arrEvents, FALSE, INFINITE );
			//WaitForSingleObject( m_evWakeRouter, INFINITE );
			MilliSleep( 1000 );
			pHostSet->DumpAllHosts();
		}
		//if the last job is done, then exit the loop.
		if( status==WAIT_OBJECT_0+1 )break;

		cdump<<lock<<"waiting wake up..."<<endl<<unlock;
		//ClearEvent( m_evWakeRouter );

/*		int maxwait = 2;
		for( int i=0; i<maxwait; i++ ){
//			if( WaitForSingleObject( m_evWakeRouter, 15*60*1000 )==WAIT_TIMEOUT ){
			if( WaitForSingleObject( m_evWakeRouter, 30 )==WAIT_TIMEOUT ){
				pHostSet->DumpRunningHosts();
			}else{
				//WaitEvent( m_evWakeRouter );
				ClearEvent( m_evWakeRouter );
				break;
			}
			if( i==maxwait-2
		}
		if( i>=maxwait ){
			cdump<<lock<<"canceling running hosts..."<<endl<<unlock;
			pHostSet->CancelRunningHosts();
			Sleep( 5000 );
		}else{
			cdump<<lock<<"waiting wake up..."<<endl<<unlock;
		}*/

//		DumpPendingJobs(); 
	}
	time_t rawtime;
	time( &rawtime );
	struct tm* timeinfo;
	timeinfo = localtime( &rawtime );
	cdump<<lock<<endl<<asctime(timeinfo)<<"  all done!"<<endl<<unlock;
}

void* CJobSet::WorkThread( void* arg )
{
	//copy parameters.
	WORKTHREADPARAM* pParam = (WORKTHREADPARAM*)arg;
	CJobObject* pJobObj = pParam->m_pJobObj;
	CHostObject* pHostObj = pParam->m_pHostObj;
	CHostSet* pHostSet = pHostObj->GetHostSet();
	CJobSet* pJobSet = pJobObj->GetJobSet();

	//execute the job on the host
	pJobObj->m_tmStart = GetTime();
	bool bOk = pJobObj->Launch( pHostObj );
	pJobObj->m_tmStop = GetTime();

	//update the speed hint of the host object.
	if( bOk ){
		double rSpeedHint = ( pJobObj->m_tmStop - pJobObj->m_tmStart ) / pJobObj->m_rCpuHint;
		pHostObj->UpdateSpeedHint( rSpeedHint );
	}else{
		pJobObj->m_nFailCount++;
		if( pHostObj->m_pSession!=NULL ){
			if( pHostObj->GetSpeedHint()!=-1 ){
				pHostObj->SetSpeedHint( 1.5*pHostObj->GetSpeedHint() );
			}
		}else{
			pHostObj->SetSpeedHint( -1 );
		}
	}

			IN_ADDR addr = pHostObj->GetAddr();
			char* pname = GetHostByAddr( addr );
			char buf[256];
			sprintf( buf, "(%d)%s\tjob %d done:%d", pHostObj->m_nSpeedHint, pname, pJobObj->GetId(), bOk );
			//cout<<"("<<pHostObj->m_rSpeedHint<<")"<<phent->h_name<<"\tjob done:"<<pJobObj->GetId()<<endl;

	//return the host tocken back to the host set.
	pHostSet->ReturnTocken( pHostObj, bOk );

	bool bSignalMoreJob = false;
	bool bSignalLastJob = false;
	pJobSet->Lock();
	if( bOk ){	//the job is successful
		pJobSet->m_ZombieSet.Push( pJobObj );
	}else{		//the job is failed, return the Object to pending set to try it again.
		if( pJobSet->m_PendingSet.IsEmpty() ){
			bSignalMoreJob = true;
		}
		pJobSet->m_PendingSet.Push( pJobObj );
	}
	pJobSet->Unlock();

	int nRunningCounter = pJobSet->m_ThreadPool.LockedDec();

	bool bPendingJobs = !pJobSet->m_PendingSet.IsEmpty();

		//cdump<<lock<<"Job fin:"<<pJobObj->GetId()<<",\tthreads:"<<nRunningCounter<<",\tMorejobs:"<<bMoreJobs<<endl<<unlock;

//	if( nRunningCounter==0 && pJobSet->m_PendingSet.IsEmpty() )bSignal = true;
	if( nRunningCounter==0 && !bPendingJobs )bSignalLastJob = true;

		sprintf( buf, "%s,\tremaining threads:%d,\tMoreJobs:%d, signal_more:%d, signal_last:%d", buf, nRunningCounter, bPendingJobs, bSignalMoreJob, bSignalLastJob );
		cdump<<lock<<buf<<endl<<unlock;

	ASSERT( !(bSignalMoreJob && bSignalLastJob) );
	if( bSignalMoreJob ){
		SignalEvent( pJobSet->m_evMoreJob );
	}else if( bSignalLastJob ){
		SignalEvent( pJobSet->m_evLastJob );
	}
//	if( bSignal ){
//		SignalEvent( pJobSet->m_evWakeRouter );
//	}
	delete pParam;

	return NULL;
}

void CJobSet::DumpPendingJobs()
{
	Lock();

	CFifoSet<CJobObject*>::iterator iter = m_PendingSet.begin();

	while( iter!=m_PendingSet.end() ){
		CJobObject* pJobObj = *iter++;
		IN_ADDR addr = pJobObj->m_addrHost;
		char* pname = GetHostByAddr( addr );
		cdump<<lock<<"pending job :"<<pJobObj->GetId()<<"\t on "<<pname<<endl<<unlock;
	}

	Unlock();
}