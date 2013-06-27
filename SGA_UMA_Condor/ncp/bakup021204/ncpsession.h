#pragma once

#include "ncphead.h"
#include "ncpthread.h"
#include "treetmpl.h"
#include <list>

class CClientSession;
class CServerSession;
class CSessionLocker;
class CServerApp;

//ncp server state definition
#define NCPS_CLOSED			1
#define NCPS_ESTABLISHED	2

class CClientSession
{
private:
	SOCKET		m_sock;
	int			m_nFileMode;		//file open mode
protected:
	int DoLogOn( const char* strUser, const char* strPass );
	int DoLogOff();
	//file operations
	int DoPassive( short* pport );
	int DoPutFile( const char* strSrc, const char* strDst, bool bOverwrite );
	int DoGetFile( const char* strSrc, const char* strDst );
	int DoRemFile( const char* strName );
//	int DoList();
	int DoChMod( const char* strName, int nMode );
	int DoSoftLink( const char* strSrc, const char* strLink ); 
	int DoFileMode( int nFileMode );

	//directory operations
	int DoMkDir( const char* strDir );
	int DoChDir( const char* strDir );
	int DoRmDir( const char* strDir );

	//other operations
	int DoRunImage( const char* strImage, bool bSync=true );
	int DoGetSysInfo( SYSINFO* pSysInfo );
	int DoKillProcs( );

public:
	CClientSession( );
	~CClientSession();

	//connect and disconnect.
	int Connect( SOCKADDR_IN* srvAddr, const char* strUser, const char* strPass );
	int Disconnect();

	//file operation
	int PutFile( const char* strSrc, const char* strDst, bool bOverwrite=true );
	int GetFile( const char* strSrc, const char* strDst );
	int Remove( const char* strName );
	int ChMod( const char* strName, int nMode );
	int SoftLink( const char* strSrc, const char* strLink );	//create a link linking to strSrc.
	int FileMode( int nFileMode );

	//directory operation
	int ChDir( const char* strDir );
	int MkDir( const char* strDir );
	int RmDir( const char* strDir );

	//run executable.
	int RunImage( const char* strImage, bool bSync=true );
	//system information
	int GetSysInfo( SYSINFO* pSysInfo );
	//kill all waiting processes
	int KillProcs( );
};

class CServerSession
{
private:
	SOCKET		m_sock;
	SOCKET		m_datasock;
	int			m_state;
	int			m_nFileMode;
	string		m_strHomeDir;
	string		m_strCurDir;
	bool		m_bRootUsr;

	CServerApp*	m_pServApp;
	CSessionLocker* m_pLocker;
protected:
	bool IsRootUser();

	//logon and off
	bool OnLogOn( CNcpMessage* pMsg, int nBufSize );
	bool OnLogOff( CNcpMessage* pMsg, int nBufSize );

	//file operations
	bool OnPassive( CNcpMessage* pMsg, int nBufSize );
	bool OnPutFile( CNcpMessage* pMsg, int nBufSize );
	bool OnGetFile( CNcpMessage* pMsg, int nBufSize );
	bool OnRemFile( CNcpMessage* pMsg, int nBufSize );
	bool OnChMod( CNcpMessage* pMsg, int nBufSize );
	bool OnSoftLink( CNcpMessage* pMsg, int nBufSize );
	bool OnFileMode( CNcpMessage* pMsg, int nBufSize );

	//directory operations
	bool OnMkDir( CNcpMessage* pMsg, int nBufSize );
	bool OnChDir( CNcpMessage* pMsg, int nBufSize );
	bool OnRmDir( CNcpMessage* pMsg, int nBufSize );

	bool OnRunImage( CNcpMessage* pMsg, int nBufSize );
	bool OnGetSysInfo( CNcpMessage* pMsg, int nBufSize );
	bool OnKillProcs( CNcpMessage* pMsg, int nBufSize );

	bool PumpMessage( CNcpMessage* pMsg, int nBufSize );
public:
	CServerSession( CServerApp* pServApp );
	~CServerSession();

	void RunSession( SOCKET sock );
	void Close();
};

#define SYSTEM_IDLE					0
#define SYSTEM_USER_LOGON			0x01
#define SYSTEM_USER_LOCK			0x02
#define SYSTEM_START_SCREEN_SAVER	0x04

inline bool IsSystemFree( int nSysStatFlags )
{
	return IsClr(nSysStatFlags, SYSTEM_USER_LOGON) || IsSet(nSysStatFlags, SYSTEM_USER_LOCK) ||
		( IsSet(nSysStatFlags, SYSTEM_USER_LOGON) && IsSet(nSysStatFlags, SYSTEM_START_SCREEN_SAVER) );
}

class CSessionLocker
{
private:
	string m_strUser;
	HMUTEX m_hMutex;
	int m_nRef;
public:
	CSessionLocker( const char* strUser ){
		m_strUser = strUser;
		m_hMutex = CreateMutex();
		m_nRef = 0;
	}
	~CSessionLocker(){
		DestroyMutex( m_hMutex );
	}

	void Lock(){
		LockMutex( m_hMutex );
	}
	void Unlock(){
		UnlockMutex( m_hMutex );
	}
	int GetRef(){
		return m_nRef;
	}
	void AddRef(){
		m_nRef++;
	}
	int ReleaseRef()
	{
		m_nRef--;
		return m_nRef;
	}
	int Compare( const CSessionLocker* pLocker )const{
		return strcmp(m_strUser.c_str(), pLocker->m_strUser.c_str());
	}
	int Compare( const char* strUser )const{
		return strcmp( m_strUser.c_str(), strUser );
	}
};

class CSessionLockerTraits : public CElementTraits<CSessionLocker*, const char*>
{
public:
#ifdef _MSC_VER
	static int CompareElements( const CSessionLocker*& pItem1, const CSessionLocker*& pItem2 )
#else
	static int CompareElements( CSessionLocker*& pItem1, CSessionLocker*& pItem2 )
#endif
	{
		return pItem1->Compare( pItem2 );
	}
#ifdef _MSC_VER
	static int CompareToKey( const CSessionLocker*& pItem, const char* strUser )
#else
	static int CompareToKey( CSessionLocker*& pItem, const char* strUser )
#endif
	{
		return pItem->Compare( strUser );
	}
};

typedef CAvlTree<CSessionLocker*, CSessionLocker*, const char*, CSessionLockerTraits> CAvlSessionLocks;

class CServerApp
{
private:
	CAvlSessionLocks m_avlSessionLocks;
	list<HPROCESS> m_lstProcHandles;
	HMUTEX m_hMutex;
	int m_nStateFlags;
protected:
	void Lock(){
		LockMutex( m_hMutex );
	}
	void Unlock(){
		UnlockMutex( m_hMutex );
	}
	void SetPriority( int nStateFlags );			//set the running processes priority
	int State2Priority(int nStateFlags );			//compute priorit class by stateflags			
public:
	CServerApp(){
		m_hMutex = CreateMutex();
		m_nStateFlags = SYSTEM_IDLE;				//sys free.
	}
	~CServerApp(){
		DestroyMutex( m_hMutex );
	}

	CSessionLocker* GetSessionLocker( const char* strUser );
	void ReleaseSessionLocker( CSessionLocker* pLocker );

	void RegistProc( HPROCESS hProc );
	void UnregistProc( HPROCESS hProc );

	void SetServState( int nStateFlags );	//set the server state flags.
	int GetServState();						//get the server state flags.
	int GetPriority();						//get the default process running priority

	void KillProcs();			//kill all the running processes

	int GetSessionCount();
	int GetProcCount();

	void RunServer();
	static int SessionThread( void* arg );
	static int PingServThread( void* arg );
};