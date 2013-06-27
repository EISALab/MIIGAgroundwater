#pragma once

#include <list>

#include <treetmpl.h>
#include "netbase.h"
#include "ncphead.h"
#include "ncpsession.h"
#include "uwkelobj.h"

using namespace std;

class CHostObject;
class CHostSet;

class CHostObject
{
public:
	IN_ADDR			m_addrHost;		//the host IP address.
	CClientSession*	m_pSession;		//the buffered ncp session
	unsigned long	m_nSpeedHint;	//the speed hint to order the host objects, millisecnods

	CHostSet*		m_pHostSet;		//the container host set.
public:
	CHostObject();
	CHostObject( IN_ADDR addrHost, CHostSet* pHostSet );
	~CHostObject();

	IN_ADDR GetAddr(){ return m_addrHost;	}
	CClientSession* GetSession(){ return m_pSession; }
	CHostSet* GetHostSet(){	return m_pHostSet;	}
	void SetHostSet( CHostSet* pHostSet ){ m_pHostSet = pHostSet; }
	void SetSession( CClientSession* pSession ){ m_pSession = pSession; }
	void SetSpeedHint( unsigned long nSpeedHint ){ m_nSpeedHint = nSpeedHint; }
	unsigned long GetSpeedHint(){ return m_nSpeedHint; }
	void UpdateSpeedHint( unsigned long nSpeedHint );

	//comparison by host ip
	int CompareHost( const CHostObject* pObj )const{
		return m_addrHost.S_un.S_addr==pObj->m_addrHost.S_un.S_addr ? 0 : 
				(m_addrHost.S_un.S_addr<pObj->m_addrHost.S_un.S_addr ? -1 : 1);
	}
	int CompareHost( const IN_ADDR hostip )const{
		return m_addrHost.S_un.S_addr==hostip.S_un.S_addr ? 0 : 
				(m_addrHost.S_un.S_addr<hostip.S_un.S_addr ? -1 : 1);
	}

	//comparison by host speed.
	int CompareSpeed( const CHostObject* pObj )const{
		//avoid overflow of the -1.
		return m_nSpeedHint==pObj->m_nSpeedHint ? 0 : 
				(m_nSpeedHint<pObj->m_nSpeedHint ? -1 : 1);
		//return m_nSpeedHint - pObj->m_nSpeedHint;
	}
	int CompareSpeed( unsigned long nSpeedHint )const {
		return m_nSpeedHint==nSpeedHint ? 0 : 
				(m_nSpeedHint<nSpeedHint ? -1 : 1);
		//return m_nSpeedHint - nSpeedHint;
	}
};

class CHostObjectTraits : public CElementTraits<CHostObject*, IN_ADDR>
{
public:
	static int CompareElements( const CHostObject*& pItem1, const CHostObject*& pItem2 )
	{
		return pItem1->CompareHost( pItem2 );
	}
	static int CompareToKey( const CHostObject*& pItem, IN_ADDR hostip )
	{
		return pItem->CompareHost( hostip );
	}
};

class CHostObjectSpeedTraits : public CElementTraits<CHostObject*, unsigned long>
{
public:
	static int CompareElements( const CHostObject*& pItem1, const CHostObject*& pItem2 )
	{
		return pItem1->CompareSpeed( pItem2 );
	}
	static int CompareToKey( const CHostObject*& pItem, unsigned long nSpeedHint )
	{
		return pItem->CompareSpeed( nSpeedHint );
	}
};

typedef CAvlTree<CHostObject*, CHostObject*, IN_ADDR, CHostObjectTraits> CAvlHostTree;
typedef CAvlTree<CHostObject*, CHostObject*, unsigned long, CHostObjectSpeedTraits> CAvlHostSpeedTree;

class CHostSet
{
private:
	CAvlHostTree		m_trHosts;				//all the hosts.
	CAvlHostSpeedTree	m_trAliveHosts;			//alive hosts avaliable for allocation
	list<CHostObject*>	m_lsRunningHosts;		//running hosts
	CAvlHostTree		m_trDeadHosts;			//dead hosts, maybe rebooting.

	HANDLE				m_mxLock;				//lock mutex
	HANDLE				m_smAlives;				//semaphore for the alive hosts

	HANDLE				m_hListenThread;		//listening thread to ping the dead hosts.
	char*				m_pUser;
	char*				m_pPassword;

	bool				m_bUseLocalHost;		//if use the local host server.
public:
	CHostSet();
	~CHostSet();

	void Lock();
	void Unlock();

	char* GetUser(){ return m_pUser; }
	char* GetPassword(){ return m_pPassword; }
	void SetUserPass( char* strUser, char* strPassword );
	virtual int CreateSession( CHostObject* pHostObj, CClientSession** ppSession );

	bool Add( IN_ADDR addrHost, bool bIsAlive=true );
	void ParseHostName( const char* strHostName );
	void LoadFromFile( const char* strFile );

	void SetUseLocalHost( bool bUse=false );
	bool IsUseLocalHost();

	//start a listen thread to ping the dead hosts and register the alive hosts
	void Listen();

	void BroadcastPing();		//broadcast a ping to collect servers.

	//retrieve a host tocken to run jobs
	CHostObject* RetrieveTocken();				//blocked if no available alive hosts
	//return the finished host tocken back to set
	void ReturnTocken( CHostObject* pHostObj, bool bAlive=true );

	static void* ListenThread( void * );

	void DumpRunningHosts();
	void DumpAllHosts();
	void CancelRunningHosts( int nMaxHosts );
	int GetAliveHostCount();
//	unsigned long GetPessimalHint();
};