#include "ncphead.h"
#include "ncphost.h"
#include "ncpsession.h"
#include "netstd.h"
#include <fstream>
#include "ofdump.h"

#define MAXLINE	256

CHostObject::CHostObject()
{
	bzero( &m_addrHost, sizeof(IN_ADDR) );
	m_pSession = NULL;
	m_nSpeedHint = -1;
	m_pHostSet = NULL;
}

CHostObject::CHostObject( IN_ADDR addrHost, CHostSet* pHostSet )
{
	bcopy( &addrHost, &m_addrHost, sizeof(IN_ADDR) );
	m_nSpeedHint = -1;
	m_pHostSet = pHostSet;
	m_pSession = NULL;
}

CHostObject::~CHostObject()
{
	IN_ADDR addr = GetAddr();
	char* pname = GetHostByAddr( addr );
	if( m_pSession ){
		try{
			cdump<<lock<<pname<<"\tdisconnecting..."<<endl<<unlock;
			m_pSession->Disconnect();
		}catch( CSockException* e ){
			printf( "error disconnecting...\n" );
			e->Delete();
		}
		delete m_pSession;
		m_pSession = NULL;
	}else{
		cdump<<lock<<pname<<"\t session pointer NULL"<<endl<<unlock;
	}
}

void CHostObject::UpdateSpeedHint( unsigned long nSpeedHint )
{
	if( m_nSpeedHint==-1 ){
		m_nSpeedHint = nSpeedHint;
	}else{
		double alpha = 0.8;
		m_nSpeedHint = (unsigned long)(alpha*m_nSpeedHint + (1-alpha)*nSpeedHint);
	}
}


///////////////////////////////////////////////////////////////////////////////////////////////////////////
//											CJobSet
///////////////////////////////////////////////////////////////////////////////////////////////////////////

//a helper function to free the AVL tree
BOOL FreeHostObject( CHostObject*& pItem, LPVOID)
{
	delete pItem;
	return TRUE;
}

//a helper function to free the AVL tree
BOOL PingHostObject( CHostObject*& pItem, LPVOID pParam)
{
	SOCKET sock = (SOCKET)pParam;

	SOCKADDR_IN addr;
	addr.sin_family = AF_INET;
	addr.sin_addr = pItem->m_addrHost;
	addr.sin_port = htons( NCP_SERVER_PING_PORT );

	int nCode = htonl( NCP_PING_SERVER );
	sendto( sock, (char*)&nCode, sizeof(int), 0, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) );

	return TRUE;
}


CHostSet::CHostSet()
{
	m_mxLock = CreateMutex();
	m_smAlives = CreateSemaphore( 0 );
	m_pUser = NULL;
	m_pPassword = NULL;
	m_bUseLocalHost = false;
}

CHostSet::~CHostSet()
{
	//send the listen thread quit message
	SOCKET sock = Socket( PF_INET, SOCK_DGRAM, 0 );

	char buf[MAXLINE];
	if( gethostname( buf, ELEMENTS(buf) ) == 0 ){
		HANDLE hListenThread = m_hListenThread;

		//send quit message to the monitor thread
		struct hostent* phent = gethostbyname( buf );
		SOCKADDR_IN addr;
		addr.sin_family = AF_INET;
		bcopy( phent->h_addr, &addr.sin_addr, sizeof(IN_ADDR) );
		addr.sin_port = htons( NCP_HOST_REGIST_PORT );

		int nCode = htonl( NCP_REGISTER_EXIT );
		sendto( sock, (char*)&nCode, sizeof(int), 0, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) );

		//synchronize the listen thread
		WaitForThread( hListenThread );
		CloseHandle( hListenThread );
	}
	closesocket( sock );

	//free the all the host objects
	Lock();
	m_trHosts.VisitTree( FreeHostObject );
	Unlock();

	CloseHandle( m_mxLock );
	CloseHandle( m_smAlives );

	if( m_pUser )delete m_pUser;
	if( m_pPassword )delete m_pPassword;
}

void CHostSet::Lock()
{
//	printf( "locking hostset,\t" );
	LockMutex( m_mxLock );
}

void CHostSet::Unlock()
{
//	printf( "unlocking hostset\n" );
	UnlockMutex( m_mxLock );
}

void CHostSet::SetUserPass( char* strUser, char* strPassword )
{
	//free memory.
	if( m_pUser )delete m_pUser;
	if( m_pPassword )delete m_pPassword;
	m_pUser = NULL;
	m_pPassword = NULL;

	//set new user and pass.
	if( strUser ){
		m_pUser = new char[ strlen(strUser)+1 ];
		strcpy( m_pUser, strUser );
	}
	if( strPassword ){
		m_pPassword = new char[ strlen(strPassword)+1 ];
		strcpy( m_pPassword, strPassword );
	}
}

int CHostSet::CreateSession( CHostObject* pHostObj, CClientSession** ppSession )
{
	*ppSession = NULL;

	SOCKADDR_IN sockaddr;
	sockaddr.sin_family = AF_INET;
	sockaddr.sin_addr = pHostObj->GetAddr();
	sockaddr.sin_port = htons( NCP_SERVER_PORT );

	CClientSession* pSession = new CClientSession();
	int nRet;
	try{
		nRet = pSession->Connect( &sockaddr, m_pUser, m_pPassword );
		if( nRet==0 ){
			*ppSession = pSession;
		}
	}catch( CSockException* e){
		e->Delete();
		delete pSession;
		*ppSession = NULL;
		return E_NOHOST;
	}


	return nRet;
}

bool CHostSet::Add( IN_ADDR addrHost, bool bIsAlive )
{
	char buf[MAXLINE];
	//get local host address
	gethostname( buf, ELEMENTS(buf) );
	hostent* phent = gethostbyname( buf );
	IN_ADDR addrloc;
	bcopy( phent->h_addr, &addrloc, sizeof(IN_ADDR) );

	//force not using local host. avoid treat local host as a computation node
	if( !IsUseLocalHost() ){
		if( addrloc.S_un.S_addr==addrHost.S_un.S_addr )return false;
	}

//	CHostObject* pHostObj = new CHostObject( addrHost, this );
//	if( m_trHosts.Search( pHostObj )==NULL ){
	if( m_trHosts.SearchByKey( addrHost )==NULL ){
		CHostObject* pHostObj = new CHostObject( addrHost, this );
		m_trHosts.Insert( pHostObj );

		if( bIsAlive ){
			m_trAliveHosts.Insert( pHostObj );
			ReleaseSemaphore( m_smAlives );
		}else{
			m_trDeadHosts.Insert( pHostObj );
		}
		return true;
	}

//	delete pHostObj;
	return false;
}

void CHostSet::ParseHostName( const char* strHostName )
{
	char buf[MAXLINE];
	strcpy( buf, strHostName );

	char* pl = strchr( buf, '[' );
	char* pm = NULL;
	if( pl )pm = strchr( pl, '-' );
	char* pr = NULL;
	if( pm )pr = strchr( buf, ']' );
	if( pl && pm && pr && pm>pl && pr>pm ){
		int nx = atoi( pl+1 );
		int ny = atoi( pm+1 );
		if( nx>ny )swap(nx,ny);

		*pl = '\0';
		pl = buf;	//pl left part
		pr = pr+1;	//pr right part

		//concatate the name.
		char bufname[MAXLINE];
		for( int i=nx; i<=ny; i++ ){
			sprintf( bufname, "%s%d%s", pl, i, pr );
			hostent* phent = gethostbyname(bufname);
			if( phent!=NULL ){
				IN_ADDR addr;
				bcopy( phent->h_addr, &addr, sizeof(IN_ADDR) );
				Add( addr, true );
			}
		}
	}else{
		//can't parse the host name, add it directly
		hostent* phent = gethostbyname(strHostName);
		if( phent!=NULL ){
			IN_ADDR addr;
			bcopy( phent->h_addr, &addr, sizeof(IN_ADDR) );
			Add( addr, true );
		}
	}
}


void CHostSet::LoadFromFile( const char* strFile )
{
	ifstream ifg(strFile);
	char buf[MAXLINE];

	while( !ifg.eof() && !ifg.fail() ){
		ifg.getline(buf, ELEMENTS(buf));
		if( strncmp(buf, "//", 2)==0 )continue;
		ParseHostName( buf );
//		hostent* phent = gethostbyname( buf );
//		if( phent!=NULL ){
//			IN_ADDR addr;
//			bcopy( phent->h_addr, &addr, sizeof(IN_ADDR) );
//			Add( addr, true );
//		}
	}
}

bool CHostSet::IsUseLocalHost()
{
	return m_bUseLocalHost;
}

void CHostSet::SetUseLocalHost( bool bUse )
{
	m_bUseLocalHost = bUse;
}

void CHostSet::Listen()
{
	m_hListenThread = CreateThread( (THREAD_ROUTINE)&ListenThread, this, 0 );
}

//blocked if no available alive hosts
CHostObject* CHostSet::RetrieveTocken()
{
	WaitForSemaphore( m_smAlives );

	Lock();
	CHostObject* pHostObj = m_trAliveHosts.RemoveHead();	//always return the fastest computer
//	m_trRunningHosts.Insert( pHostObj );					//buffer it into the running set.
	m_lsRunningHosts.push_back( pHostObj );					//buffer it into the running list
	Unlock();

	return pHostObj;
}

void CHostSet::ReturnTocken( CHostObject* pHostObj, bool bAlive )
{
	Lock();
	//POSITION pos = m_trRunningHosts.Search( pHostObj );
	//ASSERT( pos!=NULL );
	//m_trRunningHosts.RemoveAt( pos );					//remove the host from the runing set.
	m_lsRunningHosts.remove( pHostObj );			//remove it from the running list

	if( bAlive ){	//put it into the alive tree if alive, otherwise put it into dead list.
		m_trAliveHosts.Insert( pHostObj );
		ReleaseSemaphore( m_smAlives );
	}else{
		m_trDeadHosts.Insert( pHostObj );
	}
	Unlock();
}

void CHostSet::BroadcastPing()
{
	char buf[MAXLINE];
	int *pcode = (int*)buf;

	SOCKET sockbroad = Socket( PF_INET, SOCK_DGRAM, 0 );
	//enable broadcast
	BOOL bVal = TRUE;
	setsockopt( sockbroad, SOL_SOCKET, SO_BROADCAST, (char*)&bVal, sizeof(BOOL) );

	//broadcast the server register message to local net at startup.
	//create the broadcast address
	SOCKADDR_IN dstaddr;
	dstaddr.sin_family = AF_INET;
	dstaddr.sin_port = htons( NCP_SERVER_PING_PORT );
	dstaddr.sin_addr.s_addr = INADDR_BROADCAST;

	//send the broadcast register message.
	*pcode = htonl( NCP_PING_SERVER );
	int nLen = sendto( sockbroad, buf, sizeof(int), 0, (SOCKADDR*)&dstaddr, sizeof(SOCKADDR_IN) );
	closesocket( sockbroad );
}

void* CHostSet::ListenThread( void * arg )
{
	char buf[MAXLINE];
	int *pcode = (int*)buf;
	CHostSet* pHostSet = (CHostSet*)arg;

	SOCKET socksrv = Socket( PF_INET, SOCK_DGRAM, 0 );

	//bind to specific port.
	SOCKADDR_IN srvaddr;
	srvaddr.sin_family = AF_INET;
	srvaddr.sin_port = htons( NCP_HOST_REGIST_PORT );
	srvaddr.sin_addr.s_addr = INADDR_ANY;
	Bind( socksrv, (SOCKADDR*)&srvaddr, sizeof(SOCKADDR_IN) );

	//the socket to ping the dead hosts.
	SOCKET sockpin = Socket( PF_INET, SOCK_DGRAM, 0 );

	fd_set rset;
	FD_ZERO( &rset );

//	pHostSet->BroadcastPing();

	while( true ){
		FD_SET( socksrv, &rset );
		int maxfd = (int)socksrv + 1;

		//the time out is one second
		timeval timeout;
		timeout.tv_sec = 1;
		timeout.tv_usec = 0;
		int nRet = Select( maxfd, &rset, NULL, NULL, &timeout );

		if( nRet==0 ){		//time expired, ping the dead hosts, hope they will be up.
			pHostSet->Lock();
			pHostSet->m_trDeadHosts.VisitTree( PingHostObject, (LPVOID)sockpin );
			pHostSet->Unlock();

		}else{				//receive a register message from a remote host
			SOCKADDR_IN srcaddr;
			int addrlen = sizeof(SOCKADDR_IN);
			int nLen = recvfrom( socksrv, buf, ELEMENTS(buf), 0, (SOCKADDR*)&srcaddr, &addrlen );

			//receive a host udp message.
			if( nLen>=sizeof(int) ){
				(*pcode) = ntohl( *pcode );
				if( *pcode==NCP_HOST_REGIST ){	// a host is registering its host information
					int nServState = ntohl( *(pcode+1) );	//the second int is the speed hint

					pHostSet->Lock();
					//check if the host is in the set already
					POSITION pos = pHostSet->m_trHosts.SearchByKey( srcaddr.sin_addr );
					if( pos!=NULL && IsClr(nServState, SYSTEM_SUSPEND_SERVICE) ){
						//if the host is in the host set, and the host is now alive
						pos = pHostSet->m_trDeadHosts.SearchByKey( srcaddr.sin_addr );
						if( pos!=NULL ){	//check if the host is dead
							//move the host from the dead tree to alive tree.
							CHostObject* pHostObj = pHostSet->m_trDeadHosts.GetAt( pos );
							pHostSet->m_trDeadHosts.RemoveAt( pos );
							pHostSet->m_trAliveHosts.Insert( pHostObj );
							ReleaseSemaphore( pHostSet->m_smAlives );
						}
					}else if( pos==NULL ){	//the host is not in the host set. set it.
						//the host is not in the host set, insert it
						pHostSet->Add( srcaddr.sin_addr, IsClr(nServState, SYSTEM_SUSPEND_SERVICE) );
					}
					pHostSet->Unlock();
				}else if( *pcode==NCP_REGISTER_EXIT ){	//asking for exit by the destructor.
					break;
				}
			}
		}//end of if( nRet==0 );
	}//end of while

	closesocket( sockpin );
	closesocket( socksrv );
	return NULL;
}

void CHostSet::DumpRunningHosts()
{
	Lock();
//	POSITION pos = m_trRunningHosts.GetHeadPosition();
//	while( pos ){
//		CHostObject* pHostObj = m_trRunningHosts.GetNext( pos );
//		IN_ADDR addr = pHostObj->GetAddr();
//		struct hostent * phent = GetHostByAddr( (char*)&addr, sizeof(IN_ADDR), AF_INET );
//		cdump<<lock<<"running job on "<<phent->h_name<<endl<<unlock;
//	}

	list<CHostObject*>::iterator iter = m_lsRunningHosts.begin();
	while( iter!=m_lsRunningHosts.end() ){
		CHostObject* pHostObj = *iter++;
		IN_ADDR addr = pHostObj->GetAddr();
		char* pname = GetHostByAddr( addr );
		cdump<<lock<<"running job on "<<pname<<endl<<unlock;
	}
	Unlock();
}

void CHostSet::DumpAllHosts()
{
	Lock();
	cdump<<lock<<"dumping alive hosts:"<<endl;

	POSITION pos = m_trAliveHosts.GetHeadPosition();
	while( pos ){
		CHostObject* pHostObj = m_trAliveHosts.GetNext( pos );
		IN_ADDR addr = pHostObj->GetAddr();
		char* pname = GetHostByAddr( addr );
		cdump<<"\t("<<pHostObj->GetSpeedHint()<<")"<<pname<<endl;
	}


	cdump<<"dumping running hosts:"<<endl;
//	pos = m_trRunningHosts.GetHeadPosition();
//	while( pos ){
//		CHostObject* pHostObj = m_trRunningHosts.GetNext( pos );
//		IN_ADDR addr = pHostObj->GetAddr();
//		struct hostent * phent = GetHostByAddr( (char*)&addr, sizeof(IN_ADDR), AF_INET );
//		cdump<<"\t("<<pHostObj->GetSpeedHint()<<")"<<phent->h_name<<endl;
//	}

	list<CHostObject*>::iterator iter = m_lsRunningHosts.begin();
	while( iter!=m_lsRunningHosts.end() ){
		CHostObject* pHostObj = *iter++;
		IN_ADDR addr = pHostObj->GetAddr();
		char* pname = GetHostByAddr( addr );
		cdump<<"\t("<<pHostObj->GetSpeedHint()<<")"<<pname<<endl;
	}

	cdump<<"dumping dead hosts:"<<endl;
	pos = m_trDeadHosts.GetHeadPosition();
	while( pos ){
		CHostObject* pHostObj = m_trDeadHosts.GetNext( pos );
		IN_ADDR addr = pHostObj->GetAddr();
		char* pname = GetHostByAddr( addr );
		cdump<<"\t("<<pHostObj->GetSpeedHint()<<")"<<pname<<endl;
	}

	cdump<<unlock;

	Unlock();
}


void CHostSet::CancelRunningHosts( int nMaxHosts )
{
	Lock();

	//sort the running hosts in trRunningHosts.
	CAvlHostSpeedTree	trRunningHosts;	//running hosts ordered by their speed
	list<CHostObject*>::iterator iter = m_lsRunningHosts.begin();
	while( iter!=m_lsRunningHosts.end() ){
		CHostObject* pHostObj = *iter++;
		trRunningHosts.Insert( pHostObj );
	}

	POSITION posRun = trRunningHosts.GetTailPosition();
	POSITION posAlv = m_trAliveHosts.GetHeadPosition();
	//list<CHostObject*>::iterator iter = m_lsRunningHosts.begin();
//	int nCanceled = 0;
//	while( iter!=m_lsRunningHosts.end() && nCanceled<nMaxHosts ){
//		CHostObject* pHostObj = *iter++;
//	while( posRun && posAlv && nCanceled<nMaxHosts ){
	while( posRun && posAlv ){
		CHostObject* pHostObj = trRunningHosts.GetPrev( posRun );		//this is the canceled host
		CHostObject* pHostObjAlv = m_trAliveHosts.GetNext( posAlv );	//this is the preempting host
		bool bCancelIt = false;
		if( pHostObjAlv->GetSpeedHint()==-1 ){
			bCancelIt = (double)rand() / RAND_MAX < 0.3;
		}else if( 2*pHostObjAlv->GetSpeedHint()<pHostObj->GetSpeedHint() ){
			bCancelIt = true;
		}
		//do the preemption.
		if( bCancelIt && (pHostObj->m_pSession!=NULL) ){
			IN_ADDR addr = pHostObj->GetAddr();
			char* pname = GetHostByAddr( addr );
			cdump<<lock<<"killing host connection "<<pname<<endl<<unlock;
			pHostObj->m_pSession->KillConnect();
			cdump<<lock<<pname<<" killed"<<endl<<unlock;

			//kill the running processes
			SOCKADDR_IN sockaddr;
			sockaddr.sin_family = AF_INET;
			sockaddr.sin_addr = pHostObj->GetAddr();
			sockaddr.sin_port = htons( NCP_SERVER_PORT );

			CClientSession* pSession = new CClientSession();
			int nRet;
			try{
				cdump<<lock<<"killing processes on "<<pname<<endl<<unlock;
				nRet = pSession->Connect( &sockaddr, "root", "" );
				if( nRet==0 ){
					pSession->KillProcs();
					pSession->Disconnect();
				}
				cdump<<lock<<"processes killed on "<<pname<<endl<<unlock;
			}catch( CSockException* e){
				e->Delete();
			}
			delete pSession;

//			nCanceled++;
		}
	}
	Unlock();
}

int CHostSet::GetAliveHostCount()
{
	Lock();
	int nRet = m_trAliveHosts.GetCount();
	Unlock();
	return nRet;
}
