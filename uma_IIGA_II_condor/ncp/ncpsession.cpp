//#include "stdafx.h"
#include "netstd.h"
#include "ncphead.h"
#include "uwkelobj.h"
#include "ncpsession.h"
#include <strstream>

#define MAXFILEBUF		0x8000
#define MAXCMDBUF		512
#define MAXPATH			256
#define MAXLINE			256
#define MAXRETRY		5

extern FILE* pfout;

//log on and log off operations
int CClientSession::DoLogOn( const char* strUser, const char* strPass )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_LOGON, m_nFileMode );
	strcpy( pMsg->data, strUser );
	strcpy( pMsg->data+strlen(strUser)+1, strPass );
	pMsg->SetDataSize( strlen(strUser)+strlen(strPass)+2 );
	pMsg->Send( m_sock );

	//wait for the return code
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoLogOff()
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_LOGOFF );
	pMsg->Send( m_sock );

	//wait for the return code
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

//passive send CM_PASSIVE command, the server should return the passive data channel address
int CClientSession::DoPassive( short* pport )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_PASSIVE );
	pMsg->Send( m_sock );

	//wait for the return code
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsFailed() )return pMsg->GetRet();

	ASSERT( pMsg->GetSize()==sizeof(CNcpMessage)+sizeof(short) );

    //the data area is the port number in network bytes
	*pport = ntohs( *(short*)( pMsg->GetData() ) );
	
	return S_OK;
}

int CClientSession::DoPutFile( const char* strSrc, const char* strDst, bool bOverwrite )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	if( !IsFileExist( strSrc ) ){
		return E_NOENT;
	}

	//step 1. request passive mode to get the data channel address
	short dataport = 0;

	int nRet = DoPassive( &dataport );
	if( FAILED(nRet) )return nRet;

	//step 2. send the put file command.
	pMsg->Init( CM_PUTFILE );
	pMsg->m_nParamRet = bOverwrite;
	strcpy( pMsg->GetData(), strDst );
	pMsg->SetDataSize( strlen(strDst)+1 );

	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsFailed() || pMsg->m_nParamRet==S_EXIST ){
		return pMsg->GetRet();
	}

	//step 3. now the server agrees on the file transfer, connect the data channel and send file
	SOCKADDR_IN addr;
	socklen_t nlen = sizeof(SOCKADDR_IN);
	GetPeerName( m_sock, (SOCKADDR*)&addr, &nlen );
	addr.sin_port = htons( dataport );

	SOCKET sockdata;
	//import, must retry the socket initilization a few times.
	int i;
	for( i=0; i<MAXRETRY; i++ ){
		sockdata = Socket( PF_INET, SOCK_STREAM, 0 );
		ASSERT( sockdata!=INVALID_SOCKET );

		if( ::connect( sockdata, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) )==0 )break;
		closesocket( sockdata );
	}
	if( i>=MAXRETRY )throw new CSockException();

	int nLen = SendFileEx( sockdata, strSrc, m_nFileMode );
	closesocket( sockdata );

	//step 4. exchange the error code.
	pMsg->Init( CM_ACK, nLen );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoGetFile( const char* strSrc, const char* strDst )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	//step 1. request passive mode to get the data channel address
	short dataport=0;

	int nRet = DoPassive( &dataport );
	if( FAILED(nRet) )return nRet;

	//step 2. send the put file command.
	pMsg->Init( CM_GETFILE );
	strcpy( pMsg->GetData(), strSrc );
	pMsg->SetDataSize( strlen(strSrc)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsFailed() ){
		return pMsg->GetRet();
	}

	//step 3. now the server agrees on the file transfer, connect the data channel and send file
	SOCKADDR_IN addr;
	socklen_t nlen = sizeof(SOCKADDR_IN);
	GetPeerName( m_sock, (SOCKADDR*)&addr, &nlen );
	addr.sin_port = htons( dataport );

	SOCKET sockdata;
	//import, must retry the socket initilization a few times. 
	int i;
	for( i=0; i<MAXRETRY; i++ ){
		sockdata = Socket( PF_INET, SOCK_STREAM, 0 );
		ASSERT( sockdata!=INVALID_SOCKET );

		if( ::connect( sockdata, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) )==0 )break;
		closesocket( sockdata );
	}
	if( i>=MAXRETRY )throw new CSockException();

	int nLen = RecvFileEx( sockdata, strDst, m_nFileMode );
	closesocket( sockdata );

	//step 4. exchange the error code.
	pMsg->Init( CM_ACK, nLen );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoRemFile( const char* strName )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_REMFILE );
	strcpy( pMsg->GetData(), strName );
	pMsg->SetDataSize( strlen(strName)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

//chmod of a remote file, pMsg->m_nParamRet is the nMode parameters, pMsg->data is the file name
int CClientSession::DoChMod( const char* strName, int nMode )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_CHMOD, nMode );
	strcpy( pMsg->GetData(), strName );
	pMsg->SetDataSize( strlen(strName)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoSoftLink( const char* strSrc, const char* strLink )
{
	char buf[2*MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_SOFTLINK );
	strcpy( pMsg->data, strSrc );
	strcpy( pMsg->data+strlen(strSrc)+1, strLink );
	pMsg->SetDataSize( strlen(strSrc)+strlen(strLink)+2 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoFileMode( int nFileMode )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_FILEMODE, nFileMode );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsSucceeded() ){
		m_nFileMode = nFileMode;
	}
	return pMsg->GetRet();
}

int CClientSession::DoMkDir( const char* strDir )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_MKDIR );
	strcpy( pMsg->GetData(), strDir );
	pMsg->SetDataSize( strlen(strDir)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoChDir( const char* strDir )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_CHDIR );
	strcpy( pMsg->GetData(), strDir );
	pMsg->SetDataSize( strlen(strDir)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoRmDir( const char* strDir )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_RMDIR );
	strcpy( pMsg->GetData(), strDir );
	pMsg->SetDataSize( strlen(strDir)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoRunImage( const char* strImage, bool bSync )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_RUNIMAGE );

	//bSync is in the nParamRet
	pMsg->m_nParamRet = (int)bSync;

	//then is the image name.
	strcpy( pMsg->GetData(), strImage );

	pMsg->SetDataSize( strlen(strImage)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoGetSysInfo( SYSINFO* pSysInfo )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_GETSYSINFO );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsSucceeded() ){
		bcopy( pMsg->GetData(), pSysInfo, sizeof(SYSINFO) );
		sysinfo_ntoh( pSysInfo );
	}

	return pMsg->GetRet();
}

int CClientSession::DoKillProcs( )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_KILLPROCS );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoSetIdle( BOOL bIdle )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_SETIDLE, bIdle );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

//publiced user functions .............................................................
CClientSession::CClientSession( )
{
	m_sock = 0;
	m_nFileMode = O_BINARY;
}

CClientSession::~CClientSession()
{
	if( m_sock )closesocket( m_sock );
}

int CClientSession::Connect( SOCKADDR_IN* srvAddr, const char* strUser, const char* strPass )
{
	ASSERT( m_sock==0 );
	int nRet = 0;

	//initialize socket.
	m_sock = Socket( PF_INET, SOCK_STREAM, 0 );

	//make the connection.
	nRet = ::Connect( m_sock, (SOCKADDR*)srvAddr, sizeof(SOCKADDR_IN) );

	if( nRet!=0 )return E_NOHOST;

	return DoLogOn( strUser, strPass );
}

int CClientSession::Disconnect( )
{
	ASSERT( m_sock!=0 );

	int nRet = DoLogOff();
	closesocket( m_sock );
	m_sock = 0;

	return nRet;
}

void CClientSession::KillConnect()
{
	if( m_sock!=0 )closesocket( m_sock );
	m_sock = 0;
}


//file operation
int CClientSession::PutFile( const char* strSrc, const char* strDst, bool bOverwrite )
{
	return DoPutFile( strSrc, strDst, bOverwrite );
}

int CClientSession::GetFile( const char* strSrc, const char* strDst )
{
	return DoGetFile( strSrc, strDst );
}

int CClientSession::Remove( const char* strName )
{
	return DoRemFile( strName );
}

//create a link linking to strSrc.
int CClientSession::SoftLink( const char* strSrc, const char* strLink )
{
	return DoSoftLink( strSrc, strLink );
}

int CClientSession::FileMode( int nFileMode )
{
	if( m_nFileMode!=nFileMode ){
		return DoFileMode( nFileMode );
	}
	return S_OK;
}

int CClientSession::ChMod( const char* strName, int nMode )
{
	return DoChMod( strName, nMode );
}

//directory operation
int CClientSession::ChDir( const char* strDir )
{
	return DoChDir( strDir );
}

int CClientSession::MkDir( const char* strDir )
{
	return DoMkDir( strDir );
}

int CClientSession::RmDir( const char* strDir )
{
	return DoRmDir( strDir );
}

//run executable.
int CClientSession::RunImage( const char* strImage, bool bSync )
{
	return DoRunImage( strImage, bSync );
}


//system information
int CClientSession::GetSysInfo( SYSINFO* pSysInfo )
{
	return DoGetSysInfo( pSysInfo );
}

int CClientSession::KillProcs()
{
	return DoKillProcs();
}

int CClientSession::SetIdle( BOOL bIdle )
{
	return DoSetIdle( bIdle );
}

//***************************************************************************************//
bool CServerSession::IsRootUser()
{
	return m_bRootUsr;
}

void CServerSession::Close()
{
	if( m_datasock ){
		closesocket( m_datasock );
		m_datasock = 0;
	}

	if( m_sock ){
		closesocket( m_sock );
		m_sock = 0;
	}

	//switch state to closed
	m_state = NCPS_CLOSED;

	//release the session locker.
	m_pLocker->Unlock();
	m_pServApp->ReleaseSessionLocker( m_pLocker );
}

bool CServerSession::OnLogOn( CNcpMessage* pMsg, int nBufSize )
{
	char path[MAXPATH];

	//parse parameters
	int nFileMode = pMsg->GetRet();
	string strUser = (char*)pMsg->GetData();
	string strPass = (char*)( pMsg->GetData()+strUser.size()+1 );

	//get home path
#ifdef _WIN32
	GetModuleFileName( NULL, path, ELEMENTS(path) );
	char* p = strrchr( path, '\\' );
	if( p!=NULL )*p = '\0';
#else
	getcwd( path, ELEMENTS(path) );
#endif
	CatDir( path, strUser.c_str(), path, ELEMENTS(path) );	//now path is the current home path.

	//make home directory if necessary
	int nRet = S_OK;
	if( !IsDirExist(path) ){
		if( MkDir( path )!=0 ){
			nRet = E_ACCES;
		}
	}
	m_strHomeDir = path;
	m_strCurDir = m_strHomeDir;
	m_nFileMode = nFileMode;

	if( SUCCEEDED(nRet) && !IsDirExist(m_strHomeDir.c_str()) ){
		nRet = E_NOUSER;
	}
	pMsg->Init( CM_ACK, nRet );
	pMsg->Send( m_sock );

	//switch established
	if( SUCCEEDED(nRet) ){
		m_state = NCPS_ESTABLISHED;
		if( strcmp( strUser.c_str(), "root" )==0 )m_bRootUsr = true;

		ASSERT( m_pServApp );
		//lock the same user logon from other hosts.
		m_pLocker = m_pServApp->GetSessionLocker( strUser.c_str() );
		m_pLocker->Lock();
	}
	return SUCCEEDED(nRet);
}

bool CServerSession::OnLogOff( CNcpMessage* pMsg, int nBufSize )
{
	pMsg->Init( CM_ACK );
	pMsg->Send( m_sock );

	Close();
	return true;
}

//passive send CM_PASSIVE command, the server should return the passive data channel address
bool CServerSession::OnPassive( CNcpMessage* pMsg, int nBufSize )
{
	ASSERT( pMsg->GetCmd()==CM_PASSIVE );

	//create the passive socket if it not yet
	if( m_datasock==0 ){
		m_datasock = Socket( PF_INET, SOCK_STREAM, 0 );
		SOCKADDR_IN sockaddr;
		sockaddr.sin_family = AF_INET;
		sockaddr.sin_port = 0;
		sockaddr.sin_addr.s_addr = INADDR_ANY;
		Bind( m_datasock, (SOCKADDR*)&sockaddr, sizeof(SOCKADDR_IN) );
		Listen( m_datasock, 5 );
	}

	//send the socket address informaton back to the client
	SOCKADDR_IN sockaddr;
	socklen_t nlen = sizeof(SOCKADDR_IN);
	GetSockName( m_datasock, (SOCKADDR*)&sockaddr, &nlen );

	//first is IN_ADDR, then is port number.
	pMsg->Init( CM_ACK );
	short *pport = (short*)( pMsg->GetData() );
	*pport = sockaddr.sin_port;

	pMsg->SetDataSize( sizeof(short) );
	pMsg->Send( m_sock );

	return true;
}

bool CServerSession::OnPutFile( CNcpMessage* pMsg, int nBufSize )
{
	char path[MAXPATH];

	//parse the parameters
	char* pName = (char*)pMsg->GetData();
	CatDir( m_strCurDir.c_str(), pName, path, ELEMENTS(path) );
	string strFileName = path;
	bool bOverwrite = (bool)pMsg->m_nParamRet;


	pMsg->Init( CM_ACK );
	//the file is already there and the client doesn't overwrite it.
	if( !bOverwrite && IsFileExist( strFileName.c_str() ) ){
		pMsg->SetRet( S_EXIST );
		pMsg->Send( m_sock );
		return false;
	}

	//the client didn't request a data channel first.
	if( m_datasock==0 ){
		pMsg->SetRet( E_NOCHN );
		pMsg->Send( m_sock );
		return false;
	}

	//acknowledge the command
	pMsg->Send( m_sock );

/*	fd_set rset;
	FD_ZERO( &rset );
	FD_SET( m_datasock, &rset );
	Select( m_datasock+1, &rset, NULL, NULL, NULL );
	if( !FD_ISSET( m_datasock, &rset ) ){
		printf( "select error\n" );
		exit(1);
	}*/

	//wait for data channel connection and receive the file
	SOCKET sockclt = Accept( m_datasock, NULL, NULL );
	int nLen = RecvFileEx( sockclt, strFileName.c_str(), m_nFileMode );
	closesocket( sockclt );

	//wait for the exchanging ret code from client
	pMsg->Recv( m_sock, nBufSize );
	bool bOk = (nLen!=-1) && pMsg->IsSucceeded() && (nLen==pMsg->GetRet());

	//send the server's exchaning code.
	pMsg->Init( CM_ACK );
	if( !bOk ){
		pMsg->SetRet( E_ACCES );
	}
	pMsg->Send( m_sock );

	return bOk;
}

bool CServerSession::OnGetFile( CNcpMessage* pMsg, int nBufSize )
{
	char path[MAXPATH];

	//parse the parameters
	char* pName = (char*)pMsg->GetData();
	CatDir( m_strCurDir.c_str(), pName, path, ELEMENTS(path) );
	string strFileName = path;

	pMsg->Init( CM_ACK );
	//the file is not exist.
	if( !IsFileExist( strFileName.c_str() ) ){
		pMsg->SetRet( E_NOENT );
		pMsg->Send( m_sock );
		return false;
	}

	//the client didn't request a data channel first.
	if( m_datasock==0 ){
		pMsg->SetRet( E_NOCHN );
		pMsg->Send( m_sock );
		return false;
	}

	//acknowledge the command
	pMsg->Send( m_sock );

/*	fd_set rset;
	FD_ZERO( &rset );
	FD_SET( m_datasock, &rset );
	Select( m_datasock+1, &rset, NULL, NULL, NULL );
	if( !FD_ISSET( m_datasock, &rset ) ){
		printf( "select error\n" );
		exit(1);
	}*/

	//wait for data channel connection and send the file through the data channel
	SOCKET sockclt = Accept( m_datasock, NULL, NULL );
	int nLen = SendFileEx( sockclt, strFileName.c_str(), m_nFileMode );	//nLen is the actual bytes sent out.
	closesocket( sockclt );

	//wait for the exchanging ret code from client
	pMsg->Recv( m_sock, nBufSize );
	bool bOk = (nLen!=-1) && pMsg->IsSucceeded() && (pMsg->GetRet()==nLen);

	//send the server's exchaning code.
	pMsg->Init( CM_ACK );
	if( !bOk ){
		pMsg->SetRet( E_ACCES );
	}
	pMsg->Send( m_sock );

	return bOk;
}

bool CServerSession::OnRemFile( CNcpMessage* pMsg, int nBufSize )
{
	char path[MAXPATH];

	//parse the parameters
	char* pName = (char*)pMsg->GetData();
	if( IsAbsDir( pName ) ){
		strcpy( path, pName );
	}else{
		CatDir( m_strCurDir.c_str(), pName, path, ELEMENTS(path) );
	}

	int nRet = S_OK;
	if( remove( path )!=0 ){
		nRet = E_ACCES;
	}

	pMsg->Init( CM_ACK, nRet );
	//acknowledge the command
	pMsg->Send( m_sock );

	return SUCCEEDED( nRet );
}

bool CServerSession::OnChMod( CNcpMessage* pMsg, int nBufSize )
{
	char path[MAXPATH];

	//parse the parameters
	int nMode = pMsg->GetRet();
	char* pName = (char*)pMsg->GetData();
	if( IsAbsDir( pName ) ){
		strcpy( path, pName );
	}else{
		CatDir( m_strCurDir.c_str(), pName, path, ELEMENTS(path) );
	}

	//translate nMode to system independent mode;
	int nSysMode = 0;
	if( IsSet( nMode, 0x400 ) )nSysMode |= S_IREAD;
	if( IsSet( nMode, 0x200 ) )nSysMode |= S_IWRITE;
	if( IsSet( nMode, 0x100 ) )nSysMode |= S_IEXEC;

	int nRet = S_OK;
	if( chmod( path, nSysMode )!=0 ){
		nRet = E_NOENT;
	}

	pMsg->Init( CM_ACK, nRet );
	//acknowledge the command
	pMsg->Send( m_sock );

	return SUCCEEDED( nRet );
}

bool CServerSession::OnSoftLink( CNcpMessage* pMsg, int nBufSize )
{
	char path_src[MAXPATH], path_lnk[MAXPATH];

	//parse the parameters
	string strSrc = pMsg->GetData();
	string strLnk = pMsg->GetData()+strSrc.size()+1;

	if( IsAbsDir( strSrc.c_str() ) ){
		strcpy( path_src, strSrc.c_str() );
	}else{
		strcpy( path_src, m_strCurDir.c_str() );
		CatDir( path_src, strSrc.c_str(), path_src, ELEMENTS(path_src) );
	}
	if( IsAbsDir( strLnk.c_str() ) ){
		strcpy( path_lnk, strLnk.c_str() );
	}else{
		strcpy( path_lnk, m_strCurDir.c_str() );
		CatDir( path_lnk, strLnk.c_str(), path_lnk, ELEMENTS(path_lnk) );
	}

	int nRet = 0;
#ifdef _WIN32
	nRet = E_NOSUPT;
#else
	symlink( path_src, path_lnk );
	nRet = S_OK;
#endif

	//send back the return code.
	pMsg->Init( CM_ACK, nRet );
	pMsg->Send( m_sock );

	return SUCCEEDED(nRet);
}

bool CServerSession::OnFileMode( CNcpMessage* pMsg, int nSize )
{
	int nFileMode = pMsg->GetRet();
	m_nFileMode = nFileMode;

	pMsg->Init( CM_ACK );
	pMsg->Send( m_sock );

	return S_OK;
}

bool CServerSession::OnMkDir( CNcpMessage* pMsg, int nSize )
{
	char path[MAXPATH];

	//parse the parameters
	char* pName = (char*)pMsg->GetData();
	if( IsAbsDir( pName ) ){
		strcpy( path, pName );
	}else{
		CatDir( m_strCurDir.c_str(), pName, path, ELEMENTS(path) );
	}

	int nRetCode = S_OK;

	//make the directory
	if( MkDir( path )!=0 ){
		nRetCode = S_EXIST;
	}

	pMsg->Init( CM_ACK, nRetCode );
	pMsg->Send( m_sock );

	return SUCCEEDED(nRetCode);
}

bool CServerSession::OnChDir( CNcpMessage* pMsg, int nBufSize )
{
	char path[MAXPATH];
	char* pNewDir = (char*)pMsg->GetData();

	int nRetCode = S_OK;

	if( strcmp(pNewDir, "~")==0 ){
		m_strCurDir = m_strHomeDir;
	}else{
		//make the full path
		if( !IsAbsDir(pNewDir) ){
			//this is a relative directory
			CatDir( m_strCurDir.c_str(), pNewDir, path, ELEMENTS(path) );
		}else{
			//this is a absolute directory
			strcpy( path, pNewDir );
		}

		if( IsDirExist(path) ){
			NormDir( path, path, ELEMENTS(path) );
			//the path must be in the home directory
			if( IsSubDir(path, m_strHomeDir.c_str()) ){
				m_strCurDir = path;
			}else{
				nRetCode = E_ACCES;
			}
		}else{
			nRetCode = E_NOENT;
		}
	}

	pMsg->Init( CM_ACK, nRetCode );
	pMsg->Send( m_sock );

	return SUCCEEDED( nRetCode );
}

bool CServerSession::OnRmDir( CNcpMessage* pMsg, int nSize )
{
	char path[MAXPATH];

	//parse the parameters
	char* pName = (char*)pMsg->GetData();
	if( IsAbsDir( pName ) ){
		strcpy( path, pName );
	}else{
		CatDir( m_strCurDir.c_str(), pName, path, ELEMENTS(path) );
	}

	int nRet = S_OK;

	//make the directory
	if( rmdir( path )!=0 ){
		nRet = E_EXIST;
	}

	pMsg->Init( CM_ACK, nRet );
	pMsg->Send( m_sock );

	return SUCCEEDED(nRet);
}

bool CServerSession::OnRunImage( CNcpMessage* pMsg, int nSize )
{
	char path[MAXPATH];

	//parse the parameters
	bool bSync = (bool)pMsg->GetRet();
	string strImageName = (char*)(pMsg->GetData());

	if( !IsAbsDir( strImageName.c_str() ) ){
		strcpy( path, m_strCurDir.c_str() );
		CatDir( path, strImageName.c_str(), path, ELEMENTS(path) );
	}else{
		strcpy( path, strImageName.c_str() );
	}

	BOOL bOk;
	int nPriority = m_pServApp->GetPriority();
	DWORD dwExitCode = 0;
	if( bSync ){	//synchronize running, register the handle and wait the process
		HANDLE hProcess;
		//bOk = CreateProcess( path, m_strCurDir.c_str(), nPriority, &hProcess );
		bOk = CreateProcess( path, m_strCurDir.c_str(), &hProcess );
		SetProcessPriority( hProcess, nPriority );

		m_pServApp->RegistProc( hProcess );
		WaitForProcess( hProcess, &dwExitCode );
		m_pServApp->UnregistProc( hProcess );

		//CloseProcessHandle( hProcess );
		CloseHandle( hProcess );
	}else{			//just run the process and return.
		bOk = CreateProcess( path, m_strCurDir.c_str(), NULL );
		//bOk = CreateProcess( path, m_strCurDir.c_str(), nPriority, NULL );
		//SetProcessPriority( hProcess, nPriority );
	}


#ifdef _WIN32
	printf( "windows process exit code:%d\n", dwExitCode );
#else
	int status = dwExitCode;
	if( WIFEXITED(status) )printf( "normal termination, exit status=%d\n", WEXITSTATUS(status) );
	else if( WIFSIGNALED(status) )printf( "abnormal termination, signal number=%d\n", WTERMSIG(status) );
	else if( WIFSTOPPED(status) )printf( "child stopped, signal number=%d\n", WSTOPSIG(status) );
#endif

	//send back the return code.
	pMsg->Init( CM_ACK );
	if( !bOk ){
		pMsg->SetRet( E_NOEXEC );
	}else{
		pMsg->SetRet( dwExitCode );
	}
	pMsg->Send( m_sock );

	return bOk;
}

bool CServerSession::OnGetSysInfo( CNcpMessage* pMsg, int nBufSize )
{
	SYSINFO sysinfo;
	bzero( &sysinfo, sizeof(SYSINFO) );

#ifdef _WIN32
	sysinfo.nOpSystem = SI_WIN32;
#endif

#ifdef _UNIX
	sysinfo.nOpSystem = SI_UNIX;
#endif

#ifdef _LINUX
	sysinfo.nOpSystem = SI_LINUX;
#endif

#ifdef _SHARED_DISK
	sysinfo.bShareDisk = true;
#else
	sysinfo.bShareDisk = false;
#endif

//	if( IsRootUser() ){
		sysinfo.bSysIdle = IsSystemFree( m_pServApp->GetServState() );
		sysinfo.nSysStat = m_pServApp->GetServState();
		sysinfo.nSessions = m_pServApp->GetSessionCount();
		sysinfo.nChildPorcs = m_pServApp->GetProcCount();
//	}

	pMsg->Init( CM_ACK );
	sysinfo_hton( &sysinfo );
	bcopy( &sysinfo, pMsg->GetData(), sizeof(SYSINFO) );

	pMsg->SetDataSize( sizeof(SYSINFO) );
	pMsg->Send( m_sock );

	return true;
}

bool CServerSession::OnKillProcs( CNcpMessage* pMsg, int nBufSize )
{
	int nRet = S_OK;
	if( IsRootUser() ){
		m_pServApp->KillProcs();
	}else{
		nRet = E_ACCES;
	}
	pMsg->Init( CM_ACK, nRet );
	pMsg->Send( m_sock );

	return true;
}

bool CServerSession::OnSetIdle( CNcpMessage* pMsg, int nBufSize )
{
	//parse the parameters
	BOOL bIdle = pMsg->GetRet();

	int nRet = S_OK;
	if( IsRootUser() ){
		m_pServApp->SetServState( bIdle ? SYSTEM_IDLE : SYSTEM_USER_LOGON );
	}else{
		nRet = E_ACCES;
	}
	pMsg->Init( CM_ACK, nRet );
	pMsg->Send( m_sock );

	return true;
}

bool CServerSession::PumpMessage( CNcpMessage* pMsg, int nBufSize )
{
	switch( pMsg->GetCmd() ){
	case CM_LOGON:
		return OnLogOn( pMsg, nBufSize );
	case CM_LOGOFF:
		return OnLogOff( pMsg, nBufSize );
	case CM_PASSIVE:
		return OnPassive( pMsg, nBufSize );
	case CM_PUTFILE:
		return OnPutFile( pMsg, nBufSize );
	case CM_GETFILE:
		return OnGetFile( pMsg, nBufSize );
	case CM_REMFILE:
		return OnRemFile( pMsg, nBufSize );
	case CM_CHMOD:
		return OnChMod( pMsg, nBufSize );
	case CM_SOFTLINK:
		return OnSoftLink( pMsg, nBufSize );
	case CM_FILEMODE:
		return OnFileMode( pMsg, nBufSize );
	case CM_MKDIR:
		return OnMkDir( pMsg, nBufSize );
	case CM_CHDIR:
		return OnChDir( pMsg, nBufSize );
	case CM_RMDIR:
		return OnRmDir( pMsg, nBufSize );
	case CM_RUNIMAGE:
		return OnRunImage( pMsg, nBufSize );
	case CM_GETSYSINFO:
		return OnGetSysInfo( pMsg, nBufSize );
	case CM_KILLPROCS:
		return OnKillProcs( pMsg, nBufSize );
	case CM_SETIDLE:
		return OnSetIdle( pMsg, nBufSize );
	default:
		pMsg->Init(CM_ACK);
		pMsg->SetRet( E_UNKNOWN );
		pMsg->Send( m_sock );
		ASSERT(FALSE);
	}
	return false;
}

CServerSession::CServerSession( CServerApp* pServApp )
{
	m_pServApp = pServApp;
	m_sock = 0;
	m_datasock = 0;
	m_state = NCPS_CLOSED;
	m_bRootUsr = false;
	m_nFileMode = O_BINARY;
}

CServerSession::~CServerSession()
{
	if( m_sock )closesocket( m_sock );
	if( m_datasock )closesocket( m_datasock );
}

void CServerSession::RunSession( SOCKET sock )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	//initialize the state and socket.
	ASSERT( m_sock==0 );
	m_sock = sock;
	m_state = NCPS_CLOSED;

	while( true ){
		pMsg->Recv( m_sock, ELEMENTS(buf) );

		if( m_state==NCPS_CLOSED ){
			if( pMsg->GetCmd()!=CM_LOGON ){
				pMsg->Init( CM_ACK );
				pMsg->SetRet( E_UNKNOWN );
				pMsg->Send( m_sock );
				continue;
			}
		}
		
		PumpMessage( pMsg, ELEMENTS(buf) );

		if( m_state==NCPS_CLOSED )break;
	}
}


//////////////////////////////////////////////////////////////////////////////////////////////
//											CServerApp
////////////////////////////////////////////////////////////////////////////////////////

void CServerApp::SetPriority( int nStateFlags )
{
#ifdef _WIN32
	int dwPriorityClass = State2Priority( nStateFlags );
	list<HANDLE>::iterator iter = m_lstProcHandles.begin();
	while( iter!=m_lstProcHandles.end() ){
		HANDLE hProc = *iter++;
		if( GetPriorityClass(hProc)!=dwPriorityClass ){
			SetPriorityClass( hProc, dwPriorityClass );
		}
	}
#endif
}

int CServerApp::State2Priority( int nStateFlags )
{
    DWORD dwPriorityClass;
	bool bSystemFree = IsSystemFree( nStateFlags );
	if( bSystemFree )dwPriorityClass = NORMAL_PRIORITY_CLASS;
	else dwPriorityClass = IDLE_PRIORITY_CLASS;

	return dwPriorityClass;
}


CSessionLocker* CServerApp::GetSessionLocker( const char* strUser )
{
	Lock();
	POSITION pos = m_avlSessionLocks.SearchByKey( strUser );
	CSessionLocker* pLocker = NULL;
	if( pos!=NULL ){
		pLocker = m_avlSessionLocks.GetAt(pos);
	}else{
		pLocker = new CSessionLocker( strUser );
		m_avlSessionLocks.Insert( pLocker );
	}
	pLocker->AddRef();
	Unlock();
	return pLocker;
}

void CServerApp::ReleaseSessionLocker( CSessionLocker* pLocker )
{
	Lock();
	POSITION pos = m_avlSessionLocks.Search( pLocker );
	ASSERT( pos!=NULL );

	if( pLocker->ReleaseRef()==0 ){
		m_avlSessionLocks.RemoveAt( pos );
		delete pLocker;
	}
	Unlock();
}

void CServerApp::RegistProc( HANDLE hProc )
{
	Lock();
	m_lstProcHandles.push_back( hProc );
	Unlock();
}

void CServerApp::UnregistProc( HANDLE hProc )
{
	Lock();
	m_lstProcHandles.remove( hProc );
	Unlock();
}

void CServerApp::SetServState( int nStateFlags )
{
	Lock();
	if( m_nStateFlags!=nStateFlags ){
		m_nStateFlags = nStateFlags;
		if( IsClr(m_nStateFlags, SYSTEM_SUSPEND_SERVICE) ){
			//change the running process priority.
			SetPriority( nStateFlags );
		}
	}
	//always check suspend service
	if( IsSet(m_nStateFlags, SYSTEM_SUSPEND_SERVICE) ){
		//service must stop, kill the running processes
		list<HANDLE>::iterator iter = m_lstProcHandles.begin();
		while( iter!=m_lstProcHandles.end() ){
			HANDLE hProc = *iter++;
			KillProcess( hProc );
		}
	}
	Unlock();
}

int CServerApp::GetServState()
{
	return m_nStateFlags;
}

int CServerApp::GetPriority()
{
	return State2Priority( m_nStateFlags );
}

//kill all the running processes
void CServerApp::KillProcs()
{
	Lock();
	list<HANDLE>::iterator iter = m_lstProcHandles.begin();
	while( iter!=m_lstProcHandles.end() ){
		HANDLE hProc = *iter++;
		KillProcess( hProc );
/*#ifdef _WIN32
		TerminateProcess( hProc, -1 );
#else
		kill( hProc, SIGKILL );
#endif*/
	}
	Unlock();
}

//a helper function to free the AVL tree
BOOL CountSessions( CSessionLocker*& pItem, LPVOID pParam)
{
	int* pnCount = (int*)pParam;
	*pnCount += pItem->GetRef();
	return TRUE;
}

int CServerApp::GetSessionCount()
{
	Lock();
	int nRet = 0;
	m_avlSessionLocks.VisitTree( CountSessions, &nRet );
	Unlock();

	return nRet;
}

int CServerApp::GetProcCount()
{
	return (int)m_lstProcHandles.size();
}

struct _session_threa_param
{
	SOCKET sock;
	CServerApp* pServApp;
};

void CServerApp::RunServer()
{
	SOCKET socksrv;
	SOCKADDR_IN srvaddr;

	//setup the tcp server socket and bind.
	socksrv = Socket( PF_INET, SOCK_STREAM, 0 );

	srvaddr.sin_family = AF_INET;
	srvaddr.sin_addr.s_addr = INADDR_ANY;

	srvaddr.sin_port = htons( NCP_SERVER_PORT );
	Bind( socksrv, (SOCKADDR*)&srvaddr, sizeof(SOCKADDR_IN) );
	Listen( socksrv, 5 );

	//start the ping server
	//CreateThread( (THREAD_ROUTINE)PingServThread, NULL, 0, NULL );
	HANDLE hThread = CreateThread( (THREAD_ROUTINE)PingServThread, this, 0 );
	CloseHandle( hThread );

#ifndef _WIN32 
#ifdef _SHARED_DISK
	hThread = CreateThread( (THREAD_ROUTINE)MonitorThread, this, 0 );
	CloseHandle( hThread );
#endif 
#endif

	//prepare to accept the socket connection
	while( true ){
		SOCKET sockclt = Accept( socksrv, (SOCKADDR*)NULL, NULL );
		struct _session_threa_param* p = new struct _session_threa_param;
		p->pServApp = this;
		p->sock = sockclt;
		//start a session, CreateThread automatically close handle
		//CreateThread( (THREAD_FUNC)SessionThread, (void*)p, 0, NULL );
		hThread = CreateThread( (THREAD_ROUTINE)SessionThread, (void*)p, 0 );
		CloseHandle( hThread );
		//_beginthread( (void (*)(void *))SessionThread, 0, (void*)sockclt );
	}

	closesocket( socksrv );
}

int CServerApp::SessionThread( void* arg )
{
	struct _session_threa_param* pParam = (struct _session_threa_param*)arg;
	CServerSession session( pParam->pServApp );
	try{
		session.RunSession( pParam->sock );
	}catch( CSockException* e ){
		//release the session locker.
		perror( "sock exception:" );
		session.Close();
		e->Delete();
	}

	delete pParam;

	return 0;
}

int CServerApp::PingServThread( void* arg )
{
	CServerApp* pServApp = (CServerApp*)arg;
	ASSERT( pServApp!=NULL );

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
	dstaddr.sin_port = htons( NCP_HOST_REGIST_PORT );
	dstaddr.sin_addr.s_addr = INADDR_BROADCAST;

	//send the broadcast register message.
	*pcode = htonl(NCP_HOST_REGIST);
	*(pcode+1) = htonl( pServApp->GetServState() );
	sendto( sockbroad, buf, sizeof(int)*2, 0, (SOCKADDR*)&dstaddr, sizeof(SOCKADDR_IN) );
	closesocket( sockbroad );

	//then start the ping server
    //bind to specific port.
	SOCKET socksrv = Socket( PF_INET, SOCK_DGRAM, 0 );
	SOCKADDR_IN srvaddr;
	srvaddr.sin_family = AF_INET;
	srvaddr.sin_port = htons( NCP_SERVER_PING_PORT );
	srvaddr.sin_addr.s_addr = INADDR_ANY;
	Bind( socksrv, (SOCKADDR*)&srvaddr, sizeof(SOCKADDR_IN) );

	while( true ){
		SOCKADDR_IN srcaddr;
		socklen_t addrlen = sizeof(SOCKADDR_IN);
		int nLen = recvfrom( socksrv, buf, ELEMENTS(buf), 0, (SOCKADDR*)&srcaddr, &addrlen );
		if( nLen==sizeof(int) ){
			*pcode = ntohl( *pcode );
			if( *pcode==NCP_PING_SERVER ){		//a client is ping the server, sendback to confirmation message to the register port
				srcaddr.sin_port = htons( NCP_HOST_REGIST_PORT );
				*pcode = htonl(NCP_HOST_REGIST);
				*(pcode+1) = htonl( pServApp->GetServState() );
				sendto( socksrv, buf, sizeof(int)*2, 0, (SOCKADDR*)&srcaddr, sizeof(SOCKADDR_IN) );
			}
		}
	}

	closesocket( socksrv );
	return 0;
}


#ifndef _WIN32
#ifdef _SHARED_DISK
int CServerApp::MonitorThread( void* arg )
{
	CServerApp* pServApp = (CServerApp*)arg;
	ASSERT( pServApp!=NULL );
	char buf[5*MAXLINE];

	while( true ){
		MilliSleep( 20*1000 );

		bool bMoreLogon = false;

		FILE* pf = popen( "users", "r" );
		if( pf!=NULL && !feof(pf) ){
			fgets( buf, ELEMENTS(buf), pf );
			istrstream istr(buf, strlen(buf)+1);
			//retrieve the service login name
			char username[MAXLINE];
			strcpy( username, getlogin() );
			//check if other user is logged in
			while( true ){
				char login_name[MAXLINE];
				istr>>login_name;
				if( istr.fail() || istr.eof() )break;
				if( strcmp(username, login_name)!=0 ){
					bMoreLogon = true;
					break;
				}
			}
		}
		if( pf!=NULL )pclose( pf );

		int nSysState = pServApp->GetServState();
		if( bMoreLogon ){
			SetBit( nSysState, SYSTEM_SUSPEND_SERVICE );
			SetBit( nSysState, SYSTEM_USER_LOGON );
		}else{
			ClrBit( nSysState, SYSTEM_SUSPEND_SERVICE );
			ClrBit( nSysState, SYSTEM_USER_LOGON );
		}
		pServApp->SetServState( nSysState );
	}
	return 0;
}
#endif
#endif

