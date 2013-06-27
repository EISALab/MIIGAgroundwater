#pragma once

#include "netbase.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <treetmpl.h>
#include "uwkelobj.h"

//compitable head files for win32, unix and linux.
#ifdef _WIN32
#include <direct.h>
#else
#define O_TEXT		0x4000
#define O_BINARY	0x8000
#endif

class CInAddrName
{
public:
	IN_ADDR m_addr;
	char* m_pszHostName;
	CInAddrName( IN_ADDR addr, char* pszHostName ){
		m_addr = addr;
		m_pszHostName = new char[strlen(pszHostName)+1];
		strcpy( m_pszHostName, pszHostName );
	}
	~CInAddrName(){
		if( m_pszHostName )delete[] m_pszHostName;
	}
	//comparison by host ip
	int CompareAddr( const CInAddrName* pObj )const{
		return m_addr.S_un.S_addr==pObj->m_addr.S_un.S_addr ? 0 : 
				(m_addr.S_un.S_addr<pObj->m_addr.S_un.S_addr ? -1 : 1);
	}
	int CompareAddr( const IN_ADDR hostip )const{
		return m_addr.S_un.S_addr==hostip.S_un.S_addr ? 0 : 
				(m_addr.S_un.S_addr<hostip.S_un.S_addr ? -1 : 1);
	}
};

class CInAddrNameTraits : public CElementTraits<CInAddrName*, IN_ADDR>
{
public:
	static int CompareElements( const CInAddrName*& pItem1, const CInAddrName*& pItem2 )
	{
		return pItem1->CompareAddr( pItem2 );
	}
	static int CompareToKey( const CInAddrName*& pItem, IN_ADDR hostip )
	{
		return pItem->CompareAddr( hostip );
	}
};

typedef CAvlTree<CInAddrName*, CInAddrName*, IN_ADDR, CInAddrNameTraits> CAvlInAddrNameTree;

//a helper function to free the AVL tree
inline BOOL FreeInAddrName( CInAddrName*& pItem, LPVOID)
{
	delete pItem;
	return TRUE;
}

class CInAddrNameCache
{
private:
	HANDLE m_mxLock;
	CAvlInAddrNameTree m_avlInAddrNames;
public:
	CInAddrNameCache(){
		m_mxLock = CreateMutex();
	}
	~CInAddrNameCache(){
		Lock();
		m_avlInAddrNames.VisitTree( FreeInAddrName );
		Unlock();
		CloseHandle( m_mxLock );
	}
	void Lock(){
		LockMutex( m_mxLock );
	}
	void Unlock(){
		UnlockMutex( m_mxLock );
	}
	char* Lookup( IN_ADDR addr ){
		Lock();
		char* pRetName = NULL;
		POSITION pos = m_avlInAddrNames.SearchByKey( addr );
		if( pos!=NULL ){
			pRetName = m_avlInAddrNames.GetAt(pos)->m_pszHostName;
		}
		Unlock();
		return pRetName;
	}
	void CacheInAddr( IN_ADDR addr, char* pszHostName ){
		Lock();
		ASSERT( m_avlInAddrNames.SearchByKey(addr)==NULL );
		CInAddrName *pNameObj = new CInAddrName( addr, pszHostName );
		m_avlInAddrNames.Insert( pNameObj );
		Unlock();
	}
};

char* GetHostByAddr( IN_ADDR in_addr );

typedef struct netf_stat
{
	short nfs_nlink;
	unsigned short nfs_mode;
	int nfs_size;
	time_t nfs_atime;
	time_t nfs_mtime;
	time_t nfs_ctime;
}NETF_STAT, *PNETF_STAT;

inline void hton_netf_stat( PNETF_STAT pstat )
{
	pstat->nfs_nlink = htons( pstat->nfs_nlink );
	pstat->nfs_mode = htons( pstat->nfs_mode );
	pstat->nfs_size = htonl( pstat->nfs_size );
	pstat->nfs_atime = htonl( pstat->nfs_atime );
	pstat->nfs_mtime = htonl( pstat->nfs_mtime );
	pstat->nfs_ctime = htonl( pstat->nfs_ctime );
}

inline void ntoh_netf_stat( PNETF_STAT pstat )
{
	pstat->nfs_nlink = ntohs( pstat->nfs_nlink );
	pstat->nfs_mode = ntohs( pstat->nfs_mode );
	pstat->nfs_size = ntohl( pstat->nfs_size );
	pstat->nfs_atime = ntohl( pstat->nfs_atime );
	pstat->nfs_mtime = ntohl( pstat->nfs_mtime );
	pstat->nfs_ctime = ntohl( pstat->nfs_ctime );
}

inline bool IsFileExist( const char* strFileName )
{
	struct stat flstat;
	if( stat( strFileName, &flstat )==0 && 
		IsSet(flstat.st_mode, S_IFREG) )return true;
	
	return false;
}

inline bool IsDirExist( const char* strDir )
{
	struct stat flstat;
	if( stat( strDir, &flstat )==0 && 
		IsSet(flstat.st_mode, S_IFDIR) )return true;
	
	return false;
}

inline bool IsAbsDir( const char* strDir )
{
#ifdef _WIN32
	if( strDir[0]=='\\' || strchr(strDir, ':')!=NULL )return true;
#else
	if( strDir[0]=='/' )return true;
#endif

	return false;
}

inline bool IsSubDir( const char* strSub, const char* strDir )
{
	return true;
}

inline char* NormDir( const char* strDir, char* strRet, int nsize )
{
	if( strDir!=strRet )strcpy( strRet, strDir );

#ifdef _WIN32
	if( strRet[strlen(strRet)-1]!='\\' ){
		strcat( strRet, "\\" );
	}
	//_fullpath( strRet, strRet, nsize );
#else
	if( strRet[strlen(strRet)-1]!='/' ){
		strcat( strRet, "/" );
	}
#endif
	return strRet;
}

inline char* CatDir( const char* str1, const char* str2, char* strRet, int nsize )
{
	strcpy( strRet, str1 );
	NormDir( strRet, strRet, nsize );
	strcat( strRet, str2 );
	return strRet;
}

inline int MkDir( const char* strDir )
{
#ifdef _WIN32
		return mkdir( strDir );
#else
		return mkdir( strDir, S_IREAD|S_IWRITE|S_IEXEC );
#endif
}

int GetNetfStat( const char* strFile, PNETF_STAT* pStat );
void SendNetfStat( SOCKET sock, PNETF_STAT* pStat );
void RecvNetfStat( SOCKET sock, PNETF_STAT* pStat );

bool SendFileStream( SOCKET sock, FILE* pf, int nBufSize=0x1000 );
bool RecvFileStream( SOCKET sock, FILE* pf, unsigned long flsize, int nBufSize=0x1000 );

long SendFileStreamEx( SOCKET sock, FILE* pf, int nBufSize=0x1000 );
long RecvFileStreamEx( SOCKET sock, FILE* pf, long flsize=-1, int nBufSize=0x1000 );

//SendFile send a file through a connected stream.
//sock			the socket for the connected stream.
//strFileName	the filename of the file that will be sent
//nBufSize		the buffer size of each package.
long SendFileEx( SOCKET sock, const char* strFileName, int nFileMode, int nBufSize=0x1000 );
long RecvFileEx( SOCKET sock, const char* strFileName, int nFileMode, int nBufSize=0x1000 );

//SendFile send a file through a connected stream.
//sock			the socket for the connected stream.
//strFileName	the filename of the file that will be sent
//nBufSize		the buffer size of each package.
bool SendFile( SOCKET sock, const char* strFileName, int nBufSize=0x1000 );
bool RecvFile( SOCKET sock, const char* strFileName, int nBufSize=0x1000 );
