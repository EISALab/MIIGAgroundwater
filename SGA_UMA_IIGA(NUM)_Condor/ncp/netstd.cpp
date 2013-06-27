
//#ifdef _WIN32 //_MSC_VER
//#include "stdafx.h"
//#endif

#include "netbase.h"
#include "netstd.h"

CInAddrNameCache theInAddrNameCache;

char* GetHostByAddr( IN_ADDR in_addr )
{
	char* pszHostName = theInAddrNameCache.Lookup( in_addr );
	if( pszHostName==NULL ){
		struct hostent * phent = gethostbyaddr( (char*)&in_addr, sizeof(IN_ADDR), AF_INET );
		if( phent!=NULL ){
			pszHostName = phent->h_name;
		}else{
			pszHostName = inet_ntoa( in_addr );
		}
		theInAddrNameCache.CacheInAddr( in_addr, pszHostName );
	}

	return pszHostName;
}


int GetNetfStat( const char* strFile, PNETF_STAT pStat )
{
	struct stat flstat;
	int nRet = stat( strFile, &flstat );

	if( nRet==0 ){
		pStat->nfs_nlink = flstat.st_nlink;
		pStat->nfs_mode = flstat.st_mode;
		pStat->nfs_size = (int)flstat.st_size;
		pStat->nfs_atime = flstat.st_atime;
		pStat->nfs_mtime = flstat.st_mtime;
		pStat->nfs_ctime = flstat.st_ctime;
	}
	return nRet;
}

void SendNetfStat( SOCKET sock, PNETF_STAT pStat )
{
	SendBuffer( sock, (const char*)pStat, sizeof(NETF_STAT) );
}

void RecvNetfStat( SOCKET sock, PNETF_STAT pStat )
{
	RecvBuffer( sock, (char*)pStat, sizeof(NETF_STAT) );
}

bool SendFileStream( SOCKET sock, FILE* pf, int nBufSize )
{
	ASSERT( NULL!=pf );
	char* pBuf = new char[nBufSize];
	try{
		while( !feof(pf) && !ferror(pf) ){
			int nLen = (int)fread( pBuf, 1, nBufSize, pf );
			if( nLen>0 )SendBuffer( sock, pBuf, nLen );
		}
	}catch( CSockException* ){
		delete[] pBuf;
		throw;
	}

	delete[] pBuf;
	return !ferror( pf );
}

bool RecvFileStream( SOCKET sock, FILE* pf, unsigned long flsize, int nBufSize )
{
	ASSERT( NULL!=pf );

	//receive the file content.
	unsigned long lAvailBytes = flsize;

	char* pBuf = new char[nBufSize];
	try{
		while( lAvailBytes>0 && !ferror(pf) ){
			int nAvailBuf = min( (unsigned long)nBufSize, lAvailBytes );
			RecvBuffer( sock, pBuf, nAvailBuf );
			fwrite( pBuf, 1, nAvailBuf, pf );

			lAvailBytes -= nAvailBuf;
		}
	}catch( CSockException* ){
		delete[] pBuf;
		throw;
	}

	delete[] pBuf;

	return !ferror( pf );
}

//Send file stream from pf and send it out throught sock. 
//nBufSize is the internal reading buffer size. it is allocated before sending and deallocated after sending.
//Return: if successful the function returns the actual bytes sent out, otherwise it returns -1.
//Note: the returned value may be smaller than the actual file size if pf is in "text" mode.
long SendFileStreamEx( SOCKET sock, FILE* pf, int nBufSize )
{
	ASSERT( NULL!=pf );
	char* pBuf = new char[nBufSize];
	long nSentBytes = 0;	//nRet returns the total number of bytes sent out throught the socket.
	try{
		while( !feof(pf) && !ferror(pf) ){
			int nRet = (int)fread( pBuf, 1, nBufSize, pf );
			if( nRet>0 ){
				SendBuffer( sock, pBuf, nRet );
				nSentBytes += nRet;
			}
		}
	}catch( CSockException* ){
		delete[] pBuf;
		throw;
	}

	delete[] pBuf;
	return !ferror( pf ) ? nSentBytes : -1;
}

//Receive file stream through sock to pf.
//flsize tells the function how many bytes to receive on the sock. if flsize is -1, the function reads until the sock is closed.
//nBufSize is the internal reading buffer size. it is allocated before sending and deallocated after sending.
//Return: if successful the function returns the actual bytes received, otherwise it returns -1.
//Note: the returned value may be smaller than the actual file size if pf is in "text" mode.
long RecvFileStreamEx( SOCKET sock, FILE* pf, long flsize, int nBufSize )
{
	ASSERT( NULL!=pf );

	//receive the file content.
	long nRecvBytes = 0;		//the received bytes.

	char* pBuf = new char[nBufSize];
	int nRet = 0;
	while( !ferror(pf) && (flsize==-1 || nRecvBytes<flsize) ){
		int nAvailBuf = flsize==-1 ? nBufSize : min( (long)nBufSize, flsize-nRecvBytes );
		nRet = recv( sock, pBuf, nAvailBuf, 0 );
		if( nRet<=0 )break;

		fwrite( pBuf, 1, nRet, pf );
		nRecvBytes += nRet;
	}
	delete[] pBuf;

	if( nRet<0 || (flsize!=-1 && nRet==0) ){
		throw new CSockException( nRecvBytes );
	}
	return !ferror( pf ) ? nRecvBytes : -1;
}

//SendFile send a file through a connected stream.
//sock			the socket for the connected stream.
//strFileName	the filename of the file that will be sent
//nBufSize		the buffer size of each package.
long SendFileEx( SOCKET sock, const char* strFileName, int nFileMode, int nBufSize )
{
	char* strMode = NULL;
	//send the file content stream.
	switch( nFileMode ){
	case O_TEXT: 
		strMode = "rt"; 
		break;
	case O_BINARY: 
		strMode = "rb"; 
		break;
	default:
			ASSERT(FALSE);
	}

	FILE* pf = fopen( strFileName, strMode );
	if( pf==NULL )return -1;

	long lRet = SendFileStreamEx( sock, pf, nBufSize );
	fclose( pf );

	return lRet;
}

long RecvFileEx( SOCKET sock, const char* strFileName, int nFileMode, int nBufSize )
{
	char* strMode = NULL;
	//send the file content stream.
	switch( nFileMode ){
	case O_TEXT: 
		strMode = "wt"; 
		break;
	case O_BINARY: 
		strMode = "wb"; 
		break;
	default:
			ASSERT(FALSE);
	}

	FILE* pf = fopen( strFileName, strMode );
	if( pf==NULL )return -1;

	long lRet = RecvFileStreamEx( sock, pf, -1, nBufSize );
	fclose( pf );

	return lRet;
}

bool SendFile( SOCKET sock, const char* strFileName, int nBufSize )
{
	//get file header
	NETF_STAT nfstat;
	if( GetNetfStat(strFileName, &nfstat)!=0 )return false;

	//send file header
	hton_netf_stat( &nfstat );
	SendNetfStat( sock, &nfstat );

	//send the file content stream.
	FILE* pf = fopen( strFileName, "rb" );
	if( pf==NULL )return false;

	bool bOk = SendFileStream( sock, pf, nBufSize );

	fclose( pf );
	return bOk;
}

bool RecvFile( SOCKET sock, const char* strFileName, int nBufSize)
{
	//get file header
	NETF_STAT nfstat;
	RecvNetfStat( sock, &nfstat );

	ntoh_netf_stat( &nfstat );

	//receive the file content.
	FILE* pf = fopen( strFileName, "wb" );
	if( pf==NULL )return false;

	bool bOk = RecvFileStream( sock, pf, nfstat.nfs_size, nBufSize );

	fclose( pf );

	//change the file mode to the right mode
	bOk = bOk && (chmod( strFileName, nfstat.nfs_mode )==0);

	return bOk;
}

/*int SendFile( SOCKET sock, char* strFileName, int nBufSize )
{
	//find the file size and attribute
	struct _finddata_t fileinfo;
	intptr_t hfind = _findfirst( strFileName, &fileinfo );
	if( hfind==-1 )return false;

	bool bSuccess = false;
	do{
		if( !(fileinfo.attrib & FILE_ATTRIBUTE_DIRECTORY) ){
			bSuccess = true;
			break;
		}
	}while( _findnext( hfind, &fileinfo )==0 );
	_findclose( hfind );
	if( !bSuccess )return false;

	//put the file basic infomation into the header and send it out.
	NET_FILE_HEADER flHeader;
	flHeader.nSize = htonl( sizeof(NET_FILE_HEADER) );
	flHeader.nAttrib = htonl( fileinfo.attrib );
	flHeader.lFileSize = htonl( fileinfo.size );

	if( SendLenBuffer( sock, (char*)&flHeader )<0 )return false;

	//send the file content.
	FILE* pf = fopen( strFileName, "rb" );
	if( pf==NULL )return false;

	char* pBuf = new char[nBufSize];
	while( !feof(pf) ){
		int nLen = (int)fread( pBuf, 1, nBufSize, pf );
		if( SendBuffer( sock, pBuf, nLen, NULL )==SOCKET_ERROR ){
			bSuccess = false;
			break;
		}
	}

	delete[] pBuf;
	fclose( pf );
	return bSuccess;
}

inline bool RecvFile( SOCKET sock, char* strFileName, int nBufSize=0x1000 )
{
	bool bSuccess = true;

	//receive the file header
	NET_FILE_HEADER flHeader;

	if( RecvLenBuffer( sock, (char*)&flHeader, sizeof(flHeader) )<=0 )return false;
	flHeader.nSize = ntohl( flHeader.nSize );
	flHeader.nAttrib = ntohl( flHeader.nAttrib );
	flHeader.lFileSize = ntohl( flHeader.lFileSize );

	//receive the file content.
	unsigned long lAvailBytes = flHeader.lFileSize;
	FILE* pf = fopen( strFileName, "wb" );
	if( pf==NULL )return false;

	char* pBuf = new char[nBufSize];
	while( lAvailBytes>0 ){
		int nAvailBuf = min( (unsigned long)nBufSize, lAvailBytes );
		if( RecvBuffer( sock, pBuf, nAvailBuf )<=0 ||
			fwrite( pBuf, 1, nAvailBuf, pf )<(size_t)nAvailBuf ){
			bSuccess = false;
			break;
		}

		lAvailBytes -= nAvailBuf;
	}

	delete[] pBuf;
	fclose( pf );
	return bSuccess;
}*/

