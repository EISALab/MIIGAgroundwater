#pragma once

//compitable head files for win32, unix and linux.
#ifdef _WIN32
#include <Winsock2.h>
#include <io.h>

#else
#include <unistd.h>
#include <ctype.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>
#include <strings.h>
#include <signal.h>

#endif	//end of #ifdef _WIN32

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <time.h>
#include <exception>

#include "ofdump.h"

//compitable macros for win32, unix and linux
#ifdef _WIN32		//win 32 macros
#define bzero( addr, size ) memset( (addr), 0, (size) )
#define bcopy( addrsrc, addrdst, size ) memcpy( (addrdst), (addrsrc), (size) )

#define WSVERS	MAKEWORD(2,0)
typedef int socklen_t;

#else
typedef struct sockaddr_in	SOCKADDR_IN;
typedef struct sockaddr		SOCKADDR;
typedef int 				SOCKET;
#define SOCKET_ERROR		-1
#define INVALID_SOCKET		(SOCKET)(~0)

typedef int BOOL;
#define TRUE				1
#define FALSE				0
#define FAR
#define closesocket			close

#ifndef min
#define min(a,b) (a)<(b)?(a):(b)
#endif
#ifndef max
#define max(a,b) (a)<(b)?(a):(b)
#endif

inline char* strlwr( char* string )
{
	for( char* pstr=string; *pstr; pstr++ )
		if( *pstr>='A' && *pstr<='Z' )*pstr+=32;
	return string;
}

inline char* strupr( char *string )
{
	for( char* pstr=string; *pstr; pstr++ )
		if( *pstr>='a' && *pstr<='z' )*pstr-=32;
	return string;
}

#endif			//_WIN32

#ifndef ASSERT
	#if     _MSC_VER > 1000
			#include <crtdbg.h>
			#define ASSERT _ASSERT
	#else
			#include <assert.h>
			#define ASSERT assert
	#endif

	#define ASSERT_VALID(p) ASSERT(p!=NULL)
#endif

using namespace std;

#ifndef ELEMENTS
	#define ELEMENTS(array)		(sizeof(array)/sizeof((array)[0]))	//elements number of an arrray
#endif

#define MAXOFFSET	512-sizeof(int)

#define SetBit( a, i ) ( (a) |= (i) )
#define ClrBit( a, i ) ( (a) &= ~(i) )
#define IsSet( a, i ) ( ((a) & (i)) != 0 )
#define IsClr( a, i ) ( !IsSet( (a), (i) ) )

typedef struct in_addr IN_ADDR;

class CSockException : public exception
{
public:
	int m_nErrorCode;				//the error code that caused the excepton
	int m_nBytesDone;				//the bytes done before the exception is thrown
public:
	CSockException( int nBytesDone=0 ) : m_nBytesDone(nBytesDone)
	{
#ifdef _WIN32
		m_nErrorCode = WSAGetLastError();
#else
		m_nErrorCode = 0; //errno;
#endif
	}
	//~CSockException(){};

	void Delete(){ delete this; }
};

/*inline void PrintNetError()
{
	int nEwsa = WSAGetLastError();
	int nElst = GetLastError();
	printf( "WSA Error is %d, Last Error is %d\n", nEwsa, nElst );
}*/

inline SOCKET Socket( int af, int type, int protocol )
{
	SOCKET sock_ret = socket( af, type, protocol );
	if( sock_ret < 0 )throw new CSockException();

	return sock_ret;
}

inline int Bind( SOCKET s, const struct sockaddr FAR *name, int namelen )
{
	int nRet = bind( s, name, namelen );
	if( nRet != 0 )throw new CSockException();

	return nRet;
};

inline int Listen( SOCKET s, int backlog )
{
	int nRet = listen( s, backlog );
	if( nRet != 0 )throw new CSockException();

	return nRet;
}

inline SOCKET Accept( SOCKET s, struct sockaddr FAR *addr, socklen_t FAR *addrlen )
{
	SOCKET sockclt=accept( s, addr, addrlen );
	if( sockclt < 0 ){
		cdump<<lock<<"accepting exception\n"<<endl<<unlock;
		throw new CSockException();
	}

	return sockclt;
}

inline int Select( int nfds, fd_set FAR *readfds, fd_set FAR *writefds, fd_set FAR *exceptfds, struct timeval FAR *timeout )
{
	int nRet = select( nfds, readfds, writefds, exceptfds, timeout );
	if( nRet<0 )throw new CSockException();

	return nRet;
}

inline int Connect( SOCKET s, const struct sockaddr FAR *name, int namelen )
{
	int nRet = connect( s, name, namelen );
	if( nRet != 0 ){
		SOCKADDR_IN* name_in = (SOCKADDR_IN*)name;
		struct hostent * phent = gethostbyaddr( (char*)&name_in->sin_addr, sizeof(IN_ADDR), AF_INET );
		cdump<<lock<<(phent?phent->h_name:"unknown host")<<" connecting exception\n"<<endl<<unlock;
		throw new CSockException();
	}

	return nRet;
}

inline int Send ( SOCKET s, const char FAR * buf, int len, int flags )
{
	int nRet = send( s, buf, len, flags );
	if( nRet==SOCKET_ERROR ){
		cdump<<lock<<"sending exception\n"<<endl<<unlock;
		throw new CSockException();
	}

	return nRet;
}

inline int Recv( SOCKET s, char FAR* buf, int len, int flags )
{
	int nRet = recv( s, buf, len, flags );
	if( nRet==SOCKET_ERROR ){
		cdump<<lock<<"recving exception\n"<<endl<<unlock;
		throw new CSockException();
	}

	return nRet;
}

inline int GetSockName( SOCKET s, struct sockaddr FAR *name, socklen_t FAR *namelen )
{
	int nRet = getsockname( s, name, namelen );
	if( nRet != 0 )throw new CSockException();

	return nRet;
}

inline int GetPeerName( SOCKET s, struct sockaddr FAR* name, socklen_t FAR *namelen )
{
	int nRet = getpeername( s, name, namelen );
	if( nRet != 0 )throw new CSockException();

	return nRet;
}

//The function except to send nLen bytes from buf. 
//If error occurs, a CSocketError will be thrown
inline void SendBuffer( SOCKET sock, const char* buf, int nLen )
{
	int nSentBytes = 0;
	int nRet = 0;

	while( nSentBytes<nLen ){
		nRet = send( sock, buf+nSentBytes, nLen-nSentBytes, 0 );
		if( nRet==SOCKET_ERROR )break;

		nSentBytes += nRet;
	}

//	if( nRet==SOCKET_ERROR )throw new CSockException( nSentBytes );
//	ASSERT( nSentBytes==nLen );
	if( (nRet==SOCKET_ERROR) || (nSentBytes!=nLen) )throw new CSockException( nSentBytes );
}

//The function except to receive exact nLen bytes into buf. 
//If error occurs or the connection is shutdown, a CSocketError will be thrown
inline void RecvBuffer( SOCKET sock, char* buf, int nLen )
{
	int nRecvBytes = 0;
	int nRet = -1;

	while( nRecvBytes<nLen ){
		nRet = recv( sock, buf+nRecvBytes, nLen-nRecvBytes, 0 );
		if( nRet<=0 )break;

		nRecvBytes += nRet;
	}

//	if( nRet<=0 )throw new CSockException( nRecvBytes );
//	ASSERT( nRecvBytes==nLen );
	if( (nRet<=0) || (nRecvBytes!=nLen) )throw new CSockException( nRecvBytes );
}

//SendLenBuffer send a self length encoded stream. 
//The (buf+nLenOffset) must be a size field in network byte order indicating the length of the sending stream
//the nLenOffset must less than MAXOFFSET (which is a minimum package size on internet)
//the function return false if the nLenOffset is not within the range.
inline bool SendLenBuffer( SOCKET sock, const char* buf, int nLenOffset=0 )
{
	if( nLenOffset>MAXOFFSET )return false;

	int nLen = ntohl( *((int*)(buf+nLenOffset)) );
	SendBuffer( sock, buf, nLen );

	return true;
}

//RecvLenBuffer receive a self length encoded stream. 
//The (buf+nLenOffset) must be a size field in network byte order indicating the length of the receiving stream
//the nLenOffset must less than MAXOFFSET (which is a minimum package size on internet)
//the function return false if the nLenOffset is not within the range or there is no enough buffer space to hold the whole stream.
inline bool RecvLenBuffer( SOCKET sock, char* buf, int nBufSize, int nLenOffset=0 )
{
	ASSERT( nLenOffset<=MAXOFFSET );
	if( nLenOffset>MAXOFFSET )return false;

	//compute the head bytes
	int nHeadBytes = nLenOffset+sizeof(int);
	ASSERT( nHeadBytes<=nBufSize );
	if( nHeadBytes>nBufSize )return false;

	//peek the head bytes from receive buffer but don't remove them
	int nRet = recv( sock, buf, nHeadBytes, MSG_PEEK );
	if( nRet<=0 )throw new CSockException();
	ASSERT( nRet>=nHeadBytes );

	//now receive the stream indicated by nLen
	int nLen = ntohl( *((int*)(buf+nLenOffset)) );
	if( nBufSize<nLen )return false;

	RecvBuffer( sock, buf, nLen );

	return true;
}


inline char* _tcsdec( char* start, char* current )
{
        return current>start ? current-1 : NULL;
}

inline char* _tcsinc( char* string )
{
        return string+1;
}


inline char* trimright( char* string )
{
        char *lpsz = string+strlen(string);

        lpsz = _tcsdec(string, lpsz);

        while( lpsz && isspace(*lpsz) )
                lpsz = _tcsdec(string, lpsz);

        if( lpsz==NULL )lpsz=string;
        else lpsz++;

        *lpsz = '\0';

        return string;
}

inline char* trimleft( char* string )
{
        char* lpsz = string;

        while (isspace(*lpsz))
                lpsz = _tcsinc(lpsz);

        if (lpsz != string)
        {
                // fix up data and length
                int nDataLength = strlen(string) - (lpsz - string);
                memmove(string, lpsz, (nDataLength+1)*sizeof(char));
        }
        return string;
}

inline char* trim( char* string )
{
	trimleft( string );
	return trimright( string );
}