#pragma once

#include "netbase.h"

//compitable head files for win32, unix and linux.
#ifdef _WIN32
#include <direct.h>
#endif

#include <string>
#include <iostream>

#ifndef _WIN32
#define S_OK				0
#define SUCCEEDED(status)	((int)(status) >= 0)
#define FAILED(status)		((int)(status) < 0)
#endif

#define	NCP_SERVER_PORT			2400
#define NCP_SERVER_PING_PORT	2500
#define NCP_HOST_REGIST_PORT	2600

#define NCP_PING_SERVER			0x20
#define	NCP_HOST_REGIST			0x10
#define	NCP_REGISTER_EXIT		0x40


//ncp version and protocol definition.
#define NCP_VERSION			0x10
#define NCP_PROTOCOL		('N'+'C'+'P')


//general error code returned by network calls
#define	E_NOHOST			0x80000002			//the host is unknown
#define E_BROKEN			0x80000001			//the connection is broken
#define E_UNKNOWN			0x8fffffff			//unknown error

#define E_EXIST				0x80000100			//the file or diretory is already exist
#define S_EXIST				0x00000100			

#define E_NOENT				0x80000200			//the file or directory is not exist
#define S_NOENT				0x00000200		

#define E_NOSPC				0x80010000			//no enough space on the disk
#define E_NOEXEC			0x80020000			//the executalbe format error
#define E_IMGEXIT			0x80020001			//the executable gives failing exit code

#define E_NOSUPT			0x80030000			//the system doesn't support the command or service.
#define E_ACCES				0x80040000			//no such permission
#define E_NOCHN				0x80050000			//data channel is not created

#define E_NOUSER			0x80060000			//no such a user
#define	E_NOPASS			0x80070000			//invalid password

/*************************************************************************************************
									ncp command
**************************************************************************************************/
#define CM_ACK				0xffffffff

//logon logoff
#define CM_LOGON			1
#define CM_LOGOFF			2

#define CM_PASSIVE			5				//request a passive data channel
											//return the IN_ADDR + port number.

//file operation commands
#define CM_PUTFILE			10				//upload file to the server
											//data:			filename
											//S_EXIST		file is already exist
											//E_NOCHN		if passive is not issued. (no data channel)
											//E_NOSPC		no space on the disk

#define CM_GETFILE			11				//download file from the server
											//data:			filename
											//E_NOCHN		if passive is not issued. (no data channel)
											//E_NOENT		file is not exists

#define CM_REMFILE			12				//remove a file from the server
											//data:			filename
											//S_NOENT		file is not there

#define CM_SOFTLINK			14				//create a soft link for a file for unix system
											//data:			filename
											//E_NOSUP

#define CM_LIST				15				//list a folder
#define	CM_CHMOD			16				//change the rw mode of a file

#define CM_FILEMODE			17				//change the file transfer mode to ascii or binary
											//data: O_TEXT, O_BINARY

//execution commands
#define CM_RUNIMAGE			20				//run an executalbe image file on the server
											//E_NOENT		file is not exist
											//E_NOEXEC		executable format error

//directory operation commands
#define CM_MKDIR			30				//create a directory on the server
											//S_EXIST		the directory is already there
											
#define CM_CHDIR			31				//change the current directory on the server
											//E_NOENT		no such file or directory

#define CM_RMDIR			32				//remove a directory on the server
											//E_NOENT		no such a directory
											//E_EXIST		there are files in the directory

//query system command
#define CM_GETSYSINFO		40				//get server system information
#define CM_KILLPROCS		41				//kill all the waiting processes	//only root can do it.
#define CM_SETIDLE			42				//set the system state to idle. TRUE--idle, FALSE--busy


#define CM_CLEARALL			13

#define CM_RUNSYSCMD		21

#define CM_RESETSERVICE		100
#define CM_SETSERVICESTAT	101

#pragma pack(1)
class CNcpMessage{
public:
	unsigned short m_sProtocol, m_sVersion;	//protocol and version
	unsigned int m_nSize;					//size of the message package
	unsigned int m_nCmd;					//command of the message
	unsigned int m_nParamRet;				//it could be a parameter for command or a ret code for ack
	char data[0];
public:
	CNcpMessage(){
		bzero( this, sizeof(CNcpMessage) );
		m_sProtocol = NCP_PROTOCOL;
		m_sVersion = NCP_VERSION;
		m_nSize = sizeof(CNcpMessage);
	}
	CNcpMessage( int nCmd ){
		bzero( this, sizeof(CNcpMessage) );
		m_sProtocol = NCP_PROTOCOL;
		m_sVersion = NCP_VERSION;
		m_nCmd = nCmd;
		m_nSize = sizeof(CNcpMessage);
	}
	void Init( int nCmd, int nRet=S_OK ){
		bzero( this, sizeof(CNcpMessage) );
		m_sProtocol = NCP_PROTOCOL;
		m_sVersion = NCP_VERSION;
		m_nCmd = nCmd;
		m_nSize = sizeof(CNcpMessage);
		m_nParamRet = nRet;
	}
	int GetCmd(){
		return m_nCmd;
	}
	int GetSize(){
		return m_nSize;
	}
	void SetSize( int nSize ){
		m_nSize = nSize;
	}
	void SetDataSize( int nDataSize ){
		m_nSize = nDataSize + sizeof(CNcpMessage);
	}
	int GetRet(){
		return m_nParamRet;
	}
	void SetRet( int nRet ){
		m_nParamRet = nRet;
	}
	char* GetData(){
		return data;
	}
	void ntoh(){
		m_sProtocol = ntohs( m_sProtocol );
		m_sVersion = ntohs( m_sVersion );
		m_nSize = ntohl( m_nSize );
		m_nCmd = ntohl( m_nCmd );
		m_nParamRet = ntohl( m_nParamRet );
	}
	void hton(){
		m_sProtocol = htons( m_sProtocol );
		m_sVersion = htons( m_sVersion );
		m_nSize = htonl( m_nSize );
		m_nCmd = htonl( m_nCmd );
		m_nParamRet = htonl( m_nParamRet );
	}
	bool IsAck(){
		return m_nCmd==CM_ACK;
	}
	bool IsSucceeded(){
		return SUCCEEDED(m_nParamRet);
	}
	bool IsFailed(){
		return FAILED(m_nParamRet);
	}
	bool Send( SOCKET sock ){
		hton();
		char* buf = (char*)this;
		return SendLenBuffer( sock, buf, offsetof(CNcpMessage, m_nSize) );
	}
	bool Recv( SOCKET sock, int nBufSize ){
		char* buf = (char*)this;
		bool bOk = RecvLenBuffer( sock, buf, nBufSize, offsetof(CNcpMessage, m_nSize) );
		if( bOk ){
			ntoh();
			if( m_sProtocol!=NCP_PROTOCOL )return false;
			if( m_sVersion !=NCP_VERSION ) return false;
		}
		return bOk;
	}
};
#pragma pack()


//type of operating system (nOpSystem)
#define SI_WIN32			1
#define SI_UNIX				2
#define SI_LINUX			3

//the system information structure used by GetSysInfo
typedef struct _SYSINFO{
	int nOpSystem;			//the operating system type.
	int bShareDisk;			//if the hard drive is shared by multiple hosts
	int nSysStat;			//the system status.
	int bSysIdle;			//the system is idle
	int nSessions;			//the number of running sessions
	int nChildPorcs;		//the number of running processes
}SYSINFO;

inline void sysinfo_ntoh( SYSINFO* pinfo )
{
	pinfo->nOpSystem = ntohl(pinfo->nOpSystem);
	pinfo->bShareDisk = ntohl(pinfo->bShareDisk);
	pinfo->nSysStat = ntohl( pinfo->nSysStat );
	pinfo->bSysIdle = ntohl( pinfo->bSysIdle );
	pinfo->nSessions = ntohl( pinfo->nSessions );
	pinfo->nChildPorcs = ntohl( pinfo->nChildPorcs );
}

inline void sysinfo_hton( SYSINFO* pinfo )
{
	pinfo->nOpSystem = htonl(pinfo->nOpSystem);
	pinfo->bShareDisk = htonl(pinfo->bShareDisk);
	pinfo->nSysStat = htonl( pinfo->nSysStat );
	pinfo->bSysIdle = htonl( pinfo->bSysIdle );
	pinfo->nSessions = htonl( pinfo->nSessions );
	pinfo->nChildPorcs = htonl( pinfo->nChildPorcs );
}