#include <sys/types.h>
#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <ctype.h>

#ifdef _WIN32
#include <winsock2.h>
#include <windows.h>
#include <io.h>
#include <direct.h>

typedef long intptr_t;
#else
#include <unistd.h>
#include <dirent.h>

#define _stdcall
#endif

#include "ncphost.h"
#include "ncpjob.h"
#include "netstd.h"
#include "ofdump.h"

#define MAXPATH 256
#define BUFSIZE 4096
#define TRUE	 1
#define FALSE	 0

static int RemoveDir( char* strPath );
static int CopyFile( char* strSrc, char* strDes, int bForce );
static int CopyDir( char* strSrc, char* strDes );
static char* ExtractFileName( char* pszFileName );
static void NormalizePath( char* pszPath );
static char * _itoa ( int val, char *buf, int radix );
static int CompareSize( char* strSrc, char* strDes );

/*static char* trimright( char* string );
static char* trimleft( char* string );
static char* trim( char* string );
static char* _tcsdec( char* start, char* current );
static char* _tcsinc( char* string );*/

CHostSet* g_phostset;

static int DoPdeFit( int nId, CClientSession* pSession, void* pParam );

extern "C"{
	double _stdcall STRTOFC( char* pstr, int nCount )
	{
		char buf[MAXPATH];
		memcpy(buf, pstr, nCount*sizeof(char));
		buf[nCount] = '\0';
		return atof( buf );
	}

	int _stdcall COPYFILEC( char* pSrcFile, char* pDesFile, int nSrcChar, int nDesChar )
	{
		char strSrc[MAXPATH];
		char strDes[MAXPATH];
		if( nSrcChar>=MAXPATH || nDesChar>=MAXPATH ){
			return -1;
		}

		memcpy(strSrc, pSrcFile, nSrcChar*sizeof(char));
		memcpy(strDes, pDesFile, nDesChar*sizeof(char));

		strSrc[nSrcChar] = '\0';
		strDes[nDesChar] = '\0';
		
		return CopyFile( strSrc, strDes, TRUE );

	}

	int _stdcall REMOVEDIRC( char* pStrPath, int nChar )
	{
		char strPath[MAXPATH];
		if( nChar>=MAXPATH ){
			return -1;
		}

		memcpy( strPath, pStrPath, nChar*sizeof(char) );
		strPath[nChar] = '\0';
		return RemoveDir( strPath );
	}
	int _stdcall REMOVEFILEC( char* pStrFile, int nChar )
	{
		char strFile[MAXPATH];
		if( nChar>=MAXPATH ){
			return -1;
		}

		memcpy( strFile, pStrFile, nChar*sizeof(char) );
		strFile[nChar] = '\0';
		return remove( strFile );
	}

	int _stdcall COPYDIRC( char* pSrcPath, int nSrcChar, char* pDesPath, int nDesChar )
	{
		char strSrc[MAXPATH];
		char strDes[MAXPATH];
		if( nSrcChar>=MAXPATH || nDesChar>=MAXPATH ){
			return -1;
		}

		memcpy(strSrc, pSrcPath, nSrcChar*sizeof(char));
		memcpy(strDes, pDesPath, nDesChar*sizeof(char));

		strSrc[nSrcChar] = '\0';
		strDes[nDesChar] = '\0';

		return CopyDir( strSrc, strDes );
	}

//	void _stdcall MAKESLAVENAME( char* pStr, int* pnId, char* pStrRet, int nStrChar, int nRetChar )
	void _stdcall MAKESLAVENAME( char* pStr, int nStrChar, int* pnId, char* pStrRet, int nRetChar )
	{
		char str[MAXPATH];
		if( nStrChar>=MAXPATH ){
			return;
		}

		memcpy(str, pStr, nStrChar*sizeof(char));
		str[nStrChar] = '\0';

		strcpy(pStrRet, str);
		_itoa( *pnId, str, 10 );
		strcat(pStrRet, str);
		for( int i=strlen(pStrRet); i<nRetChar; i++ ){
			pStrRet[i] = ' ';
		}
	}

	void _stdcall COPYSLAVEFILES( char* pStrFileList, int nChar )
	{
		char str[MAXPATH], buf[MAXPATH], strFile[MAXPATH];
		int nStaticFlag = 0;

		if( nChar>=MAXPATH ){
			return;
		}

		memcpy(str, pStrFileList, nChar*sizeof(char));
		str[nChar] = '\0';
		FILE* pFile = fopen( str, "rt" );
		while( !feof(pFile) ){
			fgets( buf, MAXPATH, pFile );
			if( feof(pFile) )break;
//			fscanf( pFile, "%s", buf );
			trim(buf);
			if( buf[0]=='\0' || buf[0]=='#' || strncmp(buf, "//", 2)==0 )continue;

			sscanf( buf, "%s%d", strFile, &nStaticFlag );

			//source file name
			strcpy( str, "../" );
			strcat( str, strFile );

			//destination file name
			if( nStaticFlag==1 ){
				strcpy( buf, "../static/" );
				strcat( buf, strFile );
				CopyFile( str, buf, FALSE );
			}else{
				strcpy( buf, strFile );
				if( nStaticFlag==2 )CopyFile( str, buf, FALSE ); //for local executation, don't do uncessary copying
				else CopyFile( str, buf, TRUE );
			}
			//CopyFile( str, buf );
		}
		fclose(pFile);
	}
	
	void _stdcall WAITEFOREXEC( char* pStrExec, int nStrChar )
	{
		char str[MAXPATH];
		if( nStrChar>=MAXPATH ){
			return;
		}

		memcpy(str, pStrExec, nStrChar*sizeof(char));
		str[nStrChar] = '\0';

		system( str );
//		FILE* pFile = popen( str, "w" );
//		pclose(pFile);
	}

	void _stdcall INITSOCKET()
	{
		WSADATA wsadata;
		if( WSAStartup(WSVERS, &wsadata)!=0 ){
			exit(1);
		}

		g_phostset = new CHostSet();
		CHostSet& hostset = (*g_phostset);

		hostset.SetUseLocalHost( false );
		hostset.SetUserPass( "evasinha", "" );

		hostset.LoadFromFile( "host.txt" );
		hostset.Listen();
		cdump.setcontext( "trace.txt", ios::out | ios::app );
	}

	void _stdcall REFRESHHOSTS()
	{
			CHostSet& hostset = (*g_phostset);
			hostset.LoadFromFile( "host.txt" );
	}

	void _stdcall CLEARSOCKET()
	{
		delete g_phostset;

		WSACleanup();
	}

	void _stdcall ROUTESLAVES( int* pnJobIds, int* pnJobCount )
	{
		CHostSet& hostset = (*g_phostset);
		CJobSet jobset;

		for( int i=0; i<*pnJobCount; i++ ){
			jobset.Add( pnJobIds[i], DoPdeFit, NULL, 100 );
		}
		jobset.RouteJobs( &hostset );
		jobset.Clear();
	}

/*	void _stdcall ROUTESLAVES( int* pnJobIds, int* pnJobCount )
	{
		CHostSet& hostset = (*g_phostset);
//		CJobSet& jobset = *g_pjobset;
//		CHostSet hostset;
		CJobSet jobset;

//		hostset.SetUseLocalHost( true );
//		hostset.SetUserPass( "smyan", "" );

//		hostset.Listen();
//		hostset.LoadFromFile( "host.txt" );

		for( int i=0; i<*pnJobCount; i++ ){
			jobset.Add( pnJobIds[i], DoPdeFit, NULL, 100 );
		}
		jobset.RouteJobs( &hostset );
		jobset.Clear();
	}*/
}

int SendDir( CClientSession* pSession, const char* strDir, bool bOverwrite )
{
	char buf[MAXPATH];
	sprintf( buf, "%s/*.*", strDir );

	struct _finddata_t fileinfo;
	intptr_t hfind = _findfirst( buf, &fileinfo );
	if( hfind==-1 )return S_NOENT;
	int nRet = S_OK;
	do{
		if( !(fileinfo.attrib & FILE_ATTRIBUTE_DIRECTORY) ){
			//make the file name for each file
			sprintf( buf, "%s/%s", strDir, fileinfo.name );
			nRet = pSession->PutFile( buf, fileinfo.name, bOverwrite );
		}
	}while( _findnext( hfind, &fileinfo )==0 );
	_findclose( hfind );
	return nRet;
}

void LinkDir( CClientSession* pSession, const char* strDir )
{
	char buf[MAXPATH];
	sprintf( buf, "%s/*.*", strDir );

	struct _finddata_t fileinfo;
	intptr_t hfind = _findfirst( buf, &fileinfo );
	if( hfind==-1 )return;
	do{
		if( !(fileinfo.attrib & FILE_ATTRIBUTE_DIRECTORY) ){
			//make the file name for each file
			sprintf( buf, "../%s", fileinfo.name );
			pSession->SoftLink( buf, fileinfo.name );
		}
	}while( _findnext( hfind, &fileinfo )==0 );
	_findclose( hfind );
}

int DoPdeFit( int nId, CClientSession* pSession, void* pParam )
{
	SYSINFO sysinfo;

	char buf[MAXPATH], path[MAXPATH];
	sprintf( path, "slave%d", nId );

	pSession->ChDir( "~" );
	pSession->MkDir( "uma_sga_coarse" );
	pSession->ChDir( "uma_sga_coarse" );
	pSession->GetSysInfo( &sysinfo );

	char* cmd = NULL;
	if( sysinfo.nOpSystem==SI_WIN32 ){
		cmd = "surrga.exe";
	}else if( sysinfo.nOpSystem==SI_LINUX ){
		cmd = "surrga.ln";
	}else if( sysinfo.nOpSystem==SI_UNIX ){
		cmd = "surrga.un";
	}

	//send all the static files in the static folder to the remote host
	pSession->FileMode( O_TEXT );
	int nRet = SendDir( pSession, "static", false );

	pSession->FileMode( O_BINARY );
	pSession->PutFile( cmd, cmd, false );
	pSession->ChMod( cmd, 0x700 );

	//for shared disk, make sub compuation folder and enter it.
	if( sysinfo.bShareDisk ){
		pSession->MkDir( path );
		pSession->ChDir( path );
	}

	//delete the surrbak.data file now
	pSession->Remove( "surrbak.dat" );

//	cdump<<lock<<nId<<"--check point 1"<<endl<<unlock;
	//send the files in the slave folder.
	pSession->FileMode( O_TEXT );
	SendDir( pSession, path, true );

	//for shared disk, make soft link in the computation folder
	if( sysinfo.bShareDisk ){
		LinkDir( pSession, "static" );
		char buf[100];
		sprintf( buf, "../%s", cmd );
		pSession->SoftLink( buf, cmd );
	}


	nRet = pSession->RunImage( cmd, true );
//	if( SUCCEEDED(nRet) ){
	if( nRet==0 ){
		//retrieve surrback.dat
		strcpy( buf, path );
		strcat( buf, "/surrbak.dat" );
		nRet = pSession->GetFile( "surrbak.dat", buf );
	}else{
		nRet = E_IMGEXIT;
	}

	if( sysinfo.bShareDisk ){
		//remove the intemediate files to save disk space.
		pSession->Remove( "optdemo_fine.ftl" );
		pSession->Remove( "optdemo_coarse.ftl" );
		pSession->Remove( "fort.30" );
		pSession->Remove( "fort.35" );
	}

	pSession->ChDir( "~" );

	return SUCCEEDED(nRet);
}

//a simple remove path with all the files in it. No sub directory is considered
int RemoveDir( char* strPath )
{
	//printf( "remove %s\n", strPath );

/*	char flname[MAXPATH];
	DIR* dirp = opendir(strPath);

	if( dirp==NULL ){
		return 0;
	}

	struct dirent * dp = readdir(dirp);
	while( dp ){
		//printf( "%s\n", dp->d_name );
		if( strcmp(dp->d_name, ".")!=0 && strcmp(dp->d_name, "..")!=0 ){
			strcpy( flname, strPath );
			NormalizePath( flname );
			strcat( flname, dp->d_name );
			if( remove(flname)!=0 ){
				closedir(dirp);
				return -1;
			}
		}
		dp = readdir(dirp);
	}

	closedir(dirp);*/
	return rmdir( strPath );
}

int CompareSize( char* strSrc, char* strDes )
{
	struct stat stsrc, stdes;
	if( stat(strSrc, &stsrc)!=0 )return -1;
	if( stat(strDes, &stdes)!=0 )return -1;

	if( stsrc.st_size==stdes.st_size )return 0;
	return 1;
}
int CopyFile( char* strSrc, char* strDes, int bForce )
{
	if( !bForce ){
		if( CompareSize(strSrc, strDes)==0 )return 0;
	}
	FILE* pfSrc = fopen( strSrc, "rb" );
	if( pfSrc==NULL )return -1;

	FILE* pfDes = fopen( strDes, "wb" );
	if( pfDes==NULL ){
		fclose(pfSrc);
		return -1;
	}

	char buf[BUFSIZE];
	while( !feof(pfSrc) ){
		int nRead = fread( buf, sizeof(char), BUFSIZE, pfSrc );
		if( nRead>0 ){
			fwrite( buf, sizeof(char), nRead, pfDes );
		}
	}
	fclose( pfSrc );
	fclose( pfDes );
	struct stat statsrc;
	stat( strSrc, &statsrc );
	chmod( strDes, statsrc.st_mode );
	return 0;
}

int CopyDir( char* strSrc, char* strDes )
{
	char strFullName[MAXPATH];

/*	mkdir( strDes, S_IRWXU | S_IRWXG | S_IROTH );

	DIR* dirp = opendir(strSrc);

	if( dirp==NULL ){
		return -1;
	}

	struct dirent * dp = readdir(dirp);
	while( dp ){
		strcpy( strFullName, strDes );
		NormalizePath( strFullName );
		char* p = ExtractFileName( dp->d_name );
		strcat( strFullName, p );
		if( CopyFile( dp->d_name, strFullName, TRUE )!=0 ){
			closedir(dirp);
			return -1;
		}

		dp = readdir(dirp);
	}

	closedir(dirp);*/

	return 0;
}

char* ExtractFileName( char* pszFileName )
{
	char* p = pszFileName + strlen(pszFileName);
	while( p>pszFileName && *p!='/' )p--;
	return p;
}

void NormalizePath( char* pszPath )
{
	char* p = pszPath + strlen( pszPath );
	if( *(p-1)!='/' ){
		strcat( pszPath, "/" );
	}
}

/* helper routine that does the main job. */

static void xtoa (
        unsigned long val,
        char *buf,
        unsigned radix,
        int is_neg
        )
{
        char *p;                /* pointer to traverse string */
        char *firstdig;         /* pointer to first digit */
        char temp;              /* temp char */
        unsigned digval;        /* value of digit */

        p = buf;

        if (is_neg) {
            /* negative, so output '-' and negate */
            *p++ = '-';
            val = (unsigned long)(-(long)val);
        }

        firstdig = p;           /* save pointer to first digit */

        do {
            digval = (unsigned) (val % radix);
            val /= radix;       /* get next digit */

            /* convert to ascii and store */
            if (digval > 9)
                *p++ = (char) (digval - 10 + 'a');  /* a letter */
            else
                *p++ = (char) (digval + '0');       /* a digit */
        } while (val > 0);

        /* We now have the digit of the number in the buffer, but in reverse
           order.  Thus we reverse them now. */

        *p-- = '\0';            /* terminate string; p points to last digit */

        do {
            temp = *p;
            *p = *firstdig;
            *firstdig = temp;   /* swap *p and *firstdig */
            --p;
            ++firstdig;         /* advance to next two digits */
        } while (firstdig < p); /* repeat until halfway */
}

/* Actual functions just call conversion helper with neg flag set correctly,
   and return pointer to buffer. */

char * _itoa (
        int val,
        char *buf,
        int radix
        )
{
        if (radix == 10 && val < 0)
            xtoa((unsigned long)val, buf, radix, 1);
        else
            xtoa((unsigned long)(unsigned int)val, buf, radix, 0);
        return buf;
}

/*
char* _tcsdec( char* start, char* current )
{
	return current>start ? current-1 : NULL;
}

char* _tcsinc( char* string )
{
	return string+1;
}


char* trimright( char* string )
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

char* trimleft( char* string )
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

char* trim( char* string )
{
	trimleft( string );
	return trimright( string );
}
*/