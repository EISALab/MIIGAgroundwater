#include "ofdump.h"
#include <stdio.h>
#include <stdarg.h>

#ifdef _WIN32
#include <windows.h>

static CRITICAL_SECTION _crit_lock;

#else
#include <strings.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/sem.h>
#include <pthread.h>

static pthread_mutex_t _crit_lock;
#endif

#define MAXSTR 1024

ofdump _thedump;

ofdump& GetGlobDump()
{
	return _thedump;
}

//constructor and destructor
ofdump::ofdump()
{
	bowner = false;
	out = &cout;

#ifdef _WIN32
	InitializeCriticalSection( &_crit_lock );
#else
	pthread_mutex_init( &_crit_lock, NULL );
#endif

}

ofdump::~ofdump()
{
	if( bowner )delete out;
#ifdef _WIN32
	DeleteCriticalSection( &_crit_lock );
#else
	pthread_mutex_destroy( &_crit_lock );
#endif
}

void ofdump::setcontext( ostream& of )
{
	lock();
	//release the old context
	flush();
	if( bowner )delete out;
	out = NULL;

	//attach to the new context
	out = &of;
	bowner = false;
	unlock();
}

void ofdump::setcontext( const char* strfile, int open_mode )
{
	lock();
	//release the old context
	flush();
	if( bowner )delete out;
	out = NULL;

	//attach to the new context
	out = new ofstream( strfile, open_mode );
	bowner = true;
	unlock();
}

void ofdump::lock()
{
#ifdef _WIN32
	EnterCriticalSection( &_crit_lock );
#else
	pthread_mutex_lock( &_crit_lock );
#endif
}

void ofdump::unlock()
{
#ifdef _WIN32
	LeaveCriticalSection( &_crit_lock );
#else
	pthread_mutex_unlock( &_crit_lock );
#endif
}


int __cdecl ofdump::printf( const char* format, ... )
{
	char string[MAXSTR];
	va_list arglist;

	va_start(arglist, format);
	int retval = sprintf( string, format, arglist );
	*out<<string;

	return(retval);
}


/*template<class _Elem, class _Traits> inline
	basic_ostream<_Elem, _Traits>&
		__cdecl endl(basic_ostream<_Elem, _Traits>& _Ostr)
	{	// insert newline and flush stream
	_Ostr.put(_Ostr.widen('\n'));
	_Ostr.flush();
	return (_Ostr);
	}


inline _CRTIMP ofdump& __cdecl flush(ofdump& _outs) { return _outs.flush(); }
inline _CRTIMP ofdump& __cdecl endl(ofdump& _outs) { return _outs << '\n' << flush; }
inline _CRTIMP ofdump& __cdecl ends(ofdump& _outs) { return _outs << char('\0'); }

_CRTIMP ios&           __cdecl dec(ios&);
_CRTIMP ios&           __cdecl hex(ios&);
_CRTIMP ios&           __cdecl oct(ios&);

inline ios_base& __cdecl dec(ios_base& _Iosbase)
	{	// set basefield to dec
	_Iosbase.setf(ios_base::dec, ios_base::basefield);
	return (_Iosbase);
	}

inline ios_base& __cdecl fixed(ios_base& _Iosbase)
	{	// set floatfield to fixed
	_Iosbase.setf(ios_base::fixed, ios_base::floatfield);
	return (_Iosbase);
	}

inline ios_base& __cdecl hex(ios_base& _Iosbase)
	{	// set basefield to hex
	_Iosbase.setf(ios_base::hex, ios_base::basefield);
	return (_Iosbase);
	}*/


//inline _CRTIMP ios&           __cdecl dec(ios&){  _outs<<dec; }
//inline _CRTIMP ios&           __cdecl hex(ios&){ return _outs<<hex; }
//inline _CRTIMP ios&           __cdecl oct(ios&){ return _outs<<oct; }
