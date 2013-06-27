#pragma once

#include <iostream>
#include <ostream>
#include <fstream>
using namespace std;

class ofdump
{
public:
	ostream* out;
	bool bowner;

public:
	//constructor
	ofdump();
	virtual ~ofdump();

	void lock();
	void unlock();

	void setcontext( ostream& ostr );
	void setcontext( const char* strfile, int open_mode=ios::out );

	ofdump& flush()
	{	(*out).flush();	return *this; }

	int printf( const char*, ... );

	ofdump& operator<<(ofdump& (__cdecl * _f)(ofdump&))
	{	return (*_f)(*this); }

	ofdump& operator<<(ostream& (__cdecl * _f)(ostream&))
	{	(*_f)(*out); return *this; }

	ofdump& operator<<(ios& (__cdecl * _f)(ios&))
	{	(*_f)(*out); return *this; }

	ofdump& operator<<(const char * val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(const unsigned char * val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(const signed char * val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(char val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(unsigned char val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(signed char val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(short val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(unsigned short val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(int val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(unsigned int val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(long val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(unsigned long val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(float val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(double val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(long double val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(const void * val)
	{	*out<<val; return *this;	}

	ofdump& operator<<(streambuf* val)
	{	*out<<val; return *this;	}
};

inline _CRTIMP ofdump& __cdecl lock(ofdump& _outs) { _outs.flush(); _outs.lock(); return _outs; }
inline _CRTIMP ofdump& __cdecl unlock(ofdump& _outs) { _outs.flush(); _outs.unlock(); return _outs; }

ofdump& GetGlobDump();
#define cdump	GetGlobDump()