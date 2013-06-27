# Microsoft Developer Studio Project File - Name="surrga" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=surrga - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "surrga.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "surrga.mak" CFG="surrga - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "surrga - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "surrga - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "surrga - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "surrga - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ  /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "../helper" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ  /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "surrga - Win32 Release"
# Name "surrga - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\amg1r5.f
# End Source File
# Begin Source File

SOURCE=.\ctime.f
# End Source File
# Begin Source File

SOURCE=.\de45.f
# End Source File
# Begin Source File

SOURCE=.\fhb1.f
# End Source File
# Begin Source File

SOURCE=.\gage5.f
# End Source File
# Begin Source File

SOURCE=.\glo1bas6.f
DEP_F90_GLO1B=\
	".\openspec.inc"\
	".\parallel.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1bas6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1bcf6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1chd6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1drn6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1drt1.f
DEP_F90_GWF1D=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1ets1.f
# End Source File
# Begin Source File

SOURCE=.\gwf1evt6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1ghb6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1hfb6.f
DEP_F90_GWF1H=\
	".\openspec.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1huf1.f
DEP_F90_GWF1HU=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1ibs6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1lpf1.f
DEP_F90_GWF1L=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1rch6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1riv6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1str6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1wel6.f
# End Source File
# Begin Source File

SOURCE=.\hydmod.f
DEP_F90_HYDMO=\
	".\hydmod.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\lak3.f
# End Source File
# Begin Source File

SOURCE=.\lmg1.f
DEP_F90_LMG1_=\
	".\parallel.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\lmt6.f
DEP_F90_LMT6_=\
	".\openspec.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\main.f90
NODEP_F90_MAIN_=\
	".\Release\surrfit.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\memchk.f
# End Source File
# Begin Source File

SOURCE=.\mf2k.f
DEP_F90_MF2K_=\
	".\lmt6.inc"\
	".\openspec.inc"\
	".\parallel.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\mt3dms4.for
# End Source File
# Begin Source File

SOURCE=.\mt_adv4.for
# End Source File
# Begin Source File

SOURCE=.\mt_btn4.for
# End Source File
# Begin Source File

SOURCE=.\mt_dsp4.for
# End Source File
# Begin Source File

SOURCE=.\mt_fmi4.for
# End Source File
# Begin Source File

SOURCE=.\mt_gcg4.for
# End Source File
# Begin Source File

SOURCE=.\mt_rct4.for
# End Source File
# Begin Source File

SOURCE=.\mt_ssm4.for
# End Source File
# Begin Source File

SOURCE=.\mt_utl4.for
DEP_F90_MT_UT=\
	".\filespec.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\objfunc.f90
NODEP_F90_OBJFU=\
	".\Release\PARAMS.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\objfunc_coarse.f90
NODEP_F90_OBJFUN=\
	".\Release\PARAMS.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1adv2.f
DEP_F90_OBS1A=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1bas6.f
DEP_F90_OBS1B=\
	".\parallel.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1drn6.f
DEP_F90_OBS1D=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1drt1.f
DEP_F90_OBS1DR=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1ghb6.f
DEP_F90_OBS1G=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1riv6.f
DEP_F90_OBS1R=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1str6.f
DEP_F90_OBS1S=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=".\para-non.f"
DEP_F90_PARA_=\
	".\parallel.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\params.f90
# End Source File
# Begin Source File

SOURCE=.\parutl1.f
DEP_F90_PARUT=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\pcg2.f
# End Source File
# Begin Source File

SOURCE=.\pes1bas6.f
DEP_F90_PES1B=\
	".\openspec.inc"\
	".\parallel.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\pes1gau1.f
DEP_F90_PES1G=\
	".\parallel.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\res1.f
# End Source File
# Begin Source File

SOURCE=.\sen1bas6.f
DEP_F90_SEN1B=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1chd6.f
DEP_F90_SEN1C=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1drn6.f
DEP_F90_SEN1D=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1drt1.f
DEP_F90_SEN1DR=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1ets1.f
DEP_F90_SEN1E=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1evt6.f
DEP_F90_SEN1EV=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1ghb6.f
DEP_F90_SEN1G=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1hfb6.f
DEP_F90_SEN1H=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1huf1.f
DEP_F90_SEN1HU=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1lpf1.f
DEP_F90_SEN1L=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1rch6.f
DEP_F90_SEN1R=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1riv6.f
DEP_F90_SEN1RI=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1str6.f
DEP_F90_SEN1S=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1wel6.f
DEP_F90_SEN1W=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sip5.f
# End Source File
# Begin Source File

SOURCE=.\sor5.f
# End Source File
# Begin Source File

SOURCE=..\helper\StdHelper.f90
# End Source File
# Begin Source File

SOURCE=.\surrfit.f90
NODEP_F90_SURRF=\
	".\Release\surrio.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\surrio.f90
NODEP_F90_SURRI=\
	".\Release\STD_HELPER.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\utl6.f
DEP_F90_UTL6_=\
	".\openspec.inc"\
	".\param.inc"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
