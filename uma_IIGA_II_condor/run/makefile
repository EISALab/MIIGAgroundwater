F90C = mpxlf
F90 =  mpxlf90 -qsuffix=f=f90

OBJECTS = params.o \
uma.o \
objfunc.o \
objfunc_coarse.o\
mf2k.o \
mt3dms4.o \
amg1r5.o \
ctime.o \
de45.o \
fhb1.o \
gage5.o \
glo1bas6.o \
gwf1bas6.o \
gwf1bcf6.o \
gwf1chd6.o \
gwf1drn6.o \
gwf1drt1.o \
gwf1ets1.o \
gwf1evt6.o \
gwf1ghb6.o \
gwf1hfb6.o \
gwf1huf1.o \
gwf1ibs6.o \
gwf1lpf1.o \
gwf1rch6.o \
gwf1riv6.o \
gwf1str6.o \
gwf1wel6.o \
hydmod.o \
lak3.o \
lmg1.o \
lmt6.o \
memchk.o \
mt_adv4.o \
mt_btn4.o \
mt_dsp4.o \
mt_fmi4.o \
mt_gcg4.o \
mt_rct4.o \
mt_ssm4.o \
mt_utl4.o \
obs1adv2.o \
obs1bas6.o \
obs1drn6.o \
obs1drt1.o \
obs1ghb6.o \
obs1riv6.o \
obs1str6.o \
para-non.o \
parutl1.o \
pcg2.o \
pes1bas6.o \
pes1gau1.o \
res1.o \
sen1bas6.o \
sen1chd6.o \
sen1drn6.o \
sen1drt1.o \
sen1ets1.o \
sen1evt6.o \
sen1ghb6.o \
sen1hfb6.o \
sen1huf1.o \
sen1lpf1.o \
sen1rch6.o \
sen1riv6.o \
sen1str6.o \
sen1wel6.o \
sip5.o \
sor5.o \
utl6.o 


sga : $(OBJECTS)
	$(F90C) -o sga $(OBJECTS)
params.o: params.f90
	$(F90) -c params.f90  -qsuffix=f=f90
uma.o: uma.f90
	$(F90) -c uma.f90  -qsuffix=f=f90
objfunc.o: objfunc.f90
	$(F90) -c objfunc.f90  -qsuffix=f=f90
objfunc_coarse.o: objfunc_coarse.f90
	$(F90) -c objfunc_coarse.f90  -qsuffix=f=f90
mf2k.o: mf2k.f
	$(F90C) -c mf2k.f
mt3dms4.o: mt3dms4.for
	$(F90C) -c mt3dms4.for -qsuffix=f=for
amg1r5.o: amg1r5.f
	$(F90C) -c amg1r5.f
ctime.o: ctime.f
	$(F90C) -c ctime.f
de45.o: de45.f
	$(F90C) -c de45.f
fhb1.o: fhb1.f
	$(F90C) -c fhb1.f
gage5.o: gage5.f
	$(F90C) -c gage5.f
glo1bas6.o: glo1bas6.f
	$(F90C) -c glo1bas6.f
gwf1bas6.o: gwf1bas6.f
	$(F90C) -c gwf1bas6.f
gwf1bcf6.o: gwf1bcf6.f
	$(F90C) -c gwf1bcf6.f
gwf1chd6.o: gwf1chd6.f
	$(F90C) -c gwf1chd6.f
gwf1drn6.o: gwf1drn6.f
	$(F90C) -c gwf1drn6.f
gwf1drt1.o: gwf1drt1.f
	$(F90C) -c gwf1drt1.f
gwf1ets1.o: gwf1ets1.f
	$(F90C) -c gwf1ets1.f
gwf1evt6.o:gwf1evt6.f
	$(F90C) -c gwf1evt6.f
gwf1ghb6.o: gwf1ghb6.f
	$(F90C) -c gwf1ghb6.f
gwf1hfb6.o: gwf1hfb6.f
	$(F90C) -c gwf1hfb6.f
gwf1huf1.o: gwf1huf1.f
	$(F90C) -c gwf1huf1.f
gwf1ibs6.o: gwf1ibs6.f
	$(F90C) -c gwf1ibs6.f
gwf1lpf1.o: gwf1lpf1.f
	$(F90C) -c gwf1lpf1.f
gwf1rch6.o: gwf1rch6.f
	$(F90C) -c gwf1rch6.f
gwf1riv6.o: gwf1riv6.f
	$(F90C) -c gwf1riv6.f
gwf1str6.o: gwf1str6.f
	$(F90C) -c gwf1str6.f
gwf1wel6.o: gwf1wel6.f
	$(F90C) -c gwf1wel6.f
hydmod.o: hydmod.f
	$(F90C) -c hydmod.f
lak3.o: lak3.f
	$(F90C) -c lak3.f
lmg1.o: lmg1.f
	$(F90C) -c lmg1.f
lmt6.o: lmt6.f
	$(F90C) -c lmt6.f
memchk.o: memchk.f
	$(F90C) -c memchk.f
mt_adv4.o: mt_adv4.for
	$(F90C) -c mt_adv4.for -qsuffix=f=for
mt_btn4.o: mt_btn4.for
	$(F90C) -c mt_btn4.for -qsuffix=f=for
mt_dsp4.o: mt_dsp4.for
	$(F90C) -c mt_dsp4.for -qsuffix=f=for
mt_fmi4.o: mt_fmi4.for
	$(F90C) -c mt_fmi4.for -qsuffix=f=for
mt_gcg4.o: mt_gcg4.for
	$(F90C) -c mt_gcg4.for -qsuffix=f=for
mt_rct4.o: mt_rct4.for
	$(F90C) -c mt_rct4.for -qsuffix=f=for
mt_ssm4.o: mt_ssm4.for
	$(F90C) -c mt_ssm4.for -qsuffix=f=for
mt_utl4.o: mt_utl4.for
	$(F90C) -c mt_utl4.for -qsuffix=f=for
obs1adv2.o: obs1adv2.f
	$(F90C) -c obs1adv2.f
obs1bas6.o: obs1bas6.f
	$(F90C) -c obs1bas6.f
obs1drn6.o: obs1drn6.f
	$(F90C) -c obs1drn6.f
obs1drt1.o: obs1drt1.f
	$(F90C) -c obs1drt1.f
obs1ghb6.o: obs1ghb6.f
	$(F90C) -c obs1ghb6.f
obs1riv6.o: obs1riv6.f
	$(F90C) -c obs1riv6.f
obs1str6.o: obs1str6.f
	$(F90C) -c obs1str6.f
para-non.o: para-non.f
	$(F90C) -c para-non.f
parutl1.o: parutl1.f
	$(F90C) -c parutl1.f
pcg2.o: pcg2.f
	$(F90C) -c pcg2.f
pes1bas6.o: pes1bas6.f
	$(F90C) -c pes1bas6.f
pes1gau1.o: pes1gau1.f
	$(F90C) -c pes1gau1.f
res1.o: res1.f
	$(F90C) -c res1.f
sen1bas6.o: sen1bas6.f
	$(F90C) -c sen1bas6.f
sen1chd6.o: sen1chd6.f
	$(F90C) -c sen1chd6.f
sen1drn6.o: sen1drn6.f
	$(F90C) -c sen1drn6.f
sen1drt1.o: sen1drt1.f
	$(F90C) -c sen1drt1.f
sen1ets1.o: sen1ets1.f
	$(F90C) -c sen1ets1.f
sen1evt6.o: sen1evt6.f
	$(F90C) -c sen1evt6.f
sen1ghb6.o: sen1ghb6.f
	$(F90C) -c sen1ghb6.f
sen1hfb6.o: sen1hfb6.f
	$(F90C) -c sen1hfb6.f
sen1huf1.o: sen1huf1.f
	$(F90C) -c sen1huf1.f
sen1lpf1.o: sen1lpf1.f
	$(F90C) -c sen1lpf1.f
sen1rch6.o: sen1rch6.f
	$(F90C) -c sen1rch6.f
sen1riv6.o: sen1riv6.f
	$(F90C) -c sen1riv6.f
sen1str6.o: sen1str6.f
	$(F90C) -c sen1str6.f
sen1wel6.o: sen1wel6.f
	$(F90C) -c sen1wel6.f
sip5.o: sip5.f
	$(F90C) -c sip5.f
sor5.o: sor5.f
	$(F90C) -c sor5.f
utl6.o: utl6.f
	$(F90C) -c utl6.f


#clean: rm -f *.o










