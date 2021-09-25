
SRC=mini
DST=gen-platform

all:
	cp -R ../$(SRC) ../$(DST)

	mkdir -p ../$(DST)/rtl/misc
	for f in rtl/*.vhd rtl/misc/*.vhd; do (cp -R $${f} "../$(DST)/$${f}"); done
	for c in c/*platform-bindings.c ;  do (cat $${c} >> ../$(DST)/c/platform-bindings.c); done
	for c in c/*platform.c;            do (cat $${c} >> ../$(DST)/c/platform.c); done
	for c in c/*platform.h;            do (cat $${c} >> ../$(DST)/c/platform.h); done
	for c in c/*simul.c;               do (cat $${c} >> ../$(DST)/c/simul.c); done
	for c in c/*simul.h;               do (cat $${c} >> ../$(DST)/c/simul.h); done
	for c in ml/*platform.ml;          do (cat $${c} >> ../$(DST)/ml/platform.ml); done
	for c in ml/*platform.mli;         do (cat $${c} >> ../$(DST)/ml/platform.mli); done
	for f in apps; do (cp -R $${f} "../$(DST)/$${f}"); done

	for f in apps/*.ml ; do \
	  D=../$(DST)/apps/`basename $${f} ".ml"`;\
	  mkdir -p $${D};\
	  cat $${f} > "$${D}/main.ml";\
	  echo "PLATFORM_DIR=../..\nOMICROB_OPTS=-no-shortcut-initialization" > "$${D}/config";\
	  echo "../../../_etc/make_makefiles" > "$${D}/make_makefiles";\
	  chmod 755 "$${D}/make_makefiles";\
	done

	for f in qsys/*_hw.tcl; do (cp -R $${f} "../$(DST)/$${f}"); done
	cat qsys/platform.tcl > "../$(DST)/qsys/platform_hw.tcl"
	for f in qsys/*_ext.tcl; do (cat $${f} >> "../$(DST)/qsys/platform_hw.tcl"); done
	echo "save_system platform.qsys" >> "../$(DST)/qsys/platform_hw.tcl"

	# '-' ignore les erreurs
	-(cd ../$(DST)/qsys/ ; rm -R platform.qsys; qsys-script --script=platform_hw.tcl) 

	qsys-generate --synthesis=VHDL ../$(DST)/qsys/platform.qsys

	(cd ../$(DST) ; make hw-build ; make omicrob ; make bsp)

clean:
	rm -Rf c ml rtl
	rm -f  rtl/*.vhd rtl/misc/*.vhd c/*.c  c/*.h  ml/*.ml  ml/*.mli qsys/*_hw.tcl qsys/*_ext.tcl
	rm -Rf `find apps -mindepth 1`

check:
	ghdl -a -fno-color-diagnostics rtl/misc/*.vhd rtl/*.vhd;
	rm -f *.o *.cf