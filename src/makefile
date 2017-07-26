MAKE=/usr/bin/make
INSTALLDIR=../bin/
TESTDIR=../test/

all: 
	@echo " Invoking gfspack/makefile"
	(cd gfspack; $(MAKE))

	@echo " Invoking ShortTermTrack/makefile"
	(cd ShortTermTrack; $(MAKE))

	@echo " Invoking afdeck/makefile"
	(cd afdeck; $(MAKE))

	@echo " Invoking satcenter/makefile"
	(cd satcenter; $(MAKE))

	@echo " Invoking oparet/makefile"
	(cd oparet/oparet; $(MAKE))

install:
	cp gfspack/bin2pack.x $(INSTALLDIR)/bin2pack.x
	cp ShortTermTrack/ShortTermTrack_mirs.x $(INSTALLDIR)/ShortTermTrack_mirs.x
	cp afdeck/afdeck.x $(INSTALLDIR)/afdeck.x
	cp satcenter/satcenter.x $(INSTALLDIR)/satcenter.x
	cp oparet/oparet/oparet.x $(INSTALLDIR)/oparet.x
	cp gfspack/bin2pack.x $(TESTDIR)/gfspack/bin2pack.x
	cp ShortTermTrack/ShortTermTrack_mirs.x $(TESTDIR)/ShortTermTrack/ShortTermTrack_mirs.x
	cp afdeck/afdeck.x $(TESTDIR)/afdeck/afdeck.x
	cp satcenter/satcenter.x $(TESTDIR)/satcenter/satcenter.x
	cp oparet/oparet/oparet.x $(TESTDIR)/oparet/oparet.x

clean:
	@echo " Cleaning gfspack/"
	@(cd gfspack; $(MAKE) clean) 

	@echo " Cleaning ShortTermTrack/"
	@(cd ShortTermTrack; $(MAKE) clean) 

	@echo " Cleaning afdeck/"
	@(cd afdeck; $(MAKE) clean) 

	@echo " Cleaning satcenter/"
	(cd satcenter; $(MAKE) clean)

	@echo " Cleaning oparet/"
	(cd oparet/oparet; $(MAKE) clean)

uninstall:
	@echo " Uninstalling..."
	@(cd $(INSTALLDIR); rm bin2pack.x ShortTermTrack_mirs.x afdeck.x satcenter.x oparet.x)
	rm $(TESTDIR)/gfspack/bin2pack.x
	rm $(TESTDIR)/ShortTermTrack/ShortTermTrack_mirs.x
	rm $(TESTDIR)/afdeck/afdeck.x
	rm $(TESTDIR)/satcenter/satcenter.x
	rm $(TESTDIR)/oparet/oparet.x
        
