
TARGETS = [.lib]*.c [.lib]*.h [.lib]*.finc -
        [.lib]*.mms [.lib]makefile.* [.lib]*.txt -
        [.util]*.c [.util]*.h -
        [.util]*.mms [.util]*.opt [.util]makefile.*

all : mud.zip
    @ continue

mud.zip :
    zip $@ $(TARGETS)

mud_dos.zip :
    zip -kl $@ $(TARGETS)

