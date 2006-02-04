ROOT_DIR=.
ROOT_OFFSET=.

include make.rules

LOCAL_INCLUDE = 

SUBDIRS = docs

all: $(TARGETS)

subdirs: $(SUBDIRS)
	@for d in $(SUBDIRS) ; do \
	    echo "Making in $$d" ; \
	    $(MAKE) -C $$d ; \
	done

always:
	@echo >/dev/null

