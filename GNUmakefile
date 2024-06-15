## GNUmakefile --- make file for CODATA recommended values

# Copyright (C) 2024 Ralph Schleicher

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#
#    * Neither the name of the copyright holder nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

## Code:

PACKAGE := codata-recommended-values
VERSION := $(shell cat VERSION)
TARNAME := $(PACKAGE)-$(VERSION)

BUILT_SOURCES = \
codata-2010.lisp \
codata-2014.lisp \
codata-2018.lisp \
codata-2022.lisp \
$(nil)

### Rules

%.lisp: %.lisp.in generate-code.lisp
	sbcl --non-interactive --load generate-code.lisp --load generate-$*.lisp

.PHONY: all
all: $(BUILT_SOURCES)

.PHONY: check
check: all
	quicklisp-check-build -sbcl -ccl $(PACKAGE)
	sbcl --non-interactive --eval '(asdf:test-system "$(PACKAGE)")'

.PHONY: clean
clean:
	rm -f $(BUILT_SOURCES)

### Maintenance

.PHONY: doc
doc:
	sbcl --non-interactive --load generate-doc.lisp

.PHONY: tag
tag: all
	@if test 0 != `svn status -q | grep -v "^ " | wc -l` ; then \
	    echo "Working copy is not clean" >&2 ; \
	    exit 1 ; \
	fi
	@if svn info "^/tags/$(TARNAME)" > /dev/null 2>&1 ; then \
	    echo "Tag already exists" >&2 ; \
	    exit 1 ; \
	fi
	svn copy "^/trunk" "^/tags/$(TARNAME)" -m "Version $(VERSION)."

.PHONY: sync
sync: all
	~/src/github/github.sh $(PACKAGE)

## GNUmakefile ends here
