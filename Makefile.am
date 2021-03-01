# SPDX-FileCopyrightText: 2017-2018 Roel Janssen <roel@gnu.org>
# SPDX-FileCopyrightText: 2018-2021 David Thompson <davet@gnu.org>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

include guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =                                                 \
  laura/config.scm                                        \
  laura/main.scm                                          \
  laura/utils.scm

extensionsdir = $(prefix)/share/guix/extensions
dist_extensions_DATA =                                    \
  guix/extensions/laura.scm

TEST_EXTENSIONS = .scm

SCM_TESTS = \
  tests/examples.scm

TESTS = $(SCM_TESTS)

EXTRA_DIST += $(TESTS)

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" GUILE_AUTO_COMPILE=0

SCM_LOG_DRIVER =                                          \
  $(top_builddir)/pre-inst-env                            \
  $(GUILE) --no-auto-compile -e main                      \
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

EXTRA_DIST +=      \
  guix/extensions/laura.scm.in \
  pre-inst-env.in           \
  build-aux/test-driver.scm

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE_LOAD_COMPILED_PATH[@],$(GUILE_LOAD_COMPILED_PATH),g'				\
  -e 's,[@]GUILE_LOAD_PATH[@],$(GUILE_LOAD_PATH),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]abs_top_srcdir[@],$(abs_top_srcdir),g'	\
  -e 's,[@]abs_top_builddir[@],$(abs_top_builddir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

guix/extensions/laura.scm: guix/extensions/laura.scm.in Makefile
	$(AM_V_at)rm -f $@ $@-t
	$(AM_V_at)$(MKDIR_P) "$(@D)"
	$(AM_V_GEN)$(do_subst) < "$(srcdir)/$@.in" > "$@-t"
	mv -f "$@-t" "$@"

dist-hook: $(distdir)/ChangeLog
gen-ChangeLog $(distdir)/ChangeLog:
	$(AM_V_GEN)if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t;							\
	  rm -f $(distdir)/ChangeLog;					\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;		\
	fi

.PHONY: gen-ChangeLog

CLEANFILES += \
  guix/extensions/laura.scm                      \
  $(SCM_TESTS:tests/%.scm=%.log)
