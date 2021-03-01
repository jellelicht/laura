dnl SPDX-FileCopyrightText: 2017 Roel Janssen <roel@gnu.org>
dnl SPDX-FileCopyrightText: 2019-2021 David Thompson <davet@gnu.org>
dnl
dnl SPDX-License-Identifier: GPL-3.0-or-later

AC_INIT([laura],
  [0.1.0],
  [jlicht@posteo.net], [laura],
  [https://github.com/jellelicht/laura])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([3.0 2.2])
GUILE_PROGS

if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your Guile installation.])
fi

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_PATH_PROG([SED], [sed])
if test "x$SED" = "x"; then
   AC_MSG_ERROR(['sed' could not be found; it is needed during the build.])
fi

AC_ARG_VAR(GUILE_LOAD_PATH, [The Guile load path to be embedded in the main script])
AC_ARG_VAR(GUILE_LOAD_COMPILED_PATH, [The Guile load path for compiled modules to be embedded in the main script])

AC_CONFIG_FILES([Makefile laura/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
