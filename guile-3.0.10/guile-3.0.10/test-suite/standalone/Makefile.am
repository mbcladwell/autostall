## Process this file with automake to produce Makefile.in.
##
## Copyright 2003-2014, 2020-2024 Free Software Foundation, Inc.
##
## This file is part of GUILE.
##
##   GUILE is free software; you can redistribute it and/or modify it
##   under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##
##   You should have received a copy of the GNU Lesser General Public
##   License along with GUILE; see the file COPYING.LESSER.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA 02110-1301 USA

include $(top_srcdir)/am/snarf

# initializations so we can use += below.
TESTS =
noinst_LTLIBRARIES =
check_PROGRAMS = 
check_SCRIPTS =
BUILT_SOURCES =
EXTRA_DIST =

AM_TESTS_ENVIRONMENT =						\
  top_srcdir="$(top_srcdir)"					\
  srcdir="$(srcdir)"						\
  builddir="$(builddir)"					\
  @LOCALCHARSET_TESTS_ENVIRONMENT@				\
  GUILE_AUTO_COMPILE=0 "${top_builddir}/meta/build-env"

## Check for headers in $(srcdir) and build dir before $(CPPFLAGS), which
## may point us to an old, installed version of guile.
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir) \
	      -I$(top_srcdir)/lib -I$(top_builddir)/lib

test_cflags =					\
  -I$(top_srcdir)/test-suite/standalone -I.	\
  $(EXTRA_DEFS) $(GUILE_CFLAGS) $(GCC_CFLAGS)

AM_LDFLAGS = $(GUILE_CFLAGS)

# How to link with libguile (same as `Libs' in `guile-2.0.pc'.)
LIBGUILE_LDADD =							\
  ${top_builddir}/libguile/libguile-@GUILE_EFFECTIVE_VERSION@.la	\
  $(BDW_GC_LIBS)


snarfcppopts =								      \
  -I$(top_srcdir) -I$(top_srcdir)/lib -I$(top_builddir)/lib -I$(top_builddir) \
  -I. $(DEFS) $(DEFAULT_INCLUDES) $(CPPFLAGS) $(CFLAGS)

SUFFIXES = .x
.c.x:
	$(AM_V_SNARF)$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x

.DELETE_ON_ERROR:

check_SCRIPTS += test-system-cmds
TESTS += test-system-cmds

check_SCRIPTS += test-bad-identifiers
TESTS += test-bad-identifiers

check_SCRIPTS += test-require-extension
TESTS += test-require-extension

check_SCRIPTS += test-guile-snarf
TESTS += test-guile-snarf

check_SCRIPTS += test-import-order
TESTS += test-import-order
EXTRA_DIST += test-import-order-a.scm test-import-order-b.scm \
  test-import-order-c.scm test-import-order-d.scm

check_SCRIPTS += test-command-line-encoding
TESTS += test-command-line-encoding

check_SCRIPTS += test-command-line-encoding2
TESTS += test-command-line-encoding2

check_SCRIPTS += test-language
TESTS += test-language
EXTRA_DIST += test-language.el test-language.js

check_SCRIPTS += test-guild-compile
TESTS += test-guild-compile

check_SCRIPTS += test-sigaction-fork
TESTS += test-sigaction-fork

check_SCRIPTS += test-signal-fork
TESTS += test-signal-fork

# test-num2integral
test_num2integral_SOURCES = test-num2integral.c
test_num2integral_CFLAGS = ${test_cflags}
test_num2integral_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-num2integral
TESTS += test-num2integral

# test-round
test_round_CFLAGS = ${test_cflags}
test_round_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-round
TESTS += test-round

if HAVE_SHARED_LIBRARIES

# test-asmobs
noinst_LTLIBRARIES += libtest-asmobs.la
libtest_asmobs_la_SOURCES = test-asmobs-lib.c
libtest_asmobs_la_CFLAGS = ${test_cflags}
libtest_asmobs_la_LDFLAGS = -module -avoid-version -no-undefined -rpath `pwd` # so libtool will really build an .so
libtest_asmobs_la_LIBADD = $(LIBGUILE_LDADD)
BUILT_SOURCES += test-asmobs-lib.x
check_SCRIPTS += test-asmobs
TESTS += test-asmobs

# test-ffi
noinst_LTLIBRARIES += libtest-ffi.la
libtest_ffi_la_SOURCES = test-ffi-lib.c
libtest_ffi_la_CFLAGS = ${test_cflags}
libtest_ffi_la_LDFLAGS = -module -avoid-version -no-undefined -rpath `pwd` # so libtool will really build an .so
libtest_ffi_la_LIBADD = $(LIBGUILE_LDADD)
check_SCRIPTS += test-ffi
TESTS += test-ffi

endif HAVE_SHARED_LIBRARIES

# test-foreign-object-scm
check_SCRIPTS += test-foreign-object-scm
TESTS += test-foreign-object-scm

# test-foreign-object-c
test_foreign_object_c_SOURCES = test-foreign-object-c.c
test_foreign_object_c_CFLAGS = ${test_cflags}
test_foreign_object_c_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-foreign-object-c
TESTS += test-foreign-object-c

# test-list
test_list_SOURCES = test-list.c
test_list_CFLAGS = ${test_cflags}
test_list_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-list
TESTS += test-list

# test-unwind
test_unwind_SOURCES = test-unwind.c
test_unwind_CFLAGS = ${test_cflags}
test_unwind_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-unwind
TESTS += test-unwind

# test-conversion
test_conversion_SOURCES = test-conversion.c
test_conversion_CFLAGS = ${test_cflags}
test_conversion_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-conversion
TESTS += test-conversion

# test-hashing
test_hashing_SOURCES = test-hashing.c
test_hashing_CFLAGS = ${test_cflags}
test_hashing_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-hashing
TESTS += test-hashing

# test-loose-ends
test_loose_ends_SOURCES = test-loose-ends.c
test_loose_ends_CFLAGS = ${test_cflags}
test_loose_ends_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-loose-ends
TESTS += test-loose-ends

# test-fast-slot-ref
check_SCRIPTS += test-fast-slot-ref
TESTS += test-fast-slot-ref

# test-mb-regexp
check_SCRIPTS += test-mb-regexp
TESTS += test-mb-regexp

# test-use-srfi
check_SCRIPTS += test-use-srfi
TESTS += test-use-srfi

# test-scm-c-read
test_scm_c_read_SOURCES = test-scm-c-read.c
test_scm_c_read_CFLAGS = ${test_cflags}
test_scm_c_read_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-scm-c-read
TESTS += test-scm-c-read

# test-scm-take-locale-symbol
test_scm_take_locale_symbol_SOURCES = test-scm-take-locale-symbol.c
test_scm_take_locale_symbol_CFLAGS = ${test_cflags}
test_scm_take_locale_symbol_LDADD =			\
  $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-scm-take-locale-symbol
TESTS += test-scm-take-locale-symbol

# test-scm-take-u8vector
test_scm_take_u8vector_SOURCES = test-scm-take-u8vector.c
test_scm_take_u8vector_CFLAGS = ${test_cflags}
test_scm_take_u8vector_LDADD = $(LIBGUILE_LDADD)
check_PROGRAMS += test-scm-take-u8vector
TESTS += test-scm-take-u8vector

# test-scm-take-u8vector
test_scm_to_latin1_string_SOURCES = test-scm-to-latin1-string.c
test_scm_to_latin1_string_CFLAGS = ${test_cflags}
test_scm_to_latin1_string_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-scm-to-latin1-string
TESTS += test-scm-to-latin1-string

# test-scm-values
test_scm_values_SOURCES = test-scm-values.c
test_scm_values_CFLAGS = ${test_cflags}
test_scm_values_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-scm-values
TESTS += test-scm-values

# test-scm-c-bind-keyword-arguments
test_scm_c_bind_keyword_arguments_SOURCES = test-scm-c-bind-keyword-arguments.c
test_scm_c_bind_keyword_arguments_CFLAGS = ${test_cflags}
test_scm_c_bind_keyword_arguments_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-scm-c-bind-keyword-arguments
TESTS += test-scm-c-bind-keyword-arguments

# test-srfi-4
test_srfi_4_SOURCES = test-srfi-4.c
test_srfi_4_CFLAGS = ${test_cflags}
test_srfi_4_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-srfi-4
TESTS += test-srfi-4

if HAVE_SHARED_LIBRARIES

# test-extensions
noinst_LTLIBRARIES += libtest-extensions.la
libtest_extensions_la_SOURCES = test-extensions-lib.c
libtest_extensions_la_CFLAGS = ${test_cflags}
libtest_extensions_la_LDFLAGS = -module -avoid-version -no-undefined -rpath `pwd` # so libtool will really build an .so
libtest_extensions_la_LIBADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_SCRIPTS += test-extensions
TESTS += test-extensions

endif HAVE_SHARED_LIBRARIES

if BUILD_PTHREAD_SUPPORT

# test-with-guile-module
test_with_guile_module_CFLAGS = ${test_cflags}
test_with_guile_module_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-with-guile-module
TESTS += test-with-guile-module

test_scm_with_guile_CFLAGS = ${test_cflags}
test_scm_with_guile_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-scm-with-guile
TESTS += test-scm-with-guile

test_scm_spawn_thread_CFLAGS = ${test_cflags}
test_scm_spawn_thread_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-scm-spawn-thread
TESTS += test-scm-spawn-thread

test_pthread_create_CFLAGS = ${test_cflags}
test_pthread_create_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-pthread-create
TESTS += test-pthread-create

test_pthread_create_secondary_CFLAGS = ${test_cflags} $(BDW_GC_CFLAGS)
test_pthread_create_secondary_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-pthread-create-secondary
TESTS += test-pthread-create-secondary

else

EXTRA_DIST += test-with-guile-module.c test-scm-with-guile.c

endif

test_smob_mark_SOURCES = test-smob-mark.c
test_smob_mark_CFLAGS = ${test_cflags}
test_smob_mark_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-smob-mark
TESTS += test-smob-mark

test_smob_mark_race_SOURCES = test-smob-mark-race.c
test_smob_mark_race_CFLAGS = ${test_cflags}
test_smob_mark_race_LDADD = $(LIBGUILE_LDADD) $(top_builddir)/lib/libgnu.la
check_PROGRAMS += test-smob-mark-race
TESTS += test-smob-mark-race

check_SCRIPTS += test-stack-overflow
TESTS += test-stack-overflow

check_SCRIPTS += test-out-of-memory
TESTS += test-out-of-memory

check_SCRIPTS += test-close-on-exec
TESTS += test-close-on-exec

EXTRA_DIST += ${check_SCRIPTS}
