bin_SCRIPTS = 

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES = artanis/sql-mapping/built-in.scm \
          artanis/sql-mapping/mapping.scm \
          artanis/sql-mapping/fetcher.scm \
          artanis/sql-mapping/handlers.scm \
          artanis/websocket/named-pipe.scm \
          artanis/websocket/protocols.scm \
          artanis/websocket/handshake.scm \
          artanis/websocket/frame.scm \
          artanis/tpl/lexer.scm \
          artanis/tpl/parser.scm \
          artanis/tpl/sxml.scm \
          artanis/tpl/utils.scm \
          artanis/commands/migrate.scm \
          artanis/commands/help.scm \
          artanis/commands/api.scm \
          artanis/commands/version.scm \
          artanis/commands/create.scm \
          artanis/commands/work.scm \
          artanis/commands/draw.scm \
          artanis/webapi/restful.scm \
          artanis/security/nss.scm \
          artanis/server/http.scm \
          artanis/server/aio.scm \
          artanis/server/epoll.scm \
          artanis/server/proxy.scm \
          artanis/server/ragnarok.scm \
          artanis/server/server-context.scm \
          artanis/server/scheduler.scm \
          artanis/mvc/route.scm \
          artanis/mvc/controller.scm \
          artanis/mvc/model.scm \
          artanis/mvc/migration.scm \
          artanis/mvc/view.scm \
          artanis/third-party/redis/upstream/commands.scm \
          artanis/third-party/redis/upstream/main.scm \
          artanis/third-party/redis/upstream/connection.scm \
          artanis/third-party/redis/upstream/utils.scm \
          artanis/third-party/json/upstream/AUTHORS \
          artanis/third-party/json/upstream/COPYING \
          artanis/third-party/json/upstream/builder.scm \
          artanis/third-party/json/upstream/parser.scm \
          artanis/third-party/json/upstream/COPYING.LESSER \
          artanis/third-party/json/upstream/README \
          artanis/third-party/json/upstream/record.scm \
          artanis/third-party/json/upstream/README.org \
          artanis/third-party/csv.scm \
          artanis/third-party/json.scm \
          artanis/third-party/redis.scm \
          artanis/page.scm \
          artanis/cache.scm \
          artanis/oht.scm \
          artanis/tpl.scm \
          artanis/sendmail.scm \
          artanis/cookie.scm \
          artanis/commands.scm \
          artanis/irregex.scm \
          artanis/lpc.scm \
          artanis/env.scm \
          artanis/db.scm \
          artanis/route.scm \
          artanis/config.scm \
          artanis/sql-mapping.scm \
          artanis/server.scm \
          artanis/fprm.scm \
          artanis/mime.scm \
          artanis/debug.scm \
          artanis/utils.scm \
          artanis/ffi.scm \
          artanis/ssql.scm \
          artanis/inotify.scm \
          artanis/upload.scm \
          artanis/session.scm \
          artanis/websocket.scm \
          artanis/artanis.scm \
          artanis/version.scm \
          artanis.scm

TESTS = 

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

info_TEXINFOS = doc/artanis.texi
dvi: # Don't build dvi docs

EXTRA_DIST += NEWS \
              COPYING \
              HACKING \
              README \
              README.org \
              hall.scm \
              .gitignore \
              guix.scm \
              build-aux/test-driver.scm \
              $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)
