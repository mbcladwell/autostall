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

SOURCES = myapp/db/sm/.gitkeep \
          myapp/db/migration/.gitkeep \
          myapp/db/.gitkeep \
          myapp/log/.gitkeep \
          myapp/prv/.gitkeep \
          myapp/lib/.gitkeep \
          myapp/lib/mylib.scm \
          myapp/app/views/intro/page1.html.tpl \
          myapp/app/views/intro/intropage1.html.tpl \
          myapp/app/views/pages/page1.html.tpl \
          myapp/app/views/.gitkeep \
          myapp/app/models/.gitkeep \
          myapp/app/controllers/pages.scm \
          myapp/app/controllers/.gitkeep \
          myapp/app/protocols/.gitkeep \
          myapp/app/api/v1.scm \
          myapp/app/.gitkeep \
          myapp/sys/i18n/.gitkeep \
          myapp/sys/pages/.gitkeep \
          myapp/sys/.gitkeep \
          myapp/pub/img/upload/.gitkeep \
          myapp/pub/img/.gitkeep \
          myapp/pub/css/navbar.css \
          myapp/pub/css/.gitkeep \
          myapp/pub/css/common.scss \
          myapp/pub/js/.gitkeep \
          myapp/pub/footer.tpl \
          myapp/pub/header.tpl \
          myapp/pub/.gitkeep \
          myapp/pub/favicon.ico \
          myapp/tmp/cache/tpl/home/mbc/projects/myapp/myapp/app/views/pages/page1.html.tpl.cache \
          myapp/tmp/cache/tpl/home/mbc/temp/artanmod/artanmod/app/views/intro/page1.html.tpl.cache \
          myapp/tmp/cache/tpl/home/mbc/temp/artanmod/app/views/intro/intropage1.html.tpl.cache \
          myapp/tmp/cache/tpl/home/mbc/temp/myapp/myapp/app/views/pages/page1.html.tpl.cache \
          myapp/tmp/cache/migration/.gitkeep \
          myapp/tmp/cache/README \
          myapp/tmp/cache/.gitkeep \
          myapp/tmp/cache/route.cache \
          myapp/tmp/.gitkeep \
          myapp/test/benchmark/README \
          myapp/test/benchmark/.gitkeep \
          myapp/test/functional/.gitkeep \
          myapp/test/unit/.gitkeep \
          myapp/test/.gitkeep \
          myapp/conf/README \
          myapp/conf/.gitkeep \
          myapp/conf/artanis.conf \
          myapp/update-artanis.sh \
          myapp/.route \
          myapp/README \
          myapp/ENTRY \
          myapp/mv-artanis.sh \
          myapp/install.sh

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

info_TEXINFOS = doc/myapp.texi
dvi: # Don't build dvi docs

EXTRA_DIST += AUTHORS \
              NEWS \
              COPYING \
              HACKING \
              README \
              README.org \
              hall.scm \
              .gitignore \
              guix.scm \
              build-aux/test-driver.scm \
              ./scripts/*.sh \
              $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)
