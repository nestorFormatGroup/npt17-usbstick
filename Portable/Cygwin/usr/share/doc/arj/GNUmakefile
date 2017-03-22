#
# $Id: makefile.in,v 1.14 2004/04/17 11:28:07 andrew_belov Exp $
# ----------------------------------------------------------------------------
# This file is for using the GNU ways to compile ARJ.
#

# Environment

x = 
d = .so
OS_ID = UNIX
SHELL = /bin/sh
DYN_LIBS = 
LIBS = 
REGISTER = arj-register

# Programs

INSTALL = /bin/install -c
INSTALL_DIR = /bin/install -c -d -m 755
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}

# Installation Paths

prefix = /usr
exec_prefix = /usr
bindir = ${exec_prefix}/bin
datadir = ${prefix}/share
mandir = ${prefix}/share/man
libdir = ${prefix}/lib

pkglibdir = $(libdir)/arj

# Defaults

ifndef LOCALE
LOCALE = en
endif

ALL_CFLAGS = -DLOCALE=LANG_$(LOCALE) -DLOCALE_DESC="\"$(LOCALE)\"" \
	     -DPKGLIBDIR="\"$(pkglibdir)\"" -D_UNIX

ifndef COMMERCIAL
PACKAGE = s
else
PACKAGE = c
ALL_CFLAGS += -DCOMMERCIAL
endif

ifdef DEBUG
DEBUG_SM = d
ALL_CFLAGS += -DDEBUG
else
DEBUG_SM = r
endif

ifdef BUILD_STRIP
STRIP = strip --strip-unneeded
else
STRIP = :
endif

ALL_CFLAGS +=  $(CPPFLAGS) -g -O2 $(CFLAGS)

# Build Installation paths

SRC_DIR = .
BASEDIR = $(SRC_DIR)/cygwin/$(LOCALE)/$(DEBUG_SM)$(PACKAGE)
BASEDIR_C = $(SRC_DIR)/cygwin/$(LOCALE)/$(DEBUG_SM)c
RES_DIR = $(SRC_DIR)/resource
RESFILE = $(RES_DIR)/resource.txt

TOOLS_DIR = $(BASEDIR)/tools
ARJ_DIR = $(BASEDIR)/arj
ARJSFXV_DIR = $(BASEDIR)/arjsfxv
ARJSFX_DIR = $(BASEDIR)/arjsfx
ARJSFXJR_DIR = $(BASEDIR)/arjsfxjr
REARJ_DIR = $(BASEDIR)/rearj
ARJCRYPT_DIR = $(BASEDIR)/arjcrypt
REGISTER_DIR = $(BASEDIR)/register
ARJDISP_DIR = $(BASEDIR)/arjdisp
SFXSTUB_DIR = $(BASEDIR)/sfxstub

# Definitions for compiling submodules

LDFLAGS =  $(ADD_LDFLAGS)
DLL_FLAGS = -shared $(LDFLAGS)
DLL_CFLAGS = -fPIC
REQUIRES_DEF = 

COPT = -c -Icygwin -I$(BASEDIR) -I$(SRC_DIR) $(ALL_CFLAGS)
STD_COPT = -DARJUTIL $(COPT)
ARJ_COPT = -DSFL=4 $(COPT)
ARJSFXV_COPT = -DSFL=3 $(COPT)
ARJSFX_COPT = -DSFL=2 $(COPT)
ARJSFXJR_COPT = -DSFL=1 $(COPT)
REARJ_COPT = -DREARJ $(COPT)
REGISTER_COPT = -DREGISTER $(COPT)
ARJDISP_COPT = -DARJDISP $(COPT)
ARJCRYPT_COPT = $(DLL_CFLAGS) $(COPT)
SFXSTUB_COPT = -DSFXSTUB $(COPT)

# Linkage

$(ARJ_DIR)/%.o: $(BASEDIR)/%.c
	$(CC) $(COPT) -o$@ $<

$(ARJSFX_DIR)/%.o: $(BASEDIR)/%.c
	$(CC) $(COPT) -o$@ $<

$(ARJSFXV_DIR)/%.o: $(BASEDIR)/%.c
	$(CC) $(COPT) -o$@ $<

$(ARJSFXJR_DIR)/%.o: $(BASEDIR)/%.c
	$(CC) $(COPT) -o$@ $<

$(ARJCRYPT_DIR)/%.o: $(BASEDIR)/%.c
	$(CC) $(ARJCRYPT_COPT) -o$@ $<

$(SFXSTUB_DIR)/%.o: $(BASEDIR)/%.c
	$(CC) $(COPT) -o$@ $<

$(REARJ_DIR)/%.o: $(BASEDIR)/%.c
	$(CC) $(COPT) -o$@ $<

$(REGISTER_DIR)/%.o: $(BASEDIR)/%.c
	$(CC) $(COPT) -o$@ $<

$(ARJDISP_DIR)/%.o: $(BASEDIR)/%.c
	$(CC) $(COPT) -o$@ $<

$(TOOLS_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(STD_COPT) -o$@ $<

$(ARJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(ARJ_COPT) -o$@ $<

$(ARJSFXV_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(ARJSFXV_COPT) -o$@ $<

$(ARJSFX_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(ARJSFX_COPT) -o$@ $<

$(ARJSFXJR_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(ARJSFXJR_COPT) -o$@ $<

$(ARJCRYPT_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(ARJCRYPT_COPT) -o$@ $<

$(REARJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(REARJ_COPT) -o$@ $<

$(REGISTER_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(REGISTER_COPT) -o$@ $<

$(ARJDISP_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(ARJDISP_COPT) -o$@ $<

$(SFXSTUB_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(SFXSTUB_COPT) -o$@ $<

#
# Main dependency tree
#

.PHONY: timestamp msg-headers depends prepare clean package help

ifdef COMMERCIAL
MAKE_KEY=$(TOOLS_DIR)/make_key$x
endif

all:	prepare timestamp
	$(MAKE) msg-headers
	$(MAKE) depends
	$(MAKE) do-all

do-all:	\
	$(ARJ_DIR)/arj$x \
	$(ARJCRYPT_DIR)/arjcrypt$d \
	$(REARJ_DIR)/rearj$x \
	$(REGISTER_DIR)/$(REGISTER)$x \
	$(ARJDISP_DIR)/arjdisp$x \
	$(TOOLS_DIR)/packager$x \
	$(MAKE_KEY)

MSG_ID := \
	msg_crp msg_stb msg_sfv msg_sfx msg_sfj msg_arj msg_rej msg_reg msg_adi
MSG_HEADERS := $(patsubst %,$(BASEDIR)/%.h,$(MSG_ID))

msg-headers: $(MSG_HEADERS)

.deps:
	mkdir -p $@

.deps/%.d: %.c .deps
	$(CC) $(COPT) $< -MM > $@

SOURCES = $(wildcard *.c)
DEPS = $(addprefix .deps/,$(SOURCES:.c=.d))

ifeq ($(sort $(DEPS)),$(sort $(wildcard .deps/*.d)))
include $(DEPS)
endif

depends: $(DEPS)

#
# Update timestamp file
#

timestamp: $(TOOLS_DIR)/today$x
	$(TOOLS_DIR)/today$x $(LOCALE) $(BASEDIR)

#
# The tools
#

MSGBIND_OBJS = $(patsubst %,$(TOOLS_DIR)/%, \
               filemode.o msgbind.o arjdata.o crc32.o)

TODAY_OBJS = $(patsubst %,$(TOOLS_DIR)/%, \
             filemode.o today.o)

MAKE_KEY_OBJS = $(patsubst %,$(TOOLS_DIR)/%, \
                make_key.o crc32.o misc.o arj_proc.o arjsec_h.o arjsec_l.o)

POSTPROC_OBJS = $(patsubst %,$(TOOLS_DIR)/%, \
		filemode.o postproc.o crc32.o)

JOIN_OBJS = $(patsubst %,$(TOOLS_DIR)/%, \
	    filemode.o join.o)

PACKAGER_OBJS = $(patsubst %,$(TOOLS_DIR)/%, \
		filemode.o packager.o arjdata.o)

$(TOOLS_DIR)/msgbind$x: $(MSGBIND_OBJS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(MSGBIND_OBJS) $(LIBS)

$(TOOLS_DIR)/today$x: $(TODAY_OBJS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(TODAY_OBJS) $(LIBS)

$(TOOLS_DIR)/make_key$x: $(MAKE_KEY_OBJS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(MAKE_KEY_OBJS) $(LIBS)

$(TOOLS_DIR)/postproc$x: $(POSTPROC_OBJS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(POSTPROC_OBJS) $(LIBS)

$(TOOLS_DIR)/join$x: $(JOIN_OBJS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(JOIN_OBJS) $(LIBS)

$(TOOLS_DIR)/packager$x: $(PACKAGER_OBJS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(PACKAGER_OBJS) $(LIBS)

# This rule tells that timestamping badly needs date_sig.c (it may bail out
# otherwise when the timestamps are being created for the first time)
$(BASEDIR)/date_sig.c: timestamp

# And this one explicitly specifies that chk_fmsg depends on individual files
$(ARJ_DIR)/chk_fmsg.o: $(SRC_DIR)/chk_fmsg.c $(BASEDIR)/fmsg_arj.c
$(ARJSFX_DIR)/chk_fmsg.o: $(SRC_DIR)/chk_fmsg.c $(BASEDIR)/fmsg_sfx.c
$(ARJSFXV_DIR)/chk_fmsg.o: $(SRC_DIR)/chk_fmsg.c $(BASEDIR)/fmsg_sfv.c

#
# ARJCRYPT utility
#

ifdef REQUIRES_DEF
ARJCRYPT_DEF = $(SRC_DIR)/gnu/arjcrypt.def
endif

ARJCRYPT_OBJS = $(patsubst %,$(ARJCRYPT_DIR)/%, \
		integr.o gost.o gost_t.o nmsg_crp.o arjcrypt.o)

$(ARJCRYPT_DIR)/arjcrypt$d: $(ARJCRYPT_OBJS) $(TOOLS_DIR)/postproc$x
	$(CC) $(ALL_CFLAGS) $(DLL_FLAGS) -o $@ $(ARJCRYPT_OBJS) $(ARJCRYPT_DEF) $(LIBS)
	$(STRIP) $@
	$(TOOLS_DIR)/postproc $@

$(BASEDIR)/nmsg_crp.c $(BASEDIR)/msg_crp.h: $(TOOLS_DIR)/msgbind$x $(RESFILE)
	$(TOOLS_DIR)/msgbind $(RESFILE) msg_crp $(OS_ID) $(PACKAGE) $(LOCALE) $(BASEDIR)

#
# SFX stub
#

SFXSTUB_OBJS = $(patsubst %,$(SFXSTUB_DIR)/%, \
	       nmsg_stb.o sfxstub.o)

$(SFXSTUB_DIR)/sfxstub$x: $(SFXSTUB_OBJS) $(TOOLS_DIR)/postproc$x
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(SFXSTUB_OBJS) $(LIBS)
	$(STRIP) $@
	$(TOOLS_DIR)/postproc$x $@ -sfx

$(BASEDIR)/nmsg_stb.c $(BASEDIR)/msg_stb.h: $(TOOLS_DIR)/msgbind$x $(RESFILE)
	$(TOOLS_DIR)/msgbind $(RESFILE) msg_stb $(OS_ID) $(PACKAGE) $(LOCALE) $(BASEDIR)

#
# ARJSFXV module
#

ARJSFXV_OBJS = $(patsubst %,$(ARJSFXV_DIR)/%, \
	       sfx_id.o filemode.o date_sig.o fmsg_sfv.o imsg_sfv.o nmsg_sfv.o \
	       decode.o arjsfx.o fardata.o arj_user.o arj_arcv.o arj_file.o \
	       crc32.o misc.o debug.o arj_proc.o environ.o ea_mgr.o ntstream.o \
	       uxspec.o ext_hdr.o arjtypes.o exe_sear.o chk_fmsg.o filelist.o \
	       arjsec_h.o arjsec_l.o garble.o scrnio.o ansi.o externs.o)

$(ARJSFXV_DIR)/arjsfxv$x: $(ARJSFXV_OBJS) $(TOOLS_DIR)/postproc$x
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(ARJSFXV_OBJS) $(LIBS) $(DYN_LIBS)
	$(STRIP) $@
	$(TOOLS_DIR)/postproc$x $@ -sfx

$(BASEDIR)/fmsg_sfv.c $(BASEDIR)/imsg_sfv.c $(BASEDIR)/nmsg_sfv.c \
$(BASEDIR)/msg_sfv.h: $(TOOLS_DIR)/msgbind$x $(RESFILE)
	$(TOOLS_DIR)/msgbind $(RESFILE) msg_sfv $(OS_ID) $(PACKAGE) $(LOCALE) $(BASEDIR)

#
# ARJSFX module
#

ARJSFX_OBJS = $(patsubst %,$(ARJSFX_DIR)/%, \
	      sfx_id.o filemode.o fmsg_sfx.o imsg_sfx.o nmsg_sfx.o decode.o \
	      arjsfx.o fardata.o arj_user.o arj_arcv.o arj_file.o crc32.o \
	      misc.o debug.o arj_proc.o environ.o arjtypes.o exe_sear.o \
	      chk_fmsg.o arjsec_h.o arjsec_l.o garble.o externs.o)

$(ARJSFX_DIR)/arjsfx$x: $(ARJSFX_OBJS) $(TOOLS_DIR)/postproc$x
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(ARJSFX_OBJS) $(LIBS)
	$(STRIP) $@
	$(TOOLS_DIR)/postproc$x $@ -sfx

$(BASEDIR)/fmsg_sfx.c $(BASEDIR)/imsg_sfx.c $(BASEDIR)/nmsg_sfx.c \
$(BASEDIR)/msg_sfx.h: $(TOOLS_DIR)/msgbind$x $(RESFILE)
	$(TOOLS_DIR)/msgbind $(RESFILE) msg_sfx $(OS_ID) $(PACKAGE) $(LOCALE) $(BASEDIR)

#
# ARJSFXJR module
#

ARJSFXJR_OBJS = $(patsubst %,$(ARJSFXJR_DIR)/%, \
		sfx_id.o fmsg_sfj.o nmsg_sfj.o arjsfxjr.o debug.o \
		crc32.o environ.o)

$(ARJSFXJR_DIR)/arjsfxjr$x: $(ARJSFXJR_OBJS) $(TOOLS_DIR)/postproc$x
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(ARJSFXJR_OBJS) $(LIBS)
	$(STRIP) $@
	$(TOOLS_DIR)/postproc$x $@ -sfx

$(BASEDIR)/fmsg_sfj.c $(BASEDIR)/imsg_sfj.c $(BASEDIR)/nmsg_sfj.c \
$(BASEDIR)/msg_sfj.h: $(TOOLS_DIR)/msgbind$x $(RESFILE)
	$(TOOLS_DIR)/msgbind $(RESFILE) msg_sfj $(OS_ID) $(PACKAGE) $(LOCALE) $(BASEDIR)

#
# ARJ itself
#

ARJ_OBJS = $(patsubst %,$(ARJ_DIR)/%, \
	   filemode.o date_sig.o fmsg_arj.o imsg_arj.o \
	   nmsg_arj.o integr.o file_reg.o decode.o encode.o \
	   arj.o enc_gwy.o fardata.o arj_user.o arj_arcv.o \
	   arj_file.o crc32.o misc.o debug.o arj_proc.o \
	   environ.o ea_mgr.o uxspec.o ext_hdr.o arjtypes.o exe_sear.o chk_fmsg.o \
	   filelist.o arjsec_h.o arjsec_l.o garble.o scrnio.o ansi.o \
	   crc16tab.o recovery.o gost.o gost40.o gost_t.o \
	   externs.o)

$(ARJ_DIR)/arj$x: $(ARJ_OBJS) \
		  $(RES_DIR)/$(LOCALE)/arjl.txt $(RES_DIR)/$(LOCALE)/arjs.txt \
		  $(TOOLS_DIR)/join$x \
		  $(TOOLS_DIR)/postproc$x \
		  $(SFXSTUB_DIR)/sfxstub$x \
		  $(ARJSFXV_DIR)/arjsfxv$x \
		  $(ARJSFX_DIR)/arjsfx$x \
		  $(ARJSFXJR_DIR)/arjsfxjr$x
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(ARJ_OBJS) $(LIBS) $(DYN_LIBS)
	$(STRIP) $@
	$(TOOLS_DIR)/join $(ARJ_DIR)/arj$x $(ARJSFXJR_DIR)/arjsfxjr$x
	$(TOOLS_DIR)/join $(ARJ_DIR)/arj$x $(ARJSFX_DIR)/arjsfx$x
	$(TOOLS_DIR)/join $(ARJ_DIR)/arj$x $(ARJSFXV_DIR)/arjsfxv$x
	$(TOOLS_DIR)/join $(ARJ_DIR)/arj$x $(SFXSTUB_DIR)/sfxstub$x
	rm -f $(BASEDIR)/help.arj
	$(ARJ_DIR)/arj$x a $(BASEDIR)/help.arj -+ -t1f -2e -e -jm -jh65535 -jt $(RES_DIR)/$(LOCALE)/arj?.txt
	$(TOOLS_DIR)/join $(ARJ_DIR)/arj$x $(BASEDIR)/help.arj
	$(TOOLS_DIR)/postproc $@

$(BASEDIR)/fmsg_arj.c $(BASEDIR)/imsg_arj.c $(BASEDIR)/nmsg_arj.c \
$(BASEDIR)/msg_arj.h: $(TOOLS_DIR)/msgbind$x $(RESFILE)
	$(TOOLS_DIR)/msgbind $(RESFILE) msg_arj $(OS_ID) $(PACKAGE) $(LOCALE) $(BASEDIR)

#
# REARJ utility
#

REARJ_OBJS = $(patsubst %,$(REARJ_DIR)/%, \
	     integr.o filemode.o date_sig.o fmsg_rej.o nmsg_rej.o file_reg.o \
	     rearj.o fardata.o arj_file.o crc32.o misc.o debug.o arj_proc.o \
	     environ.o arjtypes.o filelist.o scrnio.o arjsec_h.o arjsec_l.o \
             externs.o)

$(REARJ_DIR)/rearj$x: $(REARJ_OBJS) \
		      $(TOOLS_DIR)/postproc$x
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(REARJ_OBJS) $(LIBS)
	$(STRIP) $@
	$(TOOLS_DIR)/postproc $@

$(BASEDIR)/fmsg_rej.c $(BASEDIR)/imsg_rej.c $(BASEDIR)/nmsg_rej.c \
$(BASEDIR)/msg_rej.h: $(TOOLS_DIR)/msgbind$x $(RESFILE)
	$(TOOLS_DIR)/msgbind $(RESFILE) msg_rej $(OS_ID) $(PACKAGE) $(LOCALE) $(BASEDIR)

#
# Registration wizard
#

REGISTER_OBJS = $(patsubst %,$(REGISTER_DIR)/%, \
		filemode.o integr.o fmsg_reg.o nmsg_reg.o register.o fardata.o crc32.o \
		debug.o arj_proc.o environ.o)

$(REGISTER_DIR)/$(REGISTER)$x: $(REGISTER_OBJS) \
		            $(TOOLS_DIR)/postproc$x
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(REGISTER_OBJS) $(LIBS)
	$(STRIP) $@
	$(TOOLS_DIR)/postproc $@ -sfx

$(BASEDIR)/fmsg_reg.c $(BASEDIR)/imsg_reg.c $(BASEDIR)/nmsg_reg.c \
$(BASEDIR)/msg_reg.h: $(TOOLS_DIR)/msgbind$x $(RESFILE)
	$(TOOLS_DIR)/msgbind $(RESFILE) msg_reg $(OS_ID) $(PACKAGE) $(LOCALE) $(BASEDIR)

#
# Demonstration display program
#

ARJDISP_OBJS = $(patsubst %,$(ARJDISP_DIR)/%, \
	       nmsg_adi.o arjdisp.o fardata.o debug.o arj_proc.o environ.o \
	       scrnio.o)

$(ARJDISP_DIR)/arjdisp$x: $(ARJDISP_OBJS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o $@ $(ARJDISP_OBJS) $(LIBS)
	$(STRIP) $@

$(BASEDIR)/fmsg_adi.c $(BASEDIR)/imsg_adi.c $(BASEDIR)/nmsg_adi.c \
$(BASEDIR)/msg_adi.h: $(TOOLS_DIR)/msgbind$x $(RESFILE)
	$(TOOLS_DIR)/msgbind $(RESFILE) msg_adi $(OS_ID) $(PACKAGE) $(LOCALE) $(BASEDIR)

#
# Pre-compilation actions
#

prepare:
	-mkdir -p $(BASEDIR)
	-mkdir -p $(TOOLS_DIR)
	-mkdir -p $(ARJCRYPT_DIR)
	-mkdir -p $(SFXSTUB_DIR)
	-mkdir -p $(ARJSFXV_DIR)
	-mkdir -p $(ARJSFX_DIR)
	-mkdir -p $(ARJSFXJR_DIR)
	-mkdir -p $(ARJ_DIR)
	-mkdir -p $(REARJ_DIR)
	-mkdir -p $(REGISTER_DIR)
	-mkdir -p $(ARJDISP_DIR)

#
# Cleanup
#

clean:
	rm -rf .deps
	rm -rf $(BASEDIR)
	rm -f arj.core

#
# Local installation
#

ifneq ($(WINDIR),)
x = .exe
endif

install: all
	$(INSTALL_DIR) $(DESTDIR)$(bindir)
	$(INSTALL_DIR) $(DESTDIR)$(mandir)/man1
	$(INSTALL_DIR) $(DESTDIR)$(pkglibdir)

	$(INSTALL_PROGRAM) $(ARJ_DIR)/arj$x $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(ARJCRYPT_DIR)/arjcrypt$d $(DESTDIR)$(pkglibdir)
	$(INSTALL_PROGRAM) $(REARJ_DIR)/rearj$x $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(ARJDISP_DIR)/arjdisp$x $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(REGISTER_DIR)/$(REGISTER)$x $(DESTDIR)$(bindir)

	$(INSTALL_DATA) $(RES_DIR)/en/*.1 $(DESTDIR)$(mandir)/man1

#
# Packaging (was: packager$x $(BASEDIR_C) $(BASEDIR))
#

package: all
	-@mkdir retail
	$(TOOLS_DIR)/packager$x $(BASEDIR) $(BASEDIR)

#
# Traditional help
#

help:
	@echo
	@echo "Quick start:"
	@echo "all        -- standard compile"
	@echo "package    -- all + packaging (requires a commercial version of the same kind)"
	@echo "clean      -- removes all '.o' and 'core' files"
	@echo "install    -- local installation"
	@echo
	@echo "Don't panic, the legacy compatibility is retained."
	@echo
