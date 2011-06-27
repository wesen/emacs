AUTOMAKE_OPTIONS = no-texinfo.tex
EXTRA_DIST = dp.texi dpi.texi dph.texi dpans2texi.el ansicl.css
lisp_LISP = dpans2texi.el
noinst_LISP = dpans2texi.el
info_TEXINFOS = ansicl.texi
ansicl_TEXINFOS = dp.texi dpi.texi dph.texi
nodist_ansicl_TEXINFOS = temp.texi
AM_MAKEINFOFLAGS=--no-warn --enable-encoding
AM_MAKEINFOHTMLFLAGS=--css-include=ansicl.css
CLEANFILES = temp.texi ansicl ansicl-[0-9] ansicl-[0-9][0-9]
DISTCLEANFILES = temp.texi ansicl ansicl-[0-9] ansicl-[0-9][0-9]
EMACSDP = $(EMACS) --batch -q --no-site-file -l dpans2texi.elc

ansicl: dpans2texi.elc temp.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) temp.texi
	$(EMACSDP) -f dp-tr

temp.texi: dpans2texi.elc
	$(EMACSDP) -f dp-tex2texi

dist-info:

html: temp.texi
	$(MAKEINFO) $(AM_MAKEINFOHTMLFLAGS) --html temp.texi

xml: temp.texi
	$(MAKEINFO) $(AM_MAKEINFOHTMLFLAGS) --xml temp.texi

docbook: temp.texi
	$(MAKEINFO) $(AM_MAKEINFOHTMLFLAGS) --docbook temp.texi

wget: 
	wget -r -np -nd -A tex http://quimby.gnus.org/circus/cl/dpANS3/
	wget -r -np -nd -A tex http://quimby.gnus.org/circus/cl/dpANS3R/
