# Remember good Makefile Style
#
# to_build: dependancies
# 	steps to make dependancies into final build product

BuildDir=./build
HTMLdir=$(BuildDir)/html
markdown=markdown-calibre

all: doc

build: hICalendar.cabal
	runhaskell Setup.hs configure
	runhaskell Setup.hs build

install: hICalendar.cabal
	sudo runhaskell Setup.hs install

doc: $(HTMLdir)/TODO.html $(HTMLdir)/README.html

$(HTMLdir)/TODO.html: TODO.markdown
		$(markdown) $< --file="$@"

$(HTMLdir)/README.html: README.markdown
		$(markdown) $< --file="$@"

docclean: $(HTMLdir)/*.html
		rm $^
