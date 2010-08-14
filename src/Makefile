all: Main.html ICalParse.html

.SUFFIXES: .html .lhs

.lhs.html:
		pandoc -r markdown+lhs -w html -s $< > $@
