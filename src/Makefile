DOCTARGET=BaseTypes.html ICalParse.html Main.html VAlarm.html VEvent.html VFreeBusy.html VJournal.html VTodo.html Folding.html
HTML_DIR=../build/html/

all: $(DOCTARGET)

.SUFFIXES: .html .lhs

.lhs.html:
		pandoc -r markdown+lhs -w html -s $< > $@
		#mv $@ $(HTML_DIR)
