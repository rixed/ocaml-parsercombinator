all: index.html

index.html: ../parsercombinator/README.html
	cp -f $< $@

