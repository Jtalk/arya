# Arya DNS server makefile.

EC=erlc
NAME=Arya
INCLUDES=-I ./include
OUTPUT=-o ./ebin
FILES=./src/*.erl
CLEAN=./ebin/*.beam
RELEASE=\"arya-release-1\"
RELEASE_BUILD="systools:make_script($(RELEASE),[local]),halt()."

all: binaries
	@echo Making release...
	@erl -pa ./ebin -eval $(RELEASE_BUILD) > /dev/null

binaries:
	@echo Building $(NAME)...
	@$(EC) $(INCLUDES) $(OUTPUT) $(FILES) && echo Done
	
clean:
	@rm $(CLEAN) && echo Done
	
clean_release:
	@rm *.script *.boot && echo Done