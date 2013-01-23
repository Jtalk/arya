# Arya DNS server makefile.

EC=erlc
NAME=Arya
INCLUDES=-I ./include
OUTPUT=-o ./ebin
FILES=./src/*.erl
CLEAN=./ebin/*.beam

all:
	@echo Building $(NAME)...
	@$(EC) $(INCLUDES) $(OUTPUT) $(FILES) && echo Done
	
clean:
	@rm $(CLEAN) && echo Done