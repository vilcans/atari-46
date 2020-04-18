PRODUCT ?= game

DASM = dasm

SOURCES = main.asm level.asm
RESOURCES =

all: $(PRODUCT).rom

$(PRODUCT).rom: $(SOURCES) $(RESOURCES)
	$(DASM) main.asm -s$(PRODUCT).sym -l$(PRODUCT).lst -f3 -o$(PRODUCT).rom

.PHONY: run
run: $(PRODUCT).rom
	stella $(PRODUCT).rom

level.asm: level.txt convert_level.py
	python3 convert_level.py $< $@
