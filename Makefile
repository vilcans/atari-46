PRODUCT ?= game

DASM = dasm

SOURCES = main.asm level.asm
RESOURCES = whale.dat deadwhale.dat

all: $(PRODUCT).rom

$(PRODUCT).rom: $(SOURCES) $(RESOURCES)
	$(DASM) main.asm -s$(PRODUCT).sym -l$(PRODUCT).lst -f3 -o$(PRODUCT).rom

.PHONY: run
run: $(PRODUCT).rom
	stella $(PRODUCT).rom

levels/%.txt: levels/%.json
	python3 tiles_to_text.py $<

level.asm: levels/level01.txt convert_level.py
	python3 convert_level.py -o $@ $<

%.dat: %.png
	python3 convert_image.py $< $@
