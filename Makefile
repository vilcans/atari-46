PRODUCT ?= game

DASM = dasm

SOURCES = main.asm video.asm level.asm
RESOURCES = whale.dat deadwhale.dat logo.dat levelnumbers.dat

all: $(PRODUCT).rom

$(PRODUCT).rom: $(SOURCES) $(RESOURCES)
	$(DASM) main.asm -s$(PRODUCT).sym -l$(PRODUCT).lst -f3 -o$(PRODUCT).rom

.PHONY: run
run: $(PRODUCT).rom
	stella $(PRODUCT).rom

levels/%.txt: levels/%.json
	python3 tiles_to_text.py $<

level.asm: levels/level00.txt levels/level01.txt levels/level02.txt
	python3 convert_level.py -o $@ $^

logo.dat: logo.png
	python3 convert_image.py --flip $< $@

%.dat: %.png
	python3 convert_image.py $< $@
