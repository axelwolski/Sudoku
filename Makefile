# for building Sudoku strategies
SRC= general.lisp file-path.lisp coor.lisp square.lisp strategy.lisp squares.lisp game.lisp strategies.lisp standalone.lisp

STRATEGIES-DIRECTORY = Strategies

BRUTE-FORCE= $(SRC) universal-strategy.lisp deterministic-tactic.lisp non-deterministic-tactic.lisp fuzzy-set.lisp region-tactics.lisp brute-force-strategy.lisp brute-force.lisp brute-force-strategy.lisp

RANDOM= $(SRC) random-strategy.lisp

all: $(STRATEGIES-DIRECTORY)/random.lisp $(STRATEGIES-DIRECTORY)/brute-force.lisp

$(STRATEGIES-DIRECTORY)/random.lisp: $(RANDOM)
	./strip.sh $(RANDOM) > $@

$(STRATEGIES-DIRECTORY)/brute-force.lisp: $(BRUTE-FORCE)
	./strip.sh $(BRUTE-FORCE) > $@

.PHONY: clean

clean:
	rm -f *.fasl *~
	rm -f $(STRATEGIES-DIRECTORY)/random.lisp
	rm -f $(STRATEGIES-DIRECTORY)/brute-force.lisp

