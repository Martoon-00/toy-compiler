TESTS=local-backward-main local-backward local-forward-main local-forward nonlocal-backward-to-main nonlocal-backward nonlocal-forward-to-main nonlocal-forward long-jump recursion

EXE_DIR=../../../

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
	RC_RUNTIME=$(EXE_DIR)runtime $(EXE_DIR)rc -o $< && ./$@ > $@.log && diff $@.log orig/$@.log
	$(EXE_DIR)rc -i $< > $@.log && diff $@.log orig/$@.log
	$(EXE_DIR)rc -s $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ $(TESTS)
