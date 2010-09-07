DIRS=lazy object negative conjunctive n+k alist combination

all:
	@for d in $(DIRS); do (cd $$d && $(MAKE)); done

test:
	@for d in $(DIRS); do (cd $$d && $(MAKE) test); done

clean:
	@for d in $(DIRS); do (cd $$d && $(MAKE) clean); done

