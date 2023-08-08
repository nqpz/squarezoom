.PHONY: all clean
all:
	$(MAKE) -C squarezoom-random
	$(MAKE) -C squarezoom-image

clean:
	$(MAKE) clean -C squarezoom-random
	$(MAKE) clean -C squarezoom-image
