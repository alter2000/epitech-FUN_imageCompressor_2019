##
## EPITECH PROJECT, 2019
## cpp_rush3_2019
## File description:
## automated desc ftw
##

NAME = imageCompressor
SRC = $(shell find ./app ./src -name '*.hs')

$(NAME): all

all: $(SRC)
	stack install icomp:exe:icomp-exe --local-bin-path '.' --flag icomp:liquid
	mv icomp-exe imageCompressor -f

tests_run:
	stack install icomp:test:icomp-test --local-bin-path '.' --flag icomp:liquid

clean: fclean

fclean: ./.stack-work
	rm -rf .stack-work tags TAGS

re:
	$(MAKE) fclean
	$(MAKE)

.PHONY: clean re all fclean
