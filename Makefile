##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile for clean.
##

SRC     =       wolfram.hs

CC              =       ghc

NAME    =       wolfram

FLAG    =       -W

all:    $(NAME)
$(NAME):
		$(CC) -o $(NAME) $(SRC)
clean:
	rm -f $(shell find $(SOURCEDIR) -name '*.o')
	rm -f $(shell find $(SOURCEDIR) -name '*~')
	rm -f $(shell find $(SOURCEDIR) -name '*.hi')

fclean: clean
		rm -f $(NAME)

re:     fclean all

.PHONY: all clean fclean
