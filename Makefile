##
## EPITECH PROJECT, 2023
## B-FUN-500-BER-5-2-glados-pablo-elias.herrmann
## File description:
## Makefile
##

NAME     =    glados

RM         =    @rm -f

$(NAME):
	stack build --copy-bins --local-bin-path .
	@mv Glados-exe $@

all: $(NAME)

clean:
	$(RM) -r .stack-work

fclean: clean
	$(RM) $(NAME)
	$(RM) $(NAME)-exe

re: fclean all

.PHONY: all clean fclean re