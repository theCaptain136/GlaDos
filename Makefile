##
## EPITECH PROJECT, 2023
## B-FUN-500-BER-5-2-glados-pablo-elias.herrmann
## File description:
## Makefile
##

NAME     =    glados

RM         =    @rm -f

CFLAGS = -Wno-everything

$(NAME):
	stack build --copy-bins --local-bin-path .
	@mv Glados-exe $@

all: $(NAME)

clean:
	$(RM) -r .stack-work

fclean: clean
	$(RM) $(NAME)
	$(RM) $(NAME)-exe

test:
	./glados Glados/test/doubleFunctionCallLhs6.scm
	./glados Glados/test/doubleFunctionCallRhs6.scm
	./glados Glados/test/trippleFunctionCall8.scm
	./glados Glados/test/singleFunctionCall4.scm
	./glados Glados/test/FailOnMissingBrackets80.scm
	./glados Glados/test/defineCall43.scm
	./glados Glados/test/lamdaWithValue3.scm
	./glados Glados/test/customFunctions5nl6.scm

re: fclean all

.PHONY: all clean fclean re