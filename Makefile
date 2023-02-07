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

test:
	./glados Glados/test/doubleFunctionCallLhs.scm
	./glados Glados/test/doubleFunctionCallRhs.scm
	./glados Glados/test/trippleFunctionCall.scm
	./glados Glados/test/singleFunctionCall.scm
	./glados Glados/test/FailOnMissingBrackets.scm
	./glados Glados/test/defineCall.scm

re: fclean all

.PHONY: all clean fclean re