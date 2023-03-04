##
## EPITECH PROJECT, 2023
## B-FUN-500-BER-5-2-glados-pablo-elias.herrmann
## File description:
## Makefile
##

NAME     =    glados

RM         =    @rm -f

SRC	=	Glados/app/Main.hs			\
		Glados/src/AST.hs			\
		Glados/src/Evaluate.hs		\
		Glados/src/Lib.hs			\
		Glados/src/Loop.hs			\
		Glados/src/Parser.hs		\
		Glados/src/Translator.hs

SRC_STACK	=	Glados/app/Main.hs			\
				Glados/src/AST.hs			\
				Glados/src/Evaluate.hs		\
				Glados/src/Lib.hs			\
				Glados/src/Loop.hs			\
				Glados/src/Parser.hs		\
				Glados/src/Translator.hs

OBJ	=	$(patsubst %.hs,%.o,$(SRC)) $(patsubst %.hs,%.hi,$(SRC))

OBJ_STACK	=	$(patsubst %.hs,%.o,$(SRC_STACK)) $(patsubst %.hs,%.hi,$(SRC_STACK))

CFLAGS = -Wno-everything

all:
	ghc -dynamic $(SRC)

stack:
	ghc -dynamic $(SRC_STACK)

clean:
	$(RM) -r $(OBJ)
	$(RM) -r $(OBJ_STACK)

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
	./glados Glados/test/lamdaWithMultimpleCalls7nl3.scm

re: fclean all

.PHONY: all clean fclean re