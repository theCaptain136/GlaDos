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
				Glados/src/EvaluateStack.hs	\
				Glados/src/Lib.hs			\
				Glados/src/Loop.hs			\
				Glados/src/Parser.hs		\
				Glados/src/Translator.hs

OBJ	=	$(patsubst %.hs,%.o,$(SRC)) $(patsubst %.hs,%.hi,$(SRC))

OBJ_STACK	=	$(patsubst %.hs,%.o,$(SRC_STACK)) $(patsubst %.hs,%.hi,$(SRC_STACK))

CFLAGS = -Wno-everything

all:
	ghc -dynamic $(SRC) -o $(NAME)

stack:
	ghc -dynamic $(SRC_STACK) -o $(NAME)

clean:
	$(RM) -r $(OBJ)
	$(RM) -r $(OBJ_STACK)

fclean: clean
	$(RM) $(NAME)

test:
	./Glados/test/checkValue.sh

re: fclean all

restack: fclean stack

.PHONY: all clean fclean re