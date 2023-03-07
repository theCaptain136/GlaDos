#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Define the executable path
EXECUTABLE="./glados"

# Define an array of strings to pass to the executable
DIRECTORY="./Glados/test"
STRINGS=("$DIRECTORY"/*.scm)

# Define a flag for failing tests
FAIL=0

# Loop through the array of strings and call the executable with each string
for s in "${STRINGS[@]}"; do
    echo "Calling executable with string: $s"
    RESPONSE=$(timeout 2s $EXECUTABLE "$s" 2>/dev/null)
    EXIT_CODE=$?
    if [[ $EXIT_CODE -gt 0 ]]; then
        if [[ $s != "./Glados/test/FailOnMissingBrackets1.scm" ]]; then
            echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            FAIL=1
        elif [[ $EXIT_CODE == 1 ]]; then
            echo -e "${GREEN}Test Successful!${NC}"
        fi
    else
        NUMBER=$(echo "$RESPONSE" | tr -dc '0-9')
        if [[ $s == "./Glados/test/doubleFunctionCallLhs6.scm" ]]; then
            if [[ $NUMBER == 6 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/doubleFunctionCallRhs6.scm" ]]; then
            if [[ $NUMBER == 6 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/trippleFunctionCall8.scm" ]]; then
            if [[ $NUMBER == 8 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/singleFunctionCall4.scm" ]]; then
            if [[ $NUMBER == 4 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/defineCall43.scm" ]]; then
            if [[ $NUMBER == 43 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/lamdaWithValue3.scm" ]]; then
            if [[ $NUMBER == 3 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/customFunctions9.scm" ]]; then
            if [[ $NUMBER == 9 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/lamdaWithMultimpleCalls30.scm" ]]; then
            if [[ $NUMBER == 30 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/infixNotation12.scm" ]]; then
            if [[ $NUMBER == 12 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/infixNotationTripple30.scm" ]]; then
            if [[ $NUMBER == 30 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/infixNotationDoubleLhs20.scm" ]]; then
            if [[ $NUMBER == 20 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        elif [[ $s == "./Glados/test/infixNotationDoubleRhs20.scm" ]]; then
            if [[ $NUMBER == 20 ]]; then
                echo -e "${GREEN}Test Successful!${NC}"
            else echo -e "${RED}Executable returned with exit code $EXIT_CODE (failure)${NC}"
            fi
        fi
    fi
done

if [[ $FAIL == 1 ]]; then
    exit 1
else
    exit 0
fi