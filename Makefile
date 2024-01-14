#Compiler settings
CC=gcc
CFLAGS=-Wall

# Target executable name
TARGET=present

# Default target
all: $(TARGET)

# Rule to build the executable
$(TARGET): $(TARGET).c
	$(CC) $(CFLAGS) $(TARGET).c -o $(TARGET)

# Rule to clean up compiled files
clean:
	rm -f $(TARGET)

# Rule to run the program
run: all
	./$(TARGET)