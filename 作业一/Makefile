CXX = clang++
CXXFLAGS = -std=c++17 -Wall -g -O0

TARGET = compiler1
SRCS = compiler1.cpp

all: $(TARGET)

$(TARGET): $(SRCS)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SRCS)

clean:
	rm -rf $(TARGET) $(TARGET).dSYM

.PHONY: all clean
