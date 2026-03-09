all:
	g++ -o joy src/engine/*.c src/engine/*.cpp -lSDL3 -lSDL3_image -lcurl -DWITH_HTTP
