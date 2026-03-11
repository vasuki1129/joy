all:
	g++ -o joy src/engine/*.c src/engine/*.cpp -lSDL3 -lSDL3_image -lSDL3_ttf -lcurl -DWITH_HTTP
