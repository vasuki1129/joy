#include "renderer.h"
#include "s7.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_render.h>
#include <SDL3/SDL_surface.h>
#include <SDL3/SDL_video.h>
#include <SDL3_image/SDL_image.h>

#include <string>

SDL_Window *window;
SDL_Renderer *renderer;
SDL_Texture* fallbackBitmap;


SDL_Texture* getFallbackFont(){return fallbackBitmap;}
//SDL_Window *GetWindow() { return window; }
SDL_Renderer *get_renderer() { return renderer; }

void render_character(uint8_t c, float x, float y, float xs, float xy)
{
    int xOff = (c % 16) * 16;
    int yOff = (c / 16) * 16;


    SDL_FRect src = {(float)xOff,(float)yOff,16,16};
    SDL_FRect dst = {x,y,xs,xy};
    SDL_RenderTexture(renderer,fallbackBitmap,&src,&dst);
}

void renderer_init() {
    SDL_Init(SDL_INIT_VIDEO);
    SDL_CreateWindowAndRenderer("joy", 800, 600, SDL_WINDOW_RESIZABLE, &window, &renderer);
    fallbackBitmap = IMG_LoadTexture(renderer, "assets/curses_square_16x16.png");
    SDL_SetTextureScaleMode(fallbackBitmap, SDL_SCALEMODE_PIXELART);
}

void renderer_clear()
{
    SDL_SetRenderDrawColorFloat(renderer, 0.0f, 0.0f, 0.0f, 1.0f);
    SDL_RenderClear(renderer);
}

void renderer_update()
{
    int wx, wy;
    SDL_GetWindowSize(window, &wx, &wy);
    std::string call_data = "(set-window-size ";
    call_data += std::to_string(wx);

    call_data += " ";
    call_data+= std::to_string(wx);
    call_data += ")";

    s7_eval_c_string(get_scheme(), call_data.c_str());

    SDL_RenderPresent(renderer);
}

