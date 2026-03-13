#pragma once
#include "s7.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_render.h>
#include "scheme_engine.h"
#include "imgui.h"

#include <SDL3/SDL_surface.h>
#include <SDL3_ttf/SDL_ttf.h>
#include <vector>
//SDL_Window* GetWindow();
//SDL_Renderer* GetRenderer();

SDL_Renderer* get_renderer();
void renderer_init();
void renderer_clear();
void renderer_update();

SDL_Texture* getFallbackFont();
TTF_Font* get_ttf_font();

void defer_destroy_texture(SDL_Texture* texture);



// string -> x -> y -> size -> color
SCHEME_FUNC(scm_render_string_ttf)
{
  const char *str = s7_string(s7_list_ref(get_scheme(), args, 0));

  int x = s7_integer(s7_list_ref(get_scheme(), args, 1));
  int y = s7_integer(s7_list_ref(get_scheme(), args, 2));
  int size = s7_integer(s7_list_ref(get_scheme(), args, 3));
  s7_pointer color = s7_list_ref(get_scheme(), args, 4);
  float r = s7_real(s7_list_ref(get_scheme(), color, 0));
  float g = s7_real(s7_list_ref(get_scheme(), color, 1));
  float b = s7_real(s7_list_ref(get_scheme(), color, 2));
  float a = s7_real(s7_list_ref(get_scheme(), color, 3));



  char* c = strdup(str);
  const char *string = strtok(c, "\n");
  int n_lines = 0;
  while (string != NULL) {

    SDL_Color col{255, 255, 255, 255};
    SDL_Surface *textSurface =
        TTF_RenderText_Solid(get_ttf_font(), string, strlen(string), col);
    if (textSurface == NULL) {
      free(c);
      return SCHEME_NIL;
    }
    SDL_Texture *textTexture =
        SDL_CreateTextureFromSurface(get_renderer(), textSurface);
    if (textTexture == NULL) {
      SDL_DestroySurface(textSurface);
      free(c);
      return SCHEME_NIL;
    }
    int mWidth = textSurface->w;
    int mHeight = textSurface->h;
    SDL_DestroySurface(textSurface);
    SDL_FRect src{0, 0, (float)mWidth, (float)mHeight};
    SDL_FRect dst{(float)x, (float)y+n_lines*14.0f, (float)mWidth, (float)mHeight};
    SDL_SetTextureColorModFloat(textTexture, r, g, b);
    SDL_SetTextureScaleMode(textTexture, SDL_SCALEMODE_LINEAR);
    SDL_RenderTexture(get_renderer(), textTexture, &src, &dst);
    defer_destroy_texture(textTexture);
    string = strtok(NULL,"\n");
    n_lines++;
  }
  free(c);
  return SCHEME_NIL;
}

static s7_pointer scm_imgui_end(s7_scheme *sc, s7_pointer args) {
  ImGui::End();
  return s7_nil(get_scheme());
}

static s7_pointer scm_imgui_begin(s7_scheme *sc, s7_pointer args) {
  if (s7_is_string(s7_car(args))) {
    ImGui::Begin(s7_string(s7_car(args)));
  }
  return s7_nil(get_scheme());
}

static s7_pointer scm_set_color(s7_scheme *sc, s7_pointer args) {
  if(s7_is_list(sc,s7_car(args)))
  {
    s7_pointer color = s7_car(args);
    float a1 = s7_real(s7_car(color));
    float a2 = s7_real(s7_car(s7_cdr(color)));
    float a3 = s7_real(s7_car(s7_cdr(s7_cdr(color))));
    float a4 = s7_real(s7_car(s7_cdr(s7_cdr(s7_cdr(color)))));
    SDL_SetTextureColorModFloat(getFallbackFont(), a1, a2, a3);
    SDL_SetRenderDrawColorFloat(get_renderer(),a1,a2,a3,a4);
  }
  return s7_nil(get_scheme());
}

static s7_pointer scm_fill_rect(s7_scheme *sc, s7_pointer args) {
  if(s7_is_list(sc,s7_car(args)))
  {
    s7_pointer coordinate = s7_car(args);
    float a1 = s7_real(s7_car(coordinate));
    float a2 = s7_real(s7_car(s7_cdr(coordinate)));
    float a3 = s7_real(s7_car(s7_cdr(s7_cdr(coordinate))));
    float a4 = s7_real(s7_car(s7_cdr(s7_cdr(s7_cdr(coordinate)))));
    const SDL_FRect rect{a1,a2,a3,a4};
    SDL_RenderFillRect(get_renderer(),&rect);
  }
  return s7_nil(get_scheme());
}


static s7_pointer scm_draw_line(s7_scheme *sc, s7_pointer args) {
  if(s7_is_list(sc,s7_car(args)) && s7_is_list(sc,s7_car(s7_cdr(args))))
  {
    s7_pointer coordinate1 = s7_car(args);
    float a1 = s7_real(s7_car(coordinate1));
    float a2 = s7_real(s7_car(s7_cdr(coordinate1)));

    s7_pointer coordinate2 = s7_car(s7_cdr(args));
    float a3 = s7_real(s7_car(coordinate2));
    float a4 = s7_real(s7_car(s7_cdr(coordinate2)));
    SDL_RenderLine(get_renderer(),a1,a2,a3,a4);
    return s7_nil(get_scheme());
  }
  else
  {
    return s7_wrong_type_arg_error(sc, "draw-line", 1, args, "input must be two points");
  }

}

void render_character(uint8_t c, float x, float y, float xs, float xy);

SCHEME_FUNC(scm_render_character)
{
  uint8_t c;
  if(s7_is_character(s7_car(args)))
  {
      c = s7_character(s7_car(args));
  }
  else
  {
      c = s7_integer(s7_car(args));

  }

  float x = s7_real(s7_car(s7_cdr(args)));
  float y = s7_real(s7_car(s7_cdr(s7_cdr(args))));
  float xs = s7_real(s7_car(s7_cdr(s7_cdr(s7_cdr(args)))));
  float xy = s7_real(s7_car(s7_cdr(s7_cdr(s7_cdr(s7_cdr(args))))));

  render_character(c, x, y, xs, xy);
  return s7_nil(sc);
}
