#pragma once
#include "s7.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_render.h>
#include "scheme_engine.h"
#include "imgui.h"

//SDL_Window* GetWindow();
//SDL_Renderer* GetRenderer();

SDL_Renderer* get_renderer();
void renderer_init();
void renderer_clear();
void renderer_update();

SDL_Texture* getFallbackFont();


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
