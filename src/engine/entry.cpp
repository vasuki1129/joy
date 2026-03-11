#define WITH_HTTP


#include <SDL3/SDL_events.h>
#include <SDL3/SDL_render.h>
#include <SDL3/SDL_timer.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "s7.h"
#include "renderer.h"
#include "scheme_engine.h"


#include <fstream>
#include <iostream>
#include <iomanip> // Required for std::setprecision, std::fixed
#include <sstream> // Required for std::ostringstream







std::string floatToStringWithPrecision(double value, int precision) {
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(precision) << value;
    return oss.str();
}
float timer = 0.0f;

int main(int argc, char **argv) {

  bool repl = false;

    for (int i = 1; i < argc; i++) {
      if (strcmp(argv[i], "-r") == 0){
        repl = true;
      }
    }
    if (repl) {

      scheme_init();
      s7_repl(get_scheme());
      return 0;
    }

    renderer_init();
    bool quit = false;

    load_game_module("default_module/init.scm");
    float last;
    double delta = 0.0f;
    double global_time = 0.0f;

    bool editor = false;

    scheme_eval_proc("global-init-process");
    while (!quit) {

      last = SDL_GetTicks();
      global_time += delta;

      std::string expr = "";
      expr += "(set! global-delta ";
      expr += floatToStringWithPrecision(delta,8);
      expr += ")";
      s7_eval_c_string(get_scheme(),expr.c_str());

      expr = "";
      expr += "(set! global-time ";
      expr += floatToStringWithPrecision(global_time, 8);
      expr += ")";
      s7_eval_c_string(get_scheme(),expr.c_str());

      SDL_Event e;
      while (SDL_PollEvent(&e)) {




        if (e.type == SDL_EVENT_QUIT) {
            quit = true;
        }
        if (e.type == SDL_EVENT_KEY_DOWN)
        {
          if(e.key.key == SDLK_F12)
          {
            reload_current_module();
          }
          else
          {
            scheme_pass_key_down( e.key.key );
          }
        }
        else if(e.type == SDL_EVENT_KEY_UP)
        {
          scheme_pass_key_up(e.key.key);
        }
      }
      renderer_clear();

      scheme_eval_proc("global-update-process");
      scheme_eval_proc("global-render-process");

      renderer_update();
      long frame_time = SDL_GetTicks()-last;
      if (frame_time < (long)((1.0 / 160.0) * 1000.0)) {
          SDL_Delay((long)((1.0/160.0)*1000.0)-frame_time);

      }


      long final_time = SDL_GetTicks()-last;
      delta = (final_time / 1000.0);

    }
/*
*/
}
