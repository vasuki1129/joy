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
      expr += std::to_string(delta);
      expr += ")";
      s7_eval_c_string(get_scheme(),expr.c_str());

      expr = "";
      expr += "(set! global-time ";
      expr += std::to_string(global_time);
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
      }
      renderer_clear();

      scheme_eval_proc("global-update-process");
      scheme_eval_proc("global-render-process");

      renderer_update();
      delta = (SDL_GetTicks()-last)/1000.0f;
    }
/*
*/
}
