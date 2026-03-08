#pragma once
#include <SDL3/SDL.h>


#define __SCHEME_FUNC static s7_pointer
#define SCHEME_FUNC(name) __SCHEME_FUNC name ( s7_scheme* sc, s7_pointer args )
#define SCHEME_REGISTER(func, name, desc, arity) s7_define_function(s7, name, func, arity, 0, false, desc);

#include "s7.h"

void register_scheme_functions();
void scheme_init();
s7_scheme* get_scheme();

void load_game_module(std::string path);

void scheme_eval_proc(std::string function);

void reload_current_module();

void scheme_pass_key_down(SDL_Keycode code);






SCHEME_FUNC(scm_print)
{
  if (s7_is_string(s7_car(args))) {
    printf("%s", s7_string(s7_car(args)));
    fflush(stdout);
  }
  return s7_nil(sc);
}
