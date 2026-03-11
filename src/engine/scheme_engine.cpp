#include "scheme_engine.h"
#include "renderer.h"
#include "s7.h"
#include <SDL3/SDL_keycode.h>
#include "http.h"
s7_scheme *s7 = nullptr;
std::string current_module_path = "";


void scheme_pass_key_up(SDL_Keycode code)
{
  s7_call(get_scheme(),
          s7_name_to_value(get_scheme(),"process-key-up"),
          s7_make_list(s7,1,s7_make_integer(get_scheme(),(s7_int)code))
          );
}

void scheme_pass_key_down(SDL_Keycode code)
{
  s7_call(get_scheme(),
          s7_name_to_value(get_scheme(),
                           "process-key-down"),
          s7_make_list(s7,1,s7_make_integer(get_scheme(),
                          (s7_int)code))
          );
}




void load_game_module(std::string path)
{



    if(s7 != nullptr)
    {
        s7_free(s7);
    }
    scheme_init();
    s7_load(get_scheme(),path.c_str());
    s7_eval(get_scheme(), s7_name_to_value(get_scheme(), "global-init"),s7_rootlet(get_scheme()));

    current_module_path = path;
}


void scheme_eval_proc(std::string function)
{
    s7_call(get_scheme(), s7_name_to_value(get_scheme(), function.c_str()),s7_nil(get_scheme()));
}

s7_scheme *get_scheme() {
    return s7;
}

void scheme_init() {
    s7 = s7_init();
    register_scheme_functions();
    s7_add_to_load_path(s7, "src/scsrc");
    s7_add_to_load_path(s7, "src/scsrc/s7lib");
    s7_load(s7, "init.scm");
}

void register_scheme_functions() {
    SCHEME_REGISTER(scm_print,"print","prints the passed string to console",1)
    s7_define_function(s7, "imgui-begin", scm_imgui_begin, 1, 0, false, "ImGui::Begin");
    s7_define_function(s7, "imgui-end", scm_imgui_end, 0, 0, false, "ImGui::End");

    s7_define_function(s7, "fill-rect", scm_fill_rect, 1, 0, false, "SDL_FillRect");
    s7_define_function(s7, "set-color", scm_set_color, 1, 0, false, "SDL_SetRenderDrawColorFloat");
    s7_define_function(s7, "draw-line", scm_draw_line, 2, 0, false, "SDL_SetRenderLine");

    SCHEME_REGISTER(scm_render_character,"render-character","renders a given character",5);
    SCHEME_REGISTER(scm_render_string_ttf,"render-string-ttf", "renders a pretty string",5);





#ifdef WITH_HTTP
    initialize_curl();
    SCHEME_REGISTER(scm_curl_get_sync, "curl-get-sync","Makes a simple get request and blocks until it responds", 1);
#endif



}

void reload_current_module()
{
    load_game_module(current_module_path);
}
