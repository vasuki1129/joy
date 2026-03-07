#include "scheme_engine.h"
#include "renderer.h"
#include "s7.h"

s7_scheme *s7 = nullptr;
std::string current_module_path = "";

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
    s7_add_to_load_path(s7, "scsrc");
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
}

void reload_current_module()
{
    load_game_module(current_module_path);
}
