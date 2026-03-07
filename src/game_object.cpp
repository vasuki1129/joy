#include "game_object.h"
#include <unordered_map>


#include "s7.h"






void GameObject::Update() {


}

std::unordered_map<int, GameObject*> gameObjects;


GameObject *GetObjectByID(int id) {
    if (gameObjects.count(id)) {
        return gameObjects[id];
    } else {
        return nullptr;
    }
}

static s7_pointer translate(s7_scheme *sc, s7_pointer args) {

    int id = s7_number_to_integer(sc, s7_car(args));
    float x = s7_number_to_real(sc, s7_car(s7_car(args)));
    float y = s7_number_to_real(sc, s7_car(s7_car(s7_car(args))));
    float z = s7_number_to_real(sc,s7_car(s7_car(s7_car(s7_car(args)))));
    glm::vec3 translation(x, y, z);


}
