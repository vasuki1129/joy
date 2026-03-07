#pragma once
#include <string>
#include <glm/glm.hpp>
#include <glm/ext.hpp>
#include "s7.h"









struct GameObject {

    glm::vec3 position;





    glm::vec3 scale;
    glm::quat rotation;



    void Update();
};


GameObject *GetObjectByID(int id);
