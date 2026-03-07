#pragma once
#include <string>
#include "scheme_engine.h"

template <typename T> class ScriptProperty {


};




class __SProperty_float {
    float data;
    std::string successorFunction;

public:
  // this takes the arguments to the scheme function, and will update
  // the property with the output of the scheme function
  // returns false if the scheme function returned an error
  //         true otherwise
    bool update() {
        s7_scheme* s7 = get_scheme();

    };

};
