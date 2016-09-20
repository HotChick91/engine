#pragma once

#include <GLFW/glfw3.h>

#include "cl.h"
#include "types.h"

extern Point3f camera_pos;
extern Point3f camera_target;
extern Point3f up;
extern Point3f light;
extern float horizontal_angle;
extern float vertical_angle;

extern float AOV;
extern float AOVd;

extern RenderMethod render_method;

extern OctTreeNode *mainOctTree;
extern int octTreeLength;

extern GLuint texture;
extern GLFWwindow *window;
extern const int height;
extern const int width;

extern cl_command_queue queue;
extern cl_kernel kernel;
extern cl_mem mainOctCL;
extern cl_mem image;
extern cl_uint num_platforms;
extern cl_context context;
extern cl_program program;
