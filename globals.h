#pragma once

#include <time.h>
#include <GLFW/glfw3.h>

#include "cl.h"
#include "types.h"

extern Point3f camera_pos;
extern Point3f camera_target;
extern Point3f up;
extern Point3f light;
extern Point3f light_tgt;
extern clock_t light_mtime;
extern float horizontal_angle;
extern float vertical_angle;
extern float camera_speed;
extern int camera_movement_active;
extern double cursor_turn_speed;

extern float AOV;
extern float AOVd;

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
