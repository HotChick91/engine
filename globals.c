#include "globals.h"

#include "cl.h"

Point3f camera_pos = {0.89f,-0.98f,-0.25f};
Point3f camera_target = {0.0f,1.0f,0.0f};
Point3f up = {0.f, 0.f, 1.f};
Point3f light = {1.0f, -2.0f, 0.0f};
float horizontal_angle = 2.0;
float vertical_angle = 0.0;

RenderMethod render_method = Stacking;

OctTreeNode * mainOctTree;

GLFWwindow* window;
const int height = 1024;
const int width = 1024;
float *data4;

#if TRACER_CL
cl_command_queue queue;
cl_kernel kernel;
cl_mem mainOctCL;
cl_mem image;
#endif
