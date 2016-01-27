#include "globals.h"

#include "cl.h"

#define M_PI 3.14159265358979323846

Point3f camera_pos = {0.89f,-0.98f,-0.25f};
Point3f camera_target = {0.0f,1.0f,0.0f};
Point3f up = {0.f, 0.f, 1.f};
Point3f light = {1.0f, -2.0f, 0.0f};
float horizontal_angle = 2.0;
float vertical_angle = 0.0;

const float vertical_AOV = M_PI / 2;
const float horizontal_AOV = M_PI / 2;

RenderMethod render_method = Stacking;

OctTreeNode *mainOctTree;
int octTreeLength = 0;

GLuint texture;
GLFWwindow *window;
const int height = 1024;
const int width = 1024;

cl_command_queue queue;
cl_kernel kernel;
cl_mem mainOctCL;
cl_mem image;
cl_uint num_platforms;
