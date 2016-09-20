#include "globals.h"

#include "cl.h"

#define M_PI 3.14159265358979323846

Point3f camera_pos = {0.89f,-0.98f,-0.25f};
Point3f camera_target = {0.0f,1.0f,0.0f};
Point3f up = {0.f, 0.f, 1.f};
Point3f light = {0.0f, 0.0f, 0.0f};
float horizontal_angle = 2.0;
float vertical_angle = 0.0;
float camera_speed = 0.05f;
int camera_movement_active = 0;
double cursor_turn_speed = 0.02;

float AOV = M_PI / 2;
float AOVd = M_PI / 128;

OctTreeNode *mainOctTree;
int octTreeLength = 0;

GLuint texture;
GLFWwindow *window;
const int height = 720;
const int width = 720;

cl_command_queue queue;
cl_kernel kernel;
cl_mem mainOctCL;
cl_mem image;
cl_uint num_platforms;
cl_context context;
cl_program program;
