#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <GLFW/glfw3.h>

#include "cl.h"
#include "error.h"
#include "geom.h"
#include "globals.h"
#include "render.h"
#include "types.h"

// TODO create proper file for haskell stuff
#include "HsFFI.h"

void load_file(HsPtr name);
void push_oct_tree_empty(void);
void push_oct_tree_solid(float r, float g, float b);
void push_oct_tree_partial(int c0, int c1, int c2, int c3, int c4, int c5, int c6, int c7);

static void key_callback(GLFWwindow *windows, int key, int scancode, int action, int mods);
static void initOctTree(void);

cl_context context;
cl_program program;
#define SOFT_CHECK_CL(status, msg) do {if (status != CL_SUCCESS) {num_platforms = 0; fprintf(stderr, "WARNING: %s (%d)\n", msg, status); return;}} while (0)
static void init_cl(void)
{
    cl_int status;
    cl_platform_id platform_id;
    //cl_context context;
    //cl_program program;

    char *kernel_src = malloc(10240);
    check_nn(kernel_src, "kernel_src");

    status = clGetPlatformIDs(1, &platform_id, &num_platforms);
    SOFT_CHECK_CL(status, "get platform ids");

    fprintf(stderr, "#platforms: %u\n", num_platforms);
    if (num_platforms == 0)
        return;

    static char info[4][128];
    status = clGetPlatformInfo(platform_id, CL_PLATFORM_PROFILE, 128, info[0], NULL);
    SOFT_CHECK_CL(status, "get platform profile");
    status = clGetPlatformInfo(platform_id, CL_PLATFORM_VERSION, 128, info[1], NULL);
    SOFT_CHECK_CL(status, "get platform version");
    status = clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, 128, info[2], NULL);
    SOFT_CHECK_CL(status, "get platform name");
    status = clGetPlatformInfo(platform_id, CL_PLATFORM_VENDOR, 128, info[3], NULL);
    SOFT_CHECK_CL(status, "get platform vendor");

    fprintf(stderr, "profile: %s\n", info[0]);
    fprintf(stderr, "version: %s\n", info[1]);
    fprintf(stderr, "name: %s\n", info[2]);
    fprintf(stderr, "vendor: %s\n", info[3]);

    cl_context_properties *props = getContextProperties(platform_id);
    context = clCreateContextFromType(props, CL_DEVICE_TYPE_GPU, NULL, NULL, &status);
    SOFT_CHECK_CL(status, "create context");

    // create a command queue
    cl_device_id device_id;
    status = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_GPU, 1, &device_id, NULL);
    SOFT_CHECK_CL(status, "get device ids");

    queue = clCreateCommandQueue(context, device_id, 0, &status);
    SOFT_CHECK_CL(status, "create command queue");

    // allocate memory objects
    //mainOctCL = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, octTreeLength*sizeof(OctTreeNode), mainOctTree, &status);
    mainOctCL = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR, octTreeLength*sizeof(OctTreeNode), mainOctTree, &status);
    SOFT_CHECK_CL(status, "create buffer");

    glGenTextures(1, &texture);
    check_gl();
    glBindTexture(GL_TEXTURE_2D, texture);
    check_gl();

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    check_gl();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    check_gl();

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_FLOAT, NULL);
    check_gl();
    glFinish();
    check_gl();

    image = clCreateFromGLTexture(context, CL_MEM_WRITE_ONLY, GL_TEXTURE_2D, 0, texture, &status);
    SOFT_CHECK_CL(status, "create image");

    // create the compute program
    FILE *kernel_handle = fopen("ray.cl", "rb");
    check_nn(kernel_handle, "fopen ray.cl");

    size_t n_bytes = fread(kernel_src, 1, 10239, kernel_handle);
    kernel_src[n_bytes] = '\0';
    check_ferror(kernel_handle, "fread");

    program = clCreateProgramWithSource(context, 1, (const char **)&kernel_src, NULL, &status);
    SOFT_CHECK_CL(status, "create program");

    // build the compute program executable
    status = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
    if (status != CL_BUILD_PROGRAM_FAILURE && status != CL_SUCCESS) {
        SOFT_CHECK_CL(status, "build program");
    } else {
        size_t log_size;
        status = clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size);
        SOFT_CHECK_CL(status, "get program build log size");

        char *log = malloc(log_size);
        check_nn(log, "log");

        status = clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, log_size, log, NULL);
        SOFT_CHECK_CL(status, "get program build log");

        fprintf(stderr, "build kernel log:\n%s\n", log);
    }

    // create the compute kernel
    kernel = clCreateKernel(program, "ray_cl", &status);
    SOFT_CHECK_CL(status, "create kernel");

    //status = clReleaseProgram(program);
    //SOFT_CHECK_CL(status, "release program");

    //status = clReleaseContext(context);
    //SOFT_CHECK_CL(status, "release context");

    fprintf(stderr, "OpenCL initialization successful\n");
    render_method = TracerCL;
}

CheatSheet auxes[1024*1024];
static void dump_tree(void);
static void calc_levels(int id);
int main(int argc, char **argv)
{
    glfwSetErrorCallback(error_callback);

    /* Initialize the library */
    if (!glfwInit()){
        fprintf(stderr, "Initialization failed.\n");
        return 1;
    }

    /* Create a windowed mode window and its OpenGL context */
    window = glfwCreateWindow(width, height, "Hello World", NULL, NULL);
    if (!window) {
        glfwTerminate();
        fprintf(stderr, "Error creating window.\n");
        return 1;
    }

    /* Make the window's context current */
    glfwMakeContextCurrent(window);
    glfwSetInputMode(window, GLFW_STICKY_MOUSE_BUTTONS, 1);
    glfwSetKeyCallback(window, key_callback);

    //**************************** generowanie przykładowych piksli
    auxes[0].parent = -1;
    hs_init(&argc, &argv);
    initOctTree();
    hs_exit();
    camera_target = (Point3f) { cosf(horizontal_angle) * cosf(vertical_angle)
        , sinf(horizontal_angle) * cosf(vertical_angle)
            , sinf(vertical_angle)};
    float *piksele = malloc(height*width*3*sizeof(*piksele));
    calc_levels(0);

    fprintf(stderr, "sizeof(OctTreeNode)=%d\n", (int)sizeof(OctTreeNode));
    fprintf(stderr, "sizeof(CheatSheet)=%d\n", (int)sizeof(CheatSheet));
    fflush(stderr);

    //****************************

    init_cl();

    //dump_tree();
    cl_int status;

    cl_mem auxesCL = clCreateBuffer(context, CL_MEM_COPY_HOST_PTR, octTreeLength*sizeof(*auxes), auxes, &status);
    check_cl(status, "create auxes buffer");

    cl_mem tasksCL = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, 8*sizeof(int), mainOctTree[0].nodes, &status);
    check_cl(status, "create tasks buffer");

    cl_kernel find_neighbors = clCreateKernel(program, "find_neighbors", &status);
    check_cl(status, "create find_neighbors");

    status = clSetKernelArg(find_neighbors, 0, sizeof(cl_mem), &mainOctCL);
    check_cl(status, "set find_neighbors arg 0");
    status = clSetKernelArg(find_neighbors, 1, sizeof(cl_mem), &auxesCL);
    check_cl(status, "set find_neighbors arg 1");
    status = clSetKernelArg(find_neighbors, 2, sizeof(cl_mem), &tasksCL);
    check_cl(status, "set find_neighbors arg 2");

    size_t global_work_size = 8;
    size_t local_work_size = 1;
    status = clEnqueueNDRangeKernel(queue, find_neighbors, 1, NULL, &global_work_size, &local_work_size, 0, NULL, NULL);
    check_cl(status, "enqueue find_neighbors");

    status = clEnqueueReadBuffer(queue, mainOctCL, CL_FALSE, 0, octTreeLength*sizeof(*mainOctTree), mainOctTree, 0, NULL, NULL);
    check_cl(status, "read oct");

    status = clEnqueueReadBuffer(queue, auxesCL, CL_FALSE, 0, octTreeLength*sizeof(*auxes), auxes, 0, NULL, NULL);
    check_cl(status, "read aux");

    status = clFinish(queue);
    check_cl(status, "finish find_neighbors");

    mainOctCL = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, octTreeLength*sizeof(OctTreeNode), mainOctTree, &status);
    check_cl(status, "recreate mainOctTree");

    //dump_tree();

    fflush(stderr);

    double last_xpos = 0, last_ypos = 0;
    /* Loop until the user closes the window */
    while (!glfwWindowShouldClose(window))
    {
        /* Render here */
        for (int i = 0; i < height * width * 3; i++)
            piksele[i] = 0.0;

        double xpos, ypos;
        glfwGetCursorPos(window, &xpos, &ypos);
        xpos -= last_xpos;
        ypos -= last_ypos;
        last_xpos += xpos;
        last_ypos += ypos;
        if (glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_1) == GLFW_PRESS) {
            const double alpha = 0.001;
            horizontal_angle += (float)(xpos * alpha);
            vertical_angle += (float)(ypos * alpha);
            camera_target = (Point3f) { cosf(horizontal_angle) * cosf(vertical_angle)
                , sinf(horizontal_angle) * cosf(vertical_angle)
                    , sinf(vertical_angle)};
        }

        clock_t start = clock();
        captureOctTree(camera_pos, camera_target, up, width, height, piksele);
        clock_t end = clock();

        // show render time in window title
        char title[16];
        snprintf(title, 16, "%d ms", (int)((end - start) / (CLOCKS_PER_SEC / 1000)));
        glfwSetWindowTitle(window, title);

        /* Swap front and back buffers */
        glfwSwapBuffers(window);

        /* Poll for and process events */
        glfwPollEvents();
    }

    if (num_platforms > 0) {
        clReleaseMemObject(mainOctCL);
        clReleaseMemObject(image);
        clReleaseKernel(kernel);
        clReleaseCommandQueue(queue);
    }
    glfwDestroyWindow(window);

    glfwTerminate();
    return 0;
}

static void key_callback(GLFWwindow *window, int key, int scancode, int action, int mods)
{
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GL_TRUE);
    if (action == GLFW_PRESS || action == GLFW_REPEAT)
    {
        Point3f right = vectMul(camera_target, up);
        right = vectNormalize(right);
        switch (key) {
            case GLFW_KEY_W:
                camera_pos = vectMulScalar(camera_pos, camera_target, 0.05f);
                break;
            case GLFW_KEY_S:
                camera_pos = vectMulScalar(camera_pos, camera_target, -0.05f);
                break;
            case GLFW_KEY_A:
                camera_pos = vectMulScalar(camera_pos, right, -0.05f);
                break;
            case GLFW_KEY_D:
                camera_pos = vectMulScalar(camera_pos, right, 0.05f);
                break;
            case GLFW_KEY_Z:
                camera_pos = vectMulScalar(camera_pos, up, -0.05f);
                break;
            case GLFW_KEY_Q:
                camera_pos = vectMulScalar(camera_pos, up, 0.05f);
                break;
            case GLFW_KEY_I:
                vertical_angle += 0.05f;
                break;
            case GLFW_KEY_K:
                vertical_angle -= 0.05f;
                break;
            case GLFW_KEY_L:
                horizontal_angle -= 0.05f;
                break;
            case GLFW_KEY_J:
                horizontal_angle += 0.05f;
                break;
            case GLFW_KEY_T:
                AOV -= AOVd;
                break;
            case GLFW_KEY_Y:
                AOV += AOVd;
                break;
            case GLFW_KEY_M:
                light = camera_pos;
                break;
            case GLFW_KEY_P:
                fprintf(stderr, "Current rendering method is: ");
                if (render_method == Stacking) {
                    render_method = Stackless;
                    fprintf(stderr, "Stackless\n");
                } else if (render_method == Stackless && num_platforms > 0) {
                    render_method = TracerCL;
                    fprintf(stderr, "TracerCL\n");
                } else {
                    render_method = Stacking;
                    fprintf(stderr, "Stacking\n");
                }
                fflush(stderr);
                break;
                /*default:*/
        }
        camera_target = (Point3f) { cosf(horizontal_angle) * cosf(vertical_angle)
            , sinf(horizontal_angle) * cosf(vertical_angle)
                , sinf(vertical_angle)};
    }
}


void push_oct_tree_partial(int c0, int c1, int c2, int c3, int c4, int c5, int c6, int c7)
{
    int child_arr[8] = {c0, c1, c2, c3, c4, c5, c6, c7};

    for (int i = 0; i < 8; i++) {
        int x = (i / 4) % 2;
        int y = (i / 2) % 2;
        int z = i % 2;
        auxes[child_arr[i]].parent = octTreeLength;
        auxes[child_arr[i]].id = i;
        mainOctTree[octTreeLength].nodes[x][y][z] = child_arr[i];
    }

    // FIXME: ain't this nice
    auxes[octTreeLength].neighbors[0][0] = -1;
    auxes[octTreeLength].neighbors[0][1] = -1;
    auxes[octTreeLength].neighbors[1][0] = -1;
    auxes[octTreeLength].neighbors[1][1] = -1;
    auxes[octTreeLength].neighbors[2][0] = -1;
    auxes[octTreeLength].neighbors[2][1] = -1;

    octTreeLength++;
}

void push_oct_tree_solid(float r, float g, float b)
{
    mainOctTree[octTreeLength].type = Solid;
    mainOctTree[octTreeLength].color = (Color4f) {r,g,b,0};
    octTreeLength++;
}

void push_oct_tree_empty(void)
{
    mainOctTree[octTreeLength].type = Empty;
    octTreeLength++;
}

void calc_levels(int id) {
    if (auxes[id].parent != -1) {
        auxes[id].level = auxes[auxes[id].parent].level - 1;
    } else {
        auxes[id].level = 0;
        printf("zeroing %d\n", id);
    }
    if (id == 62594)
        printf("type %d\n", mainOctTree[id].type);
    if (mainOctTree[id].type >= 0) {
        for (int i = 0; i < 8; i++) {
            int x = (i / 4) % 2;
            int y = (i / 2) % 2;
            int z = i % 2;
            calc_levels(mainOctTree[id].nodes[x][y][z]);
        }
    }
}

static void dump_tree(void) {
    printf("dumping tree:\n");
    for (int i = 0; i < octTreeLength; i++) {
        if (mainOctTree[i].type == Solid) {
            printf("id: %d=%d, level: %d, type: Solid\n", i, auxes[i].id, auxes[i].level);
        } else if (mainOctTree[i].type == Empty) {
            printf("id: %d=%d, level: %d, type: Empty, neighbors: [[%d, %d], [%d, %d], [%d, %d]], levels: [[%d, %d], [%d, %d], [%d, %d]]\n", i, auxes[i].id, auxes[i].level,
                mainOctTree[i].neighbors[0][0],
                mainOctTree[i].neighbors[0][1],
                mainOctTree[i].neighbors[1][0],
                mainOctTree[i].neighbors[1][1],
                mainOctTree[i].neighbors[2][0],
                mainOctTree[i].neighbors[2][1],
                mainOctTree[i].levels[0][0],
                mainOctTree[i].levels[0][1],
                mainOctTree[i].levels[1][0],
                mainOctTree[i].levels[1][1],
                mainOctTree[i].levels[2][0],
                mainOctTree[i].levels[2][1]);
        } else {
            printf("id: %d=%d, level: %d, type: Partial, neighbors: [[%d, %d], [%d, %d], [%d, %d]], nodes: [%d, %d, %d, %d, %d, %d, %d, %d]\n", i, auxes[i].id, auxes[i].level,
                auxes[i].neighbors[0][0],
                auxes[i].neighbors[0][1],
                auxes[i].neighbors[1][0],
                auxes[i].neighbors[1][1],
                auxes[i].neighbors[2][0],
                auxes[i].neighbors[2][1],
                mainOctTree[i].nodes[0][0][0],
                mainOctTree[i].nodes[0][0][1],
                mainOctTree[i].nodes[0][1][0],
                mainOctTree[i].nodes[0][1][1],
                mainOctTree[i].nodes[1][0][0],
                mainOctTree[i].nodes[1][0][1],
                mainOctTree[i].nodes[1][1][0],
                mainOctTree[i].nodes[1][1][1]);
        }
    }
    fflush(stdout);
}

static void initOctTree(void)
{
    mainOctTree = malloc(1024 * 1024 * sizeof(*mainOctTree));
    octTreeLength = 0;

    load_file("model.json");

    fprintf(stderr, "Done loading.\n");
    fflush(stderr);
}
