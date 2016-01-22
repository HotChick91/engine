#include <GLFW/glfw3.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <CL/cl.h>

#define ARR_IDX(x, y, col) ((((y) * width + (x)) * 3) + (col))
#define ARR_IDX4(x, y, col) ((((y) * width + (x)) * 4) + (col))
typedef struct Point3f {
    float x;
    float y;
    float z;
} Point3f;

typedef struct Color4f {
    float r;
    float g;
    float b;
    float a;
} Color4f;

//enum OctTreeNodeType { Empty, Solid, Partial };
#define Empty 0
#define Solid 1
#define Partial 2

typedef struct OctTreeNode {
    char x, y, z;
    char type;
    int parent;
    union {
        Color4f color;
        int nodes[2][2][2];
    };
} OctTreeNode;

Point3f vectMul(Point3f a, Point3f b)
{
    return (Point3f){a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x};
}

Point3f vectMulScalar(Point3f a, Point3f b, float c)
{
    return (Point3f){a.x + b.x * c, a.y + b.y * c, a.z + b.z * c};
}

Point3f vectSum(Point3f a, Point3f b, Point3f c)
{
    return (Point3f){a.x + b.x + c.x, a.y + b.y + c.y, a.z + b.z + c.z};
}

Point3f vectDiv(Point3f a, float c)
{
    return (Point3f){a.x / c, a.y / c, a.z / c};
}

Point3f vectScale(Point3f a, Point3f b)
{
    return (Point3f){a.x * b.x, a.y * b.y, a.z * b.z};
}

Point3f vectNormalize(Point3f a)
{
    float len = sqrtf(a.x * a.x + a.y * a.y + a.z * a.z);
    return vectDiv(a, len);
}

struct dist_data{
    float dist;
    int plane;
};
int cmpDistData(const void * a, const void * b)
{
    return (*(struct dist_data*)a).dist < (*(struct dist_data*)b).dist ? -1 : 1;
}

const float eps = 0.000001f;
int epsEq(const float a, const float b)
{
    return fabs(a - b) < eps; // Not necessarily great numerically
}
int epsGteF(const float a, const float b)
{
    return a > b || epsEq(a,b) ; // Not necessarily great numerically
}
int epsLteF(const float a, const float b)
{
    return b > a || epsEq(a,b); // Not necessarily great numerically
}
int epsGteP(const Point3f a, const Point3f b)
{
    return epsGteF(a.x, b.x) && epsGteF(a.y, b.y) && epsGteF(a.z, b.z);
}
int epsLteP(const Point3f a, const Point3f b)
{
    return epsLteF(a.x, b.x) && epsLteF(a.y, b.y) && epsLteF(a.z, b.z);
}

// check intersection of vector [origin; direction] with plane limited
// by value and versor plane (1 on planeth coordinate, 0 on the rest)
int vectPlaneIntersection(Point3f origin, Point3f direction, int plane, Point3f value)
{
    // TODO nie ifować płaszczyzn - będzie potrzebne indexowanie zamiast .x, .y, .z
    float dist;
    if (plane == 0) // X = value plane
    {
        if (direction.x == 0
         || (origin.x < value.x && direction.x < 0)
         || (origin.x > value.x && direction.x > 0))
            return 0;
        dist = (value.x - origin.x) / direction.x;
    } else if (plane == 1) // Y = value plane
    {
        if (direction.y == 0
         || (origin.y < value.y && direction.y < 0)
         || (origin.y > value.y && direction.y > 0))
            return 0;
        dist = (value.y - origin.y) / direction.y;
    } else if (plane == 2) // Z = value plane
    {
        if (direction.z == 0
         || (origin.z < value.z && direction.z <= 0)
         || (origin.z > value.z && direction.z >= 0))
            return 0;
        dist = (value.z - origin.z) / direction.z;
    } else {
        return 0;
    }

    Point3f intersect = vectMulScalar(origin, direction, dist);

    float i_1, v_1, i_2, v_2;
    if (plane == 0) {
        i_1 = intersect.y;
        v_1 = value.y;
        i_2 = intersect.z;
        v_2 = value.z;
    } else if (plane == 1) {
        i_1 = intersect.x;
        v_1 = value.x;
        i_2 = intersect.z;
        v_2 = value.z;
    } else if (plane == 2) {
        i_1 = intersect.x;
        v_1 = value.x;
        i_2 = intersect.y;
        v_2 = value.y;
    } else {
        return 0;
    }
    if (v_1 < 0) {
        v_1 *= -1.0f;
        i_1 *= -1.0f;
    }
    if (v_2 < 0) {
        v_2 *= -1.0f;
        i_2 *= -1.0f;
    }
    return epsGteF(i_1, 0) && epsLteF(i_1, v_1) && epsGteF(i_2, 0) && epsLteF(i_2, v_1);

    // TODO sprawdzić który z tych wariantów jest szybszy (jeśli w ogóle jest jakakolwiek różnica)
    /*Point3f mul = {value.x < 0 ? -1.f : 1.f, value.y < 0 ? -1.f : 1.f, value.z < 0 ? -1.f : 1.f};*/
    /*int ret;*/
    /*ret = epsGteP(vectScale(intersect, mul), (Point3f) {0.f, 0.f, 0.f})*/
        /*&& epsLteP(vectScale(intersect, mul), vectScale(value, mul));*/
    /*if (!ret) return 0;*/
    return 1;
}

Point3f camera_pos = {0.89f,-0.98f,-0.25f};
Point3f camera_target = {0.0f,1.0f,0.0f};
Point3f up = {0.f, 0.f, 1.f};
float horizontal_angle = 2.0;
float vertical_angle = 0.0;

enum RenderMethod {Stacking, Stackless, TracerCL} render_method = Stacking;

OctTreeNode * mainOctTree;

static void error_callback(int error, const char* description);
static void key_callback(GLFWwindow* windows, int key, int scancode, int action, int mods);
void initOctTree();
void captureOctTree(Point3f camera, Point3f target, Point3f up, int width, int height, float* data);

void die(char *context, char *detail, int code) {
    fprintf(stderr, "FATAL ERROR: %s: %s (%d)\n", context, detail, code);
    exit(1);
}

void check_nn(void *ptr, char *msg) {
    if (ptr == NULL)
        die(msg, "null", 0);
}

void check_cl(cl_int status, char *msg) {
    if (status != CL_SUCCESS)
        die(msg, "opencl error", status);
}

void check_ferror(FILE *stream, char *msg) {
    int errord = ferror(stream);
    if (errord)
        die(msg, "ferror", errord);
}

cl_command_queue queue;
cl_kernel kernel;
cl_mem mainOctCL;
cl_mem image;
const int height = 1024;
const int width = 1024;
float *data4;

GLFWwindow* window;

int main(void)
{
    glfwSetErrorCallback(error_callback);

    /* Initialize the library */
    if (!glfwInit()){
        fprintf(stderr, "Initialization failed.\n");
        return -1;
    }

    /* Create a windowed mode window and its OpenGL context */
    window = glfwCreateWindow(width, height, "Hello World", NULL, NULL);
    if (!window) {
        glfwTerminate();
        fprintf(stderr, "Error creating window.\n");
        return -1;
    }

    /* Make the window's context current */
    glfwMakeContextCurrent(window);

    glfwSetKeyCallback(window, key_callback);

    //**************************** generowanie przykładowych piksli
    initOctTree();
    camera_target = (Point3f) { cosf(horizontal_angle) * cosf(vertical_angle)
                              , sinf(horizontal_angle) * cosf(vertical_angle)
                              , sinf(vertical_angle)};
    float * piksele = malloc(height*width*3*sizeof(*piksele));
    
    printf("sizeof(OctTreeNode)=%d\n", sizeof(OctTreeNode));

    //****************************

    // prepare your anus
    // i mean gpu
    cl_int status;
    cl_platform_id platform_id;
    cl_int num_platforms;
    char *kernel_src = malloc(10240);
    check_nn(kernel_src, "kernel_src");
    status = clGetPlatformIDs(1, &platform_id, &num_platforms);
    check_cl(status, "get platform ids");
    printf("#platforms: %d\n", num_platforms);
    cl_context_properties props[] = {CL_CONTEXT_PLATFORM, (cl_context_properties)platform_id, 0};

    char info[5][128];
    status = clGetPlatformInfo(platform_id, CL_PLATFORM_PROFILE, 128, info[0], NULL);
    check_cl(status, "get platform profile");
    status = clGetPlatformInfo(platform_id, CL_PLATFORM_VERSION, 128, info[1], NULL);
    check_cl(status, "get platform version");
    status = clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, 128, info[2], NULL);
    check_cl(status, "get platform name");
    status = clGetPlatformInfo(platform_id, CL_PLATFORM_VENDOR, 128, info[3], NULL);
    check_cl(status, "get platform vendor");
    status = clGetPlatformInfo(platform_id, CL_PLATFORM_EXTENSIONS, 128, info[4], NULL);
    check_cl(status, "get platform extensions");
    printf("profile: %s\n", info[0]);
    printf("version: %s\n", info[1]);
    printf("name: %s\n", info[2]);
    printf("vendor: %s\n", info[3]);
    printf("extensions: %s\n", info[4]);

    cl_context context = clCreateContextFromType(props, CL_DEVICE_TYPE_GPU, NULL, NULL, &status);
    check_cl(status, "create context");

    // create a command queue
    cl_device_id device_id;
    status = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_GPU, 1, &device_id, NULL);
    check_cl(status, "get device ids");
    queue = clCreateCommandQueueWithProperties(context, device_id, NULL, &status);
    check_cl(status, "create command queue");

    // allocate memory objects
    mainOctCL = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, 9*sizeof(OctTreeNode), mainOctTree, &status);
    check_cl(status, "create buffer");
    cl_image_format fmt = { CL_RGBA, CL_FLOAT };
    cl_image_desc desc;
    desc.image_type = CL_MEM_OBJECT_IMAGE2D;
    desc.image_width = width;
    desc.image_height = height;
    desc.image_row_pitch = 0;
    desc.image_slice_pitch = 0;
    desc.num_mip_levels = 0;
    desc.num_samples = 0;
    desc.mem_object = NULL;
    image = clCreateImage(context, CL_MEM_WRITE_ONLY, &fmt, &desc, NULL, &status);
    check_cl(status, "create image");

    data4 = malloc(width * height * 4 * sizeof(*data4));
    check_nn(data4, "data4");

    // create the compute program
    FILE *kernel_handle;
    fopen_s(&kernel_handle, "../ray.cl", "rb");
    check_nn(kernel_handle, "fopen ray.cl");
    size_t n_bytes = fread(kernel_src, 1, 10239, kernel_handle);
    kernel_src[n_bytes] = '\0';
    check_ferror(kernel_handle, "fread");
    cl_program program = clCreateProgramWithSource(context, 1, &kernel_src, NULL, &status);
    check_cl(status, "create program");

    // build the compute program executable
    status = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
    if (status != CL_BUILD_PROGRAM_FAILURE && status != CL_SUCCESS) {
        check_cl(status, "build program");
    } else {
        size_t log_size;
        status = clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size);
        check_cl(status, "get program build log size");

        char *log = malloc(log_size);
        check_nn(log, "log");

        status = clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, log_size, log, NULL);
        check_cl(status, "get program build log");

        fprintf(stderr, "build kernel log:\n%s\n", log);
    }

    // create the compute kernel
    kernel = clCreateKernel(program, "ray_cl", &status);
    check_cl(status, "create kernel");

    /* Loop until the user closes the window */
    while (!glfwWindowShouldClose(window))
    {
        /* Render here */
        for (int i = 0; i < height * width * 3; i++)
            piksele[i] = 0.0;

        clock_t start = clock();
        captureOctTree(camera_pos, camera_target, up, width, height, piksele);
        clock_t end = clock();

        // show render time in window title
        char title[16];
        snprintf(title, 16, "%d ms", (int)((end - start) / (CLOCKS_PER_SEC / 1000)));
        glfwSetWindowTitle(window, title);

        //Rysowanie ramki na około viewportu
        for (int i = 0; i < width; i++)
        {
            piksele[ARR_IDX(i, 0, 1)] = 1.;
            piksele[ARR_IDX(i, 0, 2)] = 1.;
            piksele[ARR_IDX(i, height - 1, 2)] = 1.;
        }
        for (int i = 0; i < height; i++)
        {
            piksele[ARR_IDX(0, i, 1)] = 1.;
            piksele[ARR_IDX(0, i, 2)] = 1.;
            piksele[ARR_IDX(width-1, i, 2)] = 1.;
        }
        // ostateczne rysowanie piksli powinno używać jakiejś innej techniki
        glDrawPixels(width, height, GL_RGB, GL_FLOAT, piksele);

        /* Swap front and back buffers */
        glfwSwapBuffers(window);

        /* Poll for and process events */
        glfwPollEvents();
    }

    clReleaseMemObject(mainOctCL);
    clReleaseMemObject(image);
    clReleaseProgram(program);
    clReleaseKernel(kernel);
    clReleaseCommandQueue(queue);
    clReleaseContext(context);

    glfwDestroyWindow(window);

    glfwTerminate();
    return 0;
}

void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GL_TRUE);
    if (action == GLFW_PRESS || action == GLFW_REPEAT)
    {
        Point3f right = vectMul(camera_target, up);
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
            case GLFW_KEY_P:
                printf("Current rendering method is: ");
                if (render_method == Stacking) {
                    render_method = Stackless;
                    printf("Stackless");
                } else if (render_method == Stackless) {
                    render_method = TracerCL;
                    printf("TracerCL");
                } else {
                    render_method = Stacking;
                    printf("Stacking");
                }
                printf("\n");
                break;
            /*default:*/
        }
        camera_target = (Point3f) { cosf(horizontal_angle) * cosf(vertical_angle)
                                  , sinf(horizontal_angle) * cosf(vertical_angle)
                                  , sinf(vertical_angle)};
    }
    printf("Camera position is: (%f, %f %f)\n", camera_pos.x, camera_pos.y, camera_pos.z);
    printf("Horizontal angle: %f, Vertical angle: %f\n", horizontal_angle, vertical_angle);
    printf("Camera target is: (%f, %f %f)\n", camera_target.x, camera_target.y, camera_target.z);
}

void error_callback(int error, const char* description)
{
    fprintf(stderr, "(%d) %s\n", error, description);
}

float MAX(float a, float b) {
    return a > b ? a : b;
}

void clamp(Point3f *p, float radius) {
    if (p->x < -radius)
        p->x = -radius;
    else if (p->x > radius)
        p->x = radius;
    if (p->y < -radius)
        p->y = -radius;
    else if (p->y > radius)
        p->y = radius;
    if (p->z < -radius)
        p->z = -radius;
    else if (p->z > radius)
        p->z = radius;
}

OctTreeNode *maybeSibling(OctTreeNode *tree, int dx, int dy, int dz)
{
    int nx = tree->x + dx;
    int ny = tree->y + dy;
    int nz = tree->z + dz;
    if ((nx | ny | nz) & (~1))
        return NULL;
    return mainOctTree + mainOctTree[tree->parent].nodes[nx][ny][nz];
}

int ray_cast_oct_tree_stacking(Point3f origin, Point3f direction, OctTreeNode * tree, Color4f * color)
{
#if 0
    // TODO check if origin inside tree?
    // TODO ładnie się wywalić jak tree == Null

    if (tree->type == Empty)
        return 0;
    if (tree->type == Solid) {
        *color = tree->color;
        return 1;
    }
    // if (tree->type == Partial)

    Point3f local = vectMulScalar(origin, tree->center, -1);

    // Check if we intersect 0-planes, and if so at what distance from origin
    struct dist_data intersection_dist[5];
    intersection_dist[0].dist = 0;
    intersection_dist[0].plane = -1;
    int intersection_size = 1;

    if (local.x * direction.x < 0) {
        intersection_dist[intersection_size].dist = -1 * local.x / direction.x;
        intersection_dist[intersection_size].plane = 0;
        intersection_size++;
    }
    if (local.y * direction.y < 0) {
        intersection_dist[intersection_size].dist = -1 * local.y / direction.y;
        intersection_dist[intersection_size].plane = 1;
        intersection_size++;
    }
    if (local.z * direction.z < 0) {
        intersection_dist[intersection_size].dist = -1 * local.z / direction.z;
        intersection_dist[intersection_size].plane = 2;
        intersection_size++;
    }

    // sortowanie
    qsort(intersection_dist, intersection_size, sizeof(*intersection_dist), cmpDistData);
    // aditional point for last intersection check
    intersection_dist[intersection_size].dist = intersection_dist[intersection_size-1].dist + 1.;
    intersection_dist[intersection_size].plane = -1;
    intersection_size++;

    // we need to check each specific segment if it intersects with cube
    for (int segment_num = 0; segment_num < intersection_size - 1; segment_num++)
    {
        float mid_point_distance = (intersection_dist[segment_num].dist + intersection_dist[segment_num+1].dist)/2;
        Point3f half_point = vectMulScalar(local, direction, mid_point_distance);

        Point3f oct = {half_point.x < 0. ? -1 : 1, half_point.y < 0. ? -1 : 1, half_point.z < 0. ? -1 : 1};
        Point3f pl = {tree->radius * oct.x, tree->radius * oct.y, tree->radius * oct.z};

        int found = 0;

        if (!found && intersection_dist[segment_num].plane >= 0)
        {
            Point3f begin = vectMulScalar(local, direction, intersection_dist[segment_num].dist);
            if (intersection_dist[segment_num].plane == 0)
                found = fabs(begin.y) <= tree->radius && fabs(begin.z) <= tree->radius;
            else if (intersection_dist[segment_num].plane == 1)
                found = fabs(begin.x) <= tree->radius && fabs(begin.z) <= tree->radius;
            else if (intersection_dist[segment_num].plane == 2)
                found = fabs(begin.x) <= tree->radius && fabs(begin.y) <= tree->radius;
        }
        //each end is also a begin
        /*if (!found && intersection_dist[segment_num+1].plane >= 0)
        {
            Point3f end = vectMulScalar(local, direction, intersection_dist[segment_num+1].dist);
            if (intersection_dist[segment_num+1].plane == 0)
                found = fabs(end.y) <= tree->radius && fabs(end.z) <= tree->radius;
            else if (intersection_dist[segment_num+1].plane == 1)
                found = fabs(end.x) <= tree->radius && fabs(end.z) <= tree->radius;
            else if (intersection_dist[segment_num+1].plane == 2)
                found = fabs(end.x) <= tree->radius && fabs(end.y) <= tree->radius;
        }*/

        if(!found){
            found = vectPlaneIntersection(local, direction, 0, pl);
        }
        if(!found){
            found = vectPlaneIntersection(local, direction, 1, pl);
        }
        if(!found){
            found = vectPlaneIntersection(local, direction, 2, pl);
        }

        if (found) // TODO fix after mergin stackless
        {
            OctTreeNode * t;
            t = tree->nodes[half_point.x > 0][half_point.y > 0][half_point.z > 0];
            int ret = ray_cast_oct_tree_stacking(origin, direction, t, color);
            if (ret) return 1;
        }
    }
#endif
    return 0;
}

void ray_cast_oct_tree_stackless(Point3f origin, Point3f direction, OctTreeNode * tree, Color4f * color)
{
    Point3f local, new_local;
    float xdist, ydist, zdist;
    int dx, dy, dz;

    Point3f center = (Point3f) { 0, 0, 0 };
    float radius = 1;

next_ray:

    local = vectMulScalar(origin, center, -1);

    if (tree->type == Partial) {

        dx = local.x > 0;
        dy = local.y > 0;
        dz = local.z > 0;
        tree = mainOctTree + tree->nodes[dx][dy][dz];

        radius /= 2.f;
        center = (Point3f) {
            center.x + (2 * dx - 1) * radius,
                center.y + (2 * dy - 1) * radius,
                center.z + (2 * dz - 1) * radius
        };
        goto next_ray;
    }

    if (tree->type == Solid) {
        *color = tree->color;
        return;
    }

    // to prevent bad things that potentially could happen due to numerical errors
    clamp(&local, radius);

    xdist = MAX((radius - local.x) / direction.x, (-radius - local.x) / direction.x);
    ydist = MAX((radius - local.y) / direction.y, (-radius - local.y) / direction.y);
    zdist = MAX((radius - local.z) / direction.z, (-radius - local.z) / direction.z);
    dx = 0, dy = 0, dz = 0;
    if (xdist < ydist && xdist < zdist) {
        new_local = vectMulScalar(local, direction, xdist);
        dx = direction.x > 0 ? 1 : -1;
    } else if (ydist < zdist) {
        new_local = vectMulScalar(local, direction, ydist);
        dy = direction.y > 0 ? 1 : -1;
    } else {
        new_local = vectMulScalar(local, direction, zdist);
        dz = direction.z > 0 ? 1 : -1;
    }

    while (tree->parent != -1) {
        OctTreeNode *sibling = maybeSibling(tree, dx, dy, dz);
        if (sibling != NULL) {
            origin = vectMulScalar(new_local, center, 1);
            tree = sibling;

            // radius stays the same
            center = (Point3f) { center.x + (2 * dx) * radius,
                                 center.y + (2 * dy) * radius,
                                 center.z + (2 * dz) * radius };

            goto next_ray;
        }
		center = (Point3f) {
			center.x - (2 * tree->x - 1) * radius,
				center.y - (2 * tree->y - 1) * radius,
				center.z - (2 * tree->z - 1) * radius
		};
		radius *= 2.f;
        tree = mainOctTree + tree->parent;
    }
    color->r = color->g = color->b = color->a = 0;
}
void initOctTree()
{
    mainOctTree = malloc(9*sizeof(*mainOctTree));
    mainOctTree->parent = -1;
    mainOctTree->type = Partial;
    OctTreeNode tmp;
    tmp.parent = 0;
    tmp.type = Solid;
    tmp.color = (Color4f) {0.0, 0.0, 1.0, 0.0};
    tmp.x = 0;
    tmp.y = 1;
    tmp.z = 1;
    mainOctTree[0].nodes[tmp.x][tmp.y][tmp.z] = 1;
    mainOctTree[1] = tmp;
    tmp.parent = 0;
    tmp.type = Solid;
    tmp.color = (Color4f) {1.0, 0.0, 0.0, 0.0};
    tmp.x = 1;
    tmp.y = 1;
    tmp.z = 1;
    mainOctTree[0].nodes[tmp.x][tmp.y][tmp.z] = 2;
    mainOctTree[2] = tmp;
    tmp.parent = 0;
    tmp.type = Solid;
    tmp.color = (Color4f) {1.0, 0.0, 1.0, 0.0};
    tmp.x = 0;
    tmp.y = 0;
    tmp.z = 1;
    mainOctTree[0].nodes[tmp.x][tmp.y][tmp.z] = 3;
    mainOctTree[3] = tmp;
    tmp.parent = 0;
    tmp.type = Empty;
    tmp.x = 1;
    tmp.y = 0;
    tmp.z = 1;
    mainOctTree[0].nodes[tmp.x][tmp.y][tmp.z] = 4;
    mainOctTree[4] = tmp;
    tmp.parent = 0;
    tmp.type = Solid;
    tmp.color = (Color4f) {0.0, 1.0, 0.0, 0.0};
    tmp.x = 0;
    tmp.y = 1;
    tmp.z = 0;
    mainOctTree[0].nodes[tmp.x][tmp.y][tmp.z] = 5;
    mainOctTree[5] = tmp;
    tmp.parent = 0;
    tmp.type = Empty;
    tmp.x = 1;
    tmp.y = 1;
    tmp.z = 0;
    mainOctTree[0].nodes[tmp.x][tmp.y][tmp.z] = 6;
    mainOctTree[6] = tmp;
    tmp.parent = 0;
    tmp.type = Empty;
    tmp.x = 0;
    tmp.y = 0;
    tmp.z = 0;
    mainOctTree[0].nodes[tmp.x][tmp.y][tmp.z] = 7;
    mainOctTree[7] = tmp;
    tmp.parent = 0;
    tmp.type = Empty;
    tmp.x = 1;
    tmp.y = 0;
    tmp.z = 0;
    mainOctTree[0].nodes[tmp.x][tmp.y][tmp.z] = 8;
    mainOctTree[8] = tmp;
}

void captureOctTree(Point3f camera, Point3f target, Point3f up, int width, int height, float* data)
{
    //normalize vectors
    target = vectNormalize(target);
    up = vectNormalize(up);

    Point3f right = vectMul(target, up);
    Point3f bottom_left_vec = vectSum(target, vectDiv(up, -2), vectDiv(right, -2));

    Point3f dright = vectDiv(right, (float)width);
    Point3f dup = vectDiv(up, (float)height);

    if (render_method == TracerCL) {
        // set the args values
        cl_int status;
        status = clSetKernelArg(kernel, 0, sizeof(cl_float3), &camera);
        check_cl(status, "set arg 0");
        status = clSetKernelArg(kernel, 1, sizeof(cl_float3), &bottom_left_vec);
        check_cl(status, "set arg 1");
        status = clSetKernelArg(kernel, 2, sizeof(cl_float3), &dup);
        check_cl(status, "set arg 2");
        status = clSetKernelArg(kernel, 3, sizeof(cl_float3), &dright);
        check_cl(status, "set arg 3");
        status = clSetKernelArg(kernel, 4, sizeof(cl_mem), &mainOctCL);
        check_cl(status, "set arg 4");
        status = clSetKernelArg(kernel, 5, sizeof(cl_mem), &image);
        check_cl(status, "set arg 5");

        // run kernel
        size_t global_work_size[] = {width, height};
        size_t local_work_size[] = {8, 8};
        // XXX: is event necessary?
        cl_event event;
        status = clEnqueueNDRangeKernel(queue, kernel, 2, NULL, global_work_size, local_work_size, 0, NULL, &event);
        check_cl(status, "enqueue kernel");

        size_t offset[] = {0, 0, 0};
        size_t dims[] = {width, height, 1};
        
        status = clEnqueueReadImage(queue, image, 1, offset, dims, 0, 0, data4, 1, &event, NULL);
        check_cl(status, "read");

        status = clReleaseEvent(event);
        check_cl(status, "release event");

        status = clFinish(queue);
        check_cl(status, "finish");
    }

    for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)
    {
        Color4f color = {0.,0.,0.};
        if (render_method == TracerCL) {
            color.r = data4[ARR_IDX4(x, y, 0)];
            color.g = data4[ARR_IDX4(x, y, 1)];
            color.b = data4[ARR_IDX4(x, y, 2)];
        } else {
            Point3f temp_target =
                vectMulScalar(vectMulScalar(bottom_left_vec, dup, (float)y), dright, (float)x);
            if (render_method == Stacking)
                ray_cast_oct_tree_stacking(camera, temp_target, mainOctTree, &color);
            else
                ray_cast_oct_tree_stackless(camera, temp_target, mainOctTree, &color);
        }
        data[ARR_IDX(x, y, 0)] = color.r;
        data[ARR_IDX(x, y, 1)] = color.g;
        data[ARR_IDX(x, y, 2)] = color.b;
    }
}
