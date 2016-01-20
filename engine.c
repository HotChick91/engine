#include <GLFW/glfw3.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

#define ARR_IDX(x, y, col) (((y) * width + (x)) * 3) + (col)

typedef struct Point3f {
	float x;
	float y;
	float z;
} Point3f;

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

Point3f vectNormalize(Point3f a)
{
	float len = sqrt(a.x * a.x + a.y * a.y + a.z * a.z);
	return vectDiv(a, len);
}

// check intersection of vector [origin; direction] with plane limited
// by value and versor plane (1 on planeth coordinate, 0 on the rest)
int vectPlaneIntersection(Point3f origin, Point3f direction, int plane, Point3f value)
{
    float dist;
    if (plane == 0) // X = value plane
    {
        if ((origin.x < value.x && direction.x <= 0)
         || (origin.x > value.x && direction.x >= 0))
            return 0;
        dist = (value.x - origin.x) / direction.x;
    } else if (plane == 1) // Y = value plane
    {
        if ((origin.y < value.y && direction.y <= 0)
         || (origin.y > value.y && direction.y >= 0))
            return 0;
        dist = (value.y - origin.y) / direction.y;
    } else if (plane == 2) // Z = value plane
    {
        if ((origin.z < value.z && direction.z <= 0)
         || (origin.z > value.z && direction.z >= 0))
            return 0;
        dist = (value.z - origin.z) / direction.z;
    } else {
        return 0;
    }

    Point3f intersect = vectMulScalar(origin, direction, dist);

    if (abs(intersect.x) <= value.x
     && abs(intersect.y) <= value.y
     && abs(intersect.z) <= value.z)
        return 1;
    return 0;
}

int cmpFloat(const void * a, const void * b)
{
	return *(float*)a < *(float*)b ? -1 : 1;
}

typedef struct Color3f {
	float r;
	float g;
	float b;
} Color3f;

enum OctTreeNodeType {Empty, Solid, Partial}; // TODO owrapować to ładnie, żeby nie robić syfu w top lvl

typedef struct OctTreeNode OctTreeNode;

struct OctTreeNode {
	OctTreeNode *parent;
	Point3f center; // center point of cube
	float radius; // cube radius
	enum OctTreeNodeType type;
	union {
		Color3f color;
		OctTreeNode *nodes[2][2][2];
	};
};

Point3f camera_pos = {0.89,-1.48,-0.25};
Point3f camera_target = {0.0,1.0,0.0};
Point3f up = {0., 0., 1.};
float horizontal_angle = 2.0;
float vertical_angle = 0.0;

OctTreeNode * mainOctTree;

static void error_callback(int error, const char* description);
static void key_callback(GLFWwindow* windows, int key, int scancode, int action, int mods);
void initOctTree();
void captureOctTree(Point3f camera, Point3f target, Point3f up, int width, int height, float* data);

int main(void)
{
	glfwSetErrorCallback(error_callback);

	GLFWwindow* window;

	/* Initialize the library */
	if (!glfwInit()){
		fprintf(stderr, "Initialization failed.\n");
		return -1;
	}

	/* Create a windowed mode window and its OpenGL context */
	window = glfwCreateWindow(500, 500, "Hello World", NULL, NULL);
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
	camera_target = (Point3f) { cos(horizontal_angle) * cos(vertical_angle)
							  , sin(horizontal_angle) * cos(vertical_angle)
							  , sin(vertical_angle)};
	const int height = 500;
	const int width = 500;
	float * piksele = malloc(height*width*3*sizeof(*piksele));
	/*for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)*/
	/*{*/
		/*int index_base = (y * width + x) * 3;*/
		/*piksele[index_base + 0] = (float)x / width ;*/
		/*piksele[index_base + 1] = (float)y / height;*/
		/*float x_frac = .5 - (float)x / width;*/
		/*float y_frac = .5 - (float)y / height;*/
		/*piksele[index_base + 2] = 1.0 - sqrt(x_frac * x_frac + y_frac * y_frac);*/
	/*}*/

	//****************************

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
		sprintf(title, "%d ms", (int)((end - start) / (CLOCKS_PER_SEC / 1000)));
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
				camera_pos = vectMulScalar(camera_pos, camera_target, 0.05);
				break;
			case GLFW_KEY_S:
				camera_pos = vectMulScalar(camera_pos, camera_target, -0.05);
				break;
			case GLFW_KEY_A:
				camera_pos = vectMulScalar(camera_pos, right, -0.05);
				break;
			case GLFW_KEY_D:
				camera_pos = vectMulScalar(camera_pos, right, 0.05);
				break;
			case GLFW_KEY_Z:
				camera_pos = vectMulScalar(camera_pos, up, -0.05);
				break;
			case GLFW_KEY_Q:
				camera_pos = vectMulScalar(camera_pos, up, 0.05);
				break;
			case GLFW_KEY_I:
				vertical_angle += 0.05;
				break;
			case GLFW_KEY_K:
				vertical_angle -= 0.05;
				break;
			case GLFW_KEY_L:
				horizontal_angle -= 0.05;
				break;
			case GLFW_KEY_J:
				horizontal_angle += 0.05;
				break;
			/*default:*/
		}
		camera_target = (Point3f) { cos(horizontal_angle) * cos(vertical_angle)
								  , sin(horizontal_angle) * cos(vertical_angle)
								  , sin(vertical_angle)};
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

void clamp(Point3f p, float radius) {
	if (p.x < -radius)
		p.x = -radius;
	else if (p.x > radius)
		p.x = radius;
	if (p.y < -radius)
		p.y = -radius;
	else if (p.y > radius)
		p.y = radius;
	if (p.z < -radius)
		p.z = -radius;
	else if (p.z > radius)
		p.z = radius;
}

OctTreeNode *maybeSibling(OctTreeNode *tree, int dx, int dy, int dz)
{
	// XXX: write this thing
	return NULL;
}

void ray_cast_oct_tree(Point3f origin, Point3f direction, OctTreeNode * tree, Color3f * color)
{
	if (tree->type == Solid) {
		*color = tree->color;
		return;
	}

	Point3f local = vectMulScalar(origin, tree->center, -1);
	Point3f new_local;

	if (tree->type == Partial) {
		ray_cast_oct_tree(origin, direction, tree->nodes[local.x < 0][local.y < 0][local.z < 0], color);
		return;
	}

	float xdist = MAX((tree->radius - local.x) / direction.x, (-tree->radius - local.x) / direction.x);
	float ydist = MAX((tree->radius - local.y) / direction.y, (-tree->radius - local.y) / direction.y);
	float zdist = MAX((tree->radius - local.z) / direction.z, (-tree->radius - local.z) / direction.z);
	int dx = 0, dy = 0, dz = 0;
	if (xdist < ydist && xdist < zdist) {
		new_local = vectMulScalar(local, direction, xdist);
		dx = direction.x > 0 ? 1 : -1;
		new_local.x = dx * tree->radius;
	} else if (ydist < zdist) {
		new_local = vectMulScalar(local, direction, ydist);
		dy = direction.y > 0 ? 1 : -1;
		new_local.y = dy * tree->radius;
	} else {
		new_local = vectMulScalar(local, direction, zdist);
		dz = direction.z > 0 ? 1 : -1;
		new_local.z = dz * tree->radius;
	}
	clamp(new_local, tree->radius);

	while (tree->parent != NULL) {
		OctTreeNode *sibling = maybeSibling(tree, dx, dy, dz);
		if (sibling != NULL) {
			// hopefully new_origin is valid here (ie, exactly on the face)
			ray_cast_oct_tree(vectMulScalar(new_local, tree->center, 1), direction, sibling, color);
			return;
		}
		tree = tree->parent;
	}

	color->r = color->g = color->b = 0;
}

void initOctTree()
{
	mainOctTree = malloc(sizeof(*mainOctTree));
	mainOctTree->parent = NULL;
	mainOctTree->center = (Point3f){0.,0.,0.};
	mainOctTree->radius = 1.;
	mainOctTree->type = Partial;
	OctTreeNode* tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){-.5, .5, .5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {0.0, 0.0, 1.0};
	mainOctTree->nodes[0][1][1] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){.5, .5, .5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {1.0, 0.0, 0.0};
	mainOctTree->nodes[1][1][1] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){-.5, -.5, .5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {1.0, 0.0, 1.0};
	mainOctTree->nodes[0][0][1] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){.5, -.5, .5};
	tmp->radius = .5;
	tmp->type = Empty;
	mainOctTree->nodes[1][0][1] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){-.5, .5, -.5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {0.0, 1.0, 0.0};
	mainOctTree->nodes[0][1][0] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){.5, .5, -.5};
	tmp->radius = .5;
	tmp->type = Empty;
	mainOctTree->nodes[1][1][0] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){-.5, -.5, -.5};
	tmp->radius = .5;
	tmp->type = Empty;
	/*tmp->color = (Color3f) {1.0, 1.0, 0.0};*/
	mainOctTree->nodes[0][0][0] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){.5, -.5, -.5};
	tmp->radius = .5;
	tmp->type = Empty;
	mainOctTree->nodes[1][0][0] = tmp;
}

void captureOctTree(Point3f camera, Point3f target, Point3f up, int width, int height, float* data)
{
	//normalize vectors
	target = vectNormalize(target);
	up = vectNormalize(up);

	Point3f right = vectMul(target, up);
	Point3f bottom_left_vec = vectSum(target, vectDiv(up, -2), vectDiv(right, -2));

	Point3f dright = vectDiv(right, width);
	Point3f dup = vectDiv(up, height);

	for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)
	{
		Color3f color = {0.,0.,0.};
		Point3f temp_target =
			vectMulScalar(vectMulScalar(bottom_left_vec, dup, y), dright, x);
		ray_cast_oct_tree(camera, temp_target, mainOctTree, &color);
		data[ARR_IDX(x,y,0)] = color.r;
		data[ARR_IDX(x,y,1)] = color.g;
		data[ARR_IDX(x,y,2)] = color.b;
	}

}
