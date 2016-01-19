#include <GLFW/glfw3.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

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
	Point3f center; // center point of cube
	float radius; // cube radius
	enum OctTreeNodeType type;
	union {
		Color3f color;
		struct {
			struct OctTreeNode* n0;
			OctTreeNode* n1;
			OctTreeNode* n2;
			OctTreeNode* n3;
			OctTreeNode* n4;
			OctTreeNode* n5;
			OctTreeNode* n6;
			OctTreeNode* n7;
		};
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
	const int height = 200;
	const int width = 200;
	float * piksele = (float*)malloc(height*width*3*sizeof(float));
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
		captureOctTree(camera_pos, camera_target, up, width, height, piksele);

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

// returns 0 if not found any objects
// returns 1 if object found, color is filled with color information
int ray_cast_oct_tree(Point3f origin, Point3f direction, OctTreeNode * tree, Color3f * color)
{
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
	float intersection_dist[5];
	intersection_dist[0] = 0;
	int intersection_size = 1;

	if (local.z * direction.z < 0) {
		intersection_dist[intersection_size] = -1 * local.z / direction.z;
		intersection_size++;
	}
	if (local.y * direction.y < 0) {
		intersection_dist[intersection_size] = -1 * local.y / direction.y;
		intersection_size++;
	}
	if (local.x * direction.x < 0) {
		intersection_dist[intersection_size] = -1 * local.x / direction.x;
		intersection_size++;
	}

	// sortowanie
	qsort(intersection_dist, intersection_size, sizeof(float), cmpFloat);
	// aditional point for last intersection check
	intersection_dist[intersection_size] = intersection_dist[intersection_size-1] + 1.;
	intersection_size++;

	// we need to check each specific segment if it intersects with cube
	for (int segment_num = 0; segment_num < intersection_size - 1; segment_num++)
	{
		// if segments begins/ends inside cube then it intersects
		Point3f begin = vectMulScalar(local, direction, intersection_dist[segment_num]);
		Point3f end = vectMulScalar(local, direction, intersection_dist[segment_num+1]);

		int found = 0;
		/*if ((abs(begin.x) < tree->radius*/
		 /*&& abs(begin.y) < tree->radius*/
		 /*&& abs(begin.z) < tree->radius)*/
		 /*|| (abs(end.x) < tree->radius*/
		 /*&& abs(end.y) < tree->radius*/
		 /*&& abs(end.z) < tree->radius))*/
		/*{*/
			/*found = 1;*/
		/*}*/
		// if segments ends are outside cube we will test intersect points with cube faces
		if(!found){
            found = vectPlaneIntersection(local, direction, 0, (Point3f){0, tree->radius, tree->radius});
        }
		if(!found){
            found = vectPlaneIntersection(local, direction, 1, (Point3f){tree->radius, 0, tree->radius});
        }
		if(!found){
            found = vectPlaneIntersection(local, direction, 2, (Point3f){tree->radius, tree->radius, 0});
		}

		if(!found){
            found = vectPlaneIntersection(local, direction, 0, (Point3f){tree->radius, tree->radius, tree->radius});
        }
		if(!found){
            found = vectPlaneIntersection(local, direction, 1, (Point3f){tree->radius, tree->radius, tree->radius});
        }
		if(!found){
            found = vectPlaneIntersection(local, direction, 2, (Point3f){tree->radius, tree->radius, tree->radius});
		}

		if (found)
		{
			OctTreeNode * t;
			Point3f half_point = {(begin.x + end.x)/2,(begin.y + end.y)/2,(begin.z + end.z)/2};
			if (half_point.z >= 0) // 0 1 2 3
			{
				if (half_point.y >= 0) // 0 1
				{
					if (half_point.x >= 0)
						t = tree->n1;
					else
						t = tree->n0;
				} else { // 2 3
					if (half_point.x >= 0)
						t = tree->n3;
					else
						t = tree->n2;
				}
			} else { // 4 5 6 7
				if (half_point.y >= 0) // 4 5
				{
					if (half_point.x >= 0)
						t = tree->n5;
					else
						t = tree->n4;
				} else { // 6 7
					if (half_point.x >= 0)
						t = tree->n7;
					else
						t = tree->n6;
				}
			}
			int ret = ray_cast_oct_tree(origin, direction, t, color);
			if (ret) return 1;
		}
		/*else {*/ //czy to nie sprawi jakichś problemów?
			/*break;*/
		/*}*/
	}
	return 0;
}

void initOctTree()
{
	mainOctTree = (OctTreeNode*)malloc(sizeof(OctTreeNode));
	mainOctTree->center = (Point3f){0.,0.,0.};
	mainOctTree->radius = 1.;
	mainOctTree->type = Partial;
	OctTreeNode* tmp;
	tmp = (OctTreeNode*)malloc(sizeof(OctTreeNode));
	tmp->center = (Point3f){-.5, .5, .5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {0.0, 0.0, 1.0};
	mainOctTree->n0 = tmp;
	tmp = (OctTreeNode*)malloc(sizeof(OctTreeNode));
	tmp->center = (Point3f){.5, .5, .5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {1.0, 0.0, 0.0};
	mainOctTree->n1 = tmp;
	tmp = (OctTreeNode*)malloc(sizeof(OctTreeNode));
	tmp->center = (Point3f){-.5, -.5, .5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {1.0, 0.0, 1.0};
	mainOctTree->n2 = tmp;
	tmp = (OctTreeNode*)malloc(sizeof(OctTreeNode));
	tmp->center = (Point3f){.5, -.5, .5};
	tmp->radius = .5;
	tmp->type = Empty;
	mainOctTree->n3 = tmp;
	tmp = (OctTreeNode*)malloc(sizeof(OctTreeNode));
	tmp->center = (Point3f){-.5, .5, -.5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {0.0, 1.0, 0.0};
	mainOctTree->n4 = tmp;
	tmp = (OctTreeNode*)malloc(sizeof(OctTreeNode));
	tmp->center = (Point3f){.5, .5, -.5};
	tmp->radius = .5;
	tmp->type = Empty;
	mainOctTree->n5 = tmp;
	tmp = (OctTreeNode*)malloc(sizeof(OctTreeNode));
	tmp->center = (Point3f){-.5, -.5, -.5};
	tmp->radius = .5;
	tmp->type = Empty;
	/*tmp->color = (Color3f) {1.0, 1.0, 0.0};*/
	mainOctTree->n6 = tmp;
	tmp = (OctTreeNode*)malloc(sizeof(OctTreeNode));
	tmp->center = (Point3f){.5, -.5, -.5};
	tmp->radius = .5;
	tmp->type = Empty;
	mainOctTree->n7 = tmp;
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
