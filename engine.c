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
	float dist;
	float* origin_a = (float*)(&origin);
	float* value_a = (float*)(&value);
	float* direction_a = (float*)(&direction);

	if (plane < 0 || plane >= 3) return 0;

	if (direction_a[plane] == 0
	 || (origin_a[plane] < value_a[plane] && direction_a[plane] < 0)
	 || (origin_a[plane] > value_a[plane] && direction_a[plane] > 0))
		return 0;
	dist = (value_a[plane] - origin_a[plane]) / direction_a[plane];

	Point3f intersect = vectMulScalar(origin, direction, dist);
	float* intersect_a = (float*)(&intersect);

	float i_1, v_1, i_2, v_2;
	i_1 = intersect_a[(plane + 1) % 3];
	v_1 = value_a[(plane + 1) % 3];
	i_2 = intersect_a[(plane + 2) % 3];
	v_2 = value_a[(plane + 2) % 3];

	if (v_1 < 0) {
		v_1 *= -1.0f;
		i_1 *= -1.0f;
	}
	if (v_2 < 0) {
		v_2 *= -1.0f;
		i_2 *= -1.0f;
	}
	return epsGteF(i_1, 0) && epsLteF(i_1, v_1) && epsGteF(i_2, 0) && epsLteF(i_2, v_1);
}

typedef struct Color3f {
	float r;
	float g;
	float b;
} Color3f;

enum OctTreeNodeType {Empty, Solid, Partial}; // TODO owrapować to ładnie, żeby nie robić syfu w top lvl

typedef struct OctTreeNode OctTreeNode;

struct OctTreeNode {
	int x, y, z;
	OctTreeNode *parent;
	Point3f center; // center point of cube
	float radius; // cube radius
	enum OctTreeNodeType type;
	union {
		Color3f color;
		OctTreeNode *nodes[2][2][2];
	};
};

Point3f camera_pos = {0.89f,-0.98f,-0.25f};
Point3f camera_target = {0.0f,1.0f,0.0f};
Point3f up = {0.f, 0.f, 1.f};
Point3f light = {1.0f, -2.0f, 0.0f};
float horizontal_angle = 2.0;
float vertical_angle = 0.0;

enum RenderMethod {Stacking, Stackless} render_method = Stackless;

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
	camera_target = (Point3f) { cosf(horizontal_angle) * cosf(vertical_angle)
							  , sinf(horizontal_angle) * cosf(vertical_angle)
							  , sinf(vertical_angle)};
	const int height = 500;
	const int width = 500;
	float * piksele = malloc(height*width*3*sizeof(*piksele));

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
			case GLFW_KEY_M:
				light = vectMulScalar(camera_pos, camera_target, 1.f);
				break;
			case GLFW_KEY_P:
				printf("Current rendering method is: ");
				if (render_method == Stacking) {
					render_method = Stackless;
					printf("Stackless");
				}
				else {
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
	return tree->parent->nodes[nx][ny][nz];
}

void calculate_light(Point3f p, Color3f * color)
{
	float l_dist2 = (light.x - p.x) * (light.x - p.x )
				  + (light.y - p.y) * (light.y - p.y )
				  + (light.z - p.z) * (light.z - p.z );
	float intensity = 2.0 / (2.0 + l_dist2);
	color->r *= intensity;
	color->g *= intensity;
	color->b *= intensity;
}

int ray_cast_oct_tree_stacking(Point3f origin, Point3f direction, OctTreeNode * tree, Color3f * color)
{
	// TODO check if origin inside tree?
	// TODO ładnie się wywalić jak tree == Null

	if (tree->type == Empty)
		return 0;
	if (tree->type == Solid) {
		*color = tree->color;
		return 2;
	}
	// if (tree->type == Partial)

	Point3f local = vectMulScalar(origin, tree->center, -1);
	float* local_a = (float*)(&local);
	float* direction_a = (float*)(&direction);

	// Check if we intersect 0-planes, and if so at what distance from origin
	struct dist_data intersection_dist[5];
	intersection_dist[0].dist = 0;
	intersection_dist[0].plane = -1;
	int intersection_size = 1;

	for (int axis = 0; axis < 3; axis++) {
		if (local_a[axis] * direction_a[axis] < 0) {
			intersection_dist[intersection_size].dist = -1 * local_a[axis] / direction_a[axis];
			intersection_dist[intersection_size].plane = axis;
			intersection_size++;
		}
	}

	// sortowanie
	qsort(intersection_dist, intersection_size, sizeof(*intersection_dist), cmpDistData);
	// aditional point for last intersection check
	intersection_dist[intersection_size].dist = intersection_dist[intersection_size-1].dist + 1.;
	intersection_dist[intersection_size].plane = -1;
	intersection_size++;

	// we need to check each specific segment if it intersects with cube
	for (int segment_num = 0; segment_num < intersection_size - 1; segment_num++) {
		float mid_point_distance = (intersection_dist[segment_num].dist + intersection_dist[segment_num+1].dist)/2;
		Point3f half_point = vectMulScalar(local, direction, mid_point_distance);

		Point3f final_collision;

		Point3f oct = {half_point.x < 0. ? -1 : 1, half_point.y < 0. ? -1 : 1, half_point.z < 0. ? -1 : 1};
		float* oct_a = (float*)(&oct);
		Point3f pl = {tree->radius * oct.x, tree->radius * oct.y, tree->radius * oct.z};

		int found = 0;

		if (!found && intersection_dist[segment_num].plane >= 0) {
			Point3f begin = vectMulScalar(local, direction, intersection_dist[segment_num].dist);
			float* begin_a = (float*)(&begin);
			int base_axis = intersection_dist[segment_num].plane;
			found = fabs(begin_a[ (base_axis+1)%3]) <= tree->radius
				 && fabs(begin_a[ (base_axis+2)%3]) <= tree->radius;
			if (found) {
				final_collision = begin;
			}
		}

		for (int axis = 0; axis < 3; axis++) {
			if(!found && direction_a[axis] * oct_a[axis] < 0 ){
				found = vectPlaneIntersection(local, direction, axis, pl);
				if(found) {
					float dist = (tree->radius - local_a[axis]) / direction_a[axis];
					final_collision = vectMulScalar(local, direction, dist);
				}
			}
		}

		if (found) {
			OctTreeNode * t = tree->nodes[half_point.x > 0][half_point.y > 0][half_point.z > 0];
			int ret = ray_cast_oct_tree_stacking(origin, direction, t, color);
			if (ret == 2)
			{
				calculate_light(final_collision, color);
			}
			if (ret) return 1;
		}
	}
	return 0;
}

void ray_cast_oct_tree_stackless(Point3f origin, Point3f direction, OctTreeNode * tree, Color3f * color)
{
	Point3f local, new_local;
	float xdist, ydist, zdist;
	int dx, dy, dz;

next_ray:

	local = vectMulScalar(origin, tree->center, -1);

	if (tree->type == Partial) {
		tree = tree->nodes[local.x > 0][local.y > 0][local.z > 0];
		goto next_ray;
	}

	if (tree->type == Solid) {
		*color = tree->color;
		calculate_light(origin, color);
		return;
	}

	// to prevent bad things that potentially could happen due to numerical errors
	clamp(&local, tree->radius);

	xdist = MAX((tree->radius - local.x) / direction.x, (-tree->radius - local.x) / direction.x);
	ydist = MAX((tree->radius - local.y) / direction.y, (-tree->radius - local.y) / direction.y);
	zdist = MAX((tree->radius - local.z) / direction.z, (-tree->radius - local.z) / direction.z);
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

	while (tree->parent != NULL) {
		OctTreeNode *sibling = maybeSibling(tree, dx, dy, dz);
		if (sibling != NULL) {
			origin = vectMulScalar(new_local, tree->center, 1);
			tree = sibling;
			goto next_ray;
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
	tmp->x = 0;
	tmp->y = 1;
	tmp->z = 1;
	mainOctTree->nodes[tmp->x][tmp->y][tmp->z] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){.5, .5, .5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {1.0, 0.0, 0.0};
	tmp->x = 1;
	tmp->y = 1;
	tmp->z = 1;
	mainOctTree->nodes[tmp->x][tmp->y][tmp->z] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){-.5, -.5, .5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {1.0, 0.0, 1.0};
	tmp->x = 0;
	tmp->y = 0;
	tmp->z = 1;
	mainOctTree->nodes[tmp->x][tmp->y][tmp->z] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){.5, -.5, .5};
	tmp->radius = .5;
	tmp->type = Empty;
	tmp->x = 1;
	tmp->y = 0;
	tmp->z = 1;
	mainOctTree->nodes[tmp->x][tmp->y][tmp->z] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){-.5, .5, -.5};
	tmp->radius = .5;
	tmp->type = Solid;
	tmp->color = (Color3f) {0.0, 1.0, 0.0};
	tmp->x = 0;
	tmp->y = 1;
	tmp->z = 0;
	mainOctTree->nodes[tmp->x][tmp->y][tmp->z] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){.5, .5, -.5};
	tmp->radius = .5;
	tmp->type = Empty;
	tmp->x = 1;
	tmp->y = 1;
	tmp->z = 0;
	mainOctTree->nodes[tmp->x][tmp->y][tmp->z] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){-.5, -.5, -.5};
	tmp->radius = .5;
	tmp->type = Empty;
	/*tmp->color = (Color3f) {1.0, 1.0, 0.0};*/
	tmp->x = 0;
	tmp->y = 0;
	tmp->z = 0;
	mainOctTree->nodes[tmp->x][tmp->y][tmp->z] = tmp;
	tmp = malloc(sizeof(*tmp));
	tmp->parent = mainOctTree;
	tmp->center = (Point3f){.5, -.5, -.5};
	tmp->radius = .5;
	tmp->type = Empty;
	tmp->x = 1;
	tmp->y = 0;
	tmp->z = 0;
	mainOctTree->nodes[tmp->x][tmp->y][tmp->z] = tmp;
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

	for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)
	{
		Color3f color = {0.,0.,0.};
		Point3f temp_target =
			vectMulScalar(vectMulScalar(bottom_left_vec, dup, (float)y), dright, (float)x);
		if (render_method == Stacking)
			ray_cast_oct_tree_stacking(camera, temp_target, mainOctTree, &color);
		else
			ray_cast_oct_tree_stackless(camera, temp_target, mainOctTree, &color);
		data[ARR_IDX(x,y,0)] = color.r;
		data[ARR_IDX(x,y,1)] = color.g;
		data[ARR_IDX(x,y,2)] = color.b;
	}

}
