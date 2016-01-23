#include "render.h"

#include <math.h>
#include <stdlib.h>

#include "cl.h"
#include "error.h"
#include "geom.h"
#include "globals.h"
#include "types.h"

static OctTreeNode *maybeSibling(OctTreeNode *tree, int dx, int dy, int dz)
{
    int nx = tree->x + dx;
    int ny = tree->y + dy;
    int nz = tree->z + dz;
    if ((nx | ny | nz) & (~1))
        return NULL;
    return mainOctTree + mainOctTree[tree->parent].nodes[nx][ny][nz];
}

static void calculate_light(Point3f p, Color4f * color)
{
    float l_dist2 = (light.x - p.x) * (light.x - p.x)
        + (light.y - p.y) * (light.y - p.y)
        + (light.z - p.z) * (light.z - p.z);
    float intensity = 2.f / (2.f + l_dist2);
    color->r *= intensity;
    color->g *= intensity;
    color->b *= intensity;
}

static int ray_cast_oct_tree_stacking(Point3f origin, Point3f direction, OctTreeNode * tree, float radius, Point3f center, Color4f * color)
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

    Point3f local = vectMulScalar(origin, center, -1);
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
    intersection_dist[intersection_size].dist = intersection_dist[intersection_size - 1].dist + 1.f;
    intersection_dist[intersection_size].plane = -1;
    intersection_size++;

    // we need to check each specific segment if it intersects with cube
    for (int segment_num = 0; segment_num < intersection_size - 1; segment_num++) {
        float mid_point_distance = (intersection_dist[segment_num].dist + intersection_dist[segment_num + 1].dist) / 2;
        Point3f half_point = vectMulScalar(local, direction, mid_point_distance);

        Point3f final_collision;

        Point3f oct = {half_point.x < 0. ? -1.f : 1.f, half_point.y < 0. ? -1.f : 1.f, half_point.z < 0. ? -1.f : 1.f};
        float* oct_a = (float*)(&oct);
        Point3f pl = {radius * oct.x, radius * oct.y, radius * oct.z};

        int found = 0;

        if (!found && intersection_dist[segment_num].plane >= 0) {
            Point3f begin = vectMulScalar(local, direction, intersection_dist[segment_num].dist);
            float* begin_a = (float*)(&begin);
            int base_axis = intersection_dist[segment_num].plane;
            found = fabsf(begin_a[(base_axis + 1) % 3]) <= radius
                && fabsf(begin_a[(base_axis + 2) % 3]) <= radius;
            if (found) {
                final_collision = begin;
            }
        }

        for (int axis = 0; axis < 3; axis++) {
            if (!found && direction_a[axis] * oct_a[axis] < 0) {
                found = vectPlaneIntersection(local, direction, axis, pl);
                if (found) {
                    float dist = (radius - local_a[axis]) / direction_a[axis];
                    final_collision = vectMulScalar(local, direction, dist);
                }
            }
        }

        if (found) {
            OctTreeNode * t = mainOctTree + tree->nodes[half_point.x > 0][half_point.y > 0][half_point.z > 0];
            Point3f new_pos;
            new_pos.x = center.x + (radius / 2.f) * oct.x;
            new_pos.y = center.y + (radius / 2.f) * oct.y;
            new_pos.z = center.z + (radius / 2.f) * oct.z;
            int ret = ray_cast_oct_tree_stacking(origin, direction, t, radius / 2.f, new_pos, color);
            if (ret == 2)
            {
                calculate_light(final_collision, color);
            }
            if (ret) return 1;
        }
    }
    return 0;
}

static void ray_cast_oct_tree_stackless(Point3f origin, Point3f direction, OctTreeNode * tree, Color4f * color)
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
        calculate_light(origin, color);
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
            center = (Point3f) {
                center.x + (2 * dx) * radius,
                center.y + (2 * dy) * radius,
                center.z + (2 * dz) * radius
            };

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

void captureOctTree(Point3f camera, Point3f target, Point3f up, int width, int height, float* data)
{
    //normalize vectors
    target = vectNormalize(target);
    up = vectNormalize(up);

    Point3f right = vectMul(target, up);
    Point3f bottom_left_vec = vectSum(target, vectDiv(up, -2), vectDiv(right, -2));

    Point3f dright = vectDiv(right, (float)width);
    Point3f dup = vectDiv(up, (float)height);

#if TRACER_CL
    if (render_method == TracerCL) {
        // set the args values
        cl_int status;

        status = clEnqueueAcquireGLObjects(queue, 1, &image, 0, NULL, NULL);
        check_cl(status, "enqueue gl");

        status = clSetKernelArg(kernel, 0, sizeof(cl_float3), &camera);
        check_cl(status, "set arg 0");
        status = clSetKernelArg(kernel, 1, sizeof(cl_float3), &light);
        check_cl(status, "set arg 1");
        status = clSetKernelArg(kernel, 2, sizeof(cl_float3), &bottom_left_vec);
        check_cl(status, "set arg 2");
        status = clSetKernelArg(kernel, 3, sizeof(cl_float3), &dup);
        check_cl(status, "set arg 3");
        status = clSetKernelArg(kernel, 4, sizeof(cl_float3), &dright);
        check_cl(status, "set arg 4");
        status = clSetKernelArg(kernel, 5, sizeof(cl_mem), &mainOctCL);
        check_cl(status, "set arg 5");
        status = clSetKernelArg(kernel, 6, sizeof(cl_mem), &image);
        check_cl(status, "set arg 6");

        // run kernel
        size_t global_work_size[] = {width, height};
        size_t local_work_size[] = {8, 8};
        status = clEnqueueNDRangeKernel(queue, kernel, 2, NULL, global_work_size, local_work_size, 0, NULL, NULL);
        check_cl(status, "enqueue kernel");

        size_t offset[] = {0, 0, 0};
        size_t dims[] = {width, height, 1};
        status = clEnqueueReleaseGLObjects(queue, 1, &image, 0, NULL, NULL);
        check_cl(status, "release gl");

        status = clFinish(queue);
        check_cl(status, "finish");

        glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
        glClear(GL_COLOR_BUFFER_BIT);
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, texture);

        glBegin(GL_QUADS);
        glTexCoord2i(0, 1);
        glVertex2f(-1, 1.0f);
        glTexCoord2i(1, 1);
        glVertex2f(1.0f, 1.0f);
        glTexCoord2i(1, 0);
        glVertex2f(1.0f, -1.f);
        glTexCoord2i(0, 0);
        glVertex2f(-1, -1.f);
        glEnd();
        glDisable(GL_TEXTURE_2D);
        glFinish();
    } else {
#endif
        for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)
        {
            Color4f color = {0.,0.,0.};
            Point3f temp_target =
                vectMulScalar(vectMulScalar(bottom_left_vec, dup, (float)y), dright, (float)x);
            if (render_method == Stacking)
                ray_cast_oct_tree_stacking(camera, temp_target, mainOctTree, 1.0, (Point3f) { 0, 0, 0 }, &color);
            else
                ray_cast_oct_tree_stackless(camera, temp_target, mainOctTree, &color);
            data[ARR_IDX(x, y, 0)] = color.r;
            data[ARR_IDX(x, y, 1)] = color.g;
            data[ARR_IDX(x, y, 2)] = color.b;
        }

        glDrawPixels(width, height, GL_RGB, GL_FLOAT, data);
#if TRACER_CL
    }
#endif
}
