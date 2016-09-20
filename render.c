#include "render.h"

#include <math.h>
#include <stdlib.h>
#include <GL/gl.h>

#include "cl.h"
#include "error.h"
#include "geom.h"
#include "globals.h"
#include "types.h"

void captureOctTree(Point3f camera, Point3f target, Point3f up, int width, int height, float *data)
{
    //normalize vectors
    target = vectNormalize(target);
    up = vectNormalize(up);

    Point3f right = vectNormalize(vectMul(target, up));
    Point3f relative_up = vectNormalize(vectMul(right, camera_target));

    target = vectDiv(target, tanf(AOV / 2.f));
    Point3f bottom_left_vec = vectSum(target, vectDiv(relative_up, -2), vectDiv(right, -2));

    Point3f dright = vectDiv(right, (float)width);
    Point3f dup = vectDiv(relative_up, (float)height);

    cl_int status;

    status = clEnqueueAcquireGLObjects(queue, 1, &image, 0, NULL, NULL);
    check_cl(status, "enqueue gl");

    // find the camera
    Point3f origin = camera;
    OctTreeNode *tree = mainOctTree;
    int dx, dy, dz;
    float radius = 1.f;
    Point3f center = (Point3f) { 0.f, 0.f, 0.f };
    Point3f local = vectMulScalar(origin, center, -1);
    cl_int offset = 0;
    while (tree->type >= 0) {
        dx = local.x > 0;
        dy = local.y > 0;
        dz = local.z > 0;
        offset = tree->nodes[dx][dy][dz];
        tree = mainOctTree + offset;

        radius /= 2.f;
        center = (Point3f) {
            center.x + (2 * dx - 1) * radius,
            center.y + (2 * dy - 1) * radius,
            center.z + (2 * dz - 1) * radius
        };

        local = vectMulScalar(origin, center, -1);
    }

    Point3f camera111 = vectMulScalar(origin, (Point3f) { 1.f, 1.f, 1.f }, 1.f);
    Point3f light111 = vectMulScalar(light, (Point3f) { 1.f, 1.f, 1.f }, 1.f);

    // cl_float3 is bigger than Point3f but we're only passing stack-allocated stuff, so reading garbage is safe
    status = clSetKernelArg(kernel, 0, sizeof(cl_float3), &camera111);
    check_cl(status, "set arg 0");
    status = clSetKernelArg(kernel, 1, sizeof(cl_float3), &light111);
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
    status = clSetKernelArg(kernel, 7, sizeof(cl_int), &offset);
    check_cl(status, "set arg 7");

    // run kernel
    size_t global_work_size[] = {width, height};
    size_t local_work_size[] = {8, 8};
    status = clEnqueueNDRangeKernel(queue, kernel, 2, NULL, global_work_size, local_work_size, 0, NULL, NULL);
    check_cl(status, "enqueue kernel");

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
}
