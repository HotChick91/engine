#include "camera.h"

#include <math.h>
#include <stdio.h>

#include "geom.h"
#include "globals.h"
#include "types.h"

void moveCamera(float forward_dist, float right_dist, float up_dist)
{
    Point3f right_vec = vectNormalize(vectMul(camera_target, up));
    Point3f up_vec = vectNormalize(vectMul(right_vec, camera_target));

    camera_pos = vectMulScalar(camera_pos, camera_target, forward_dist * camera_speed);
    camera_pos = vectMulScalar(camera_pos, right_vec, right_dist * camera_speed);
    camera_pos = vectMulScalar(camera_pos, up_vec, up_dist * camera_speed);
}

void moveCameraGlobal(float x, float y, float z)
{
    camera_pos = vectMulScalar(camera_pos, (Point3f){x, y, z}, camera_speed);
}

void turnCamera(float pitch, float yaw, float roll)
{
    vertical_angle += pitch * camera_speed;
    horizontal_angle += yaw * camera_speed;
    // Roll is currently ignored
    camera_target = (Point3f) { cosf(horizontal_angle) * cosf(vertical_angle)
        , sinf(horizontal_angle) * cosf(vertical_angle)
        , sinf(vertical_angle)};
}

void changeFOV(float delta)
{
    AOV += AOVd * delta;
}
