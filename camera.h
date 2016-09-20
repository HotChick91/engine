#pragma once

void moveCamera(float forward_dist, float right_dist, float up_dist);
void moveCameraGlobal(float x, float y, float z);

void turnCamera(float pitch, float yaw, float roll);
void changeFOV(float delta);
void nextRenderMethod(void);

