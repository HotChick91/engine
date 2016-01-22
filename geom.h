#pragma once

#include "types.h"

Point3f vectMul(Point3f a, Point3f b);
Point3f vectMulScalar(Point3f a, Point3f b, float c);
Point3f vectSum(Point3f a, Point3f b, Point3f c);
Point3f vectDiv(Point3f a, float c);
Point3f vectScale(Point3f a, Point3f b);
Point3f vectNormalize(Point3f a);

int cmpDistData(const void * a, const void * b);
int vectPlaneIntersection(Point3f origin, Point3f direction, int plane, Point3f value);

float MAX(float a, float b);
void clamp(Point3f *p, float radius);
