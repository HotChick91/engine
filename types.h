#pragma once

#define ARR_IDX(x, y, col) ((((y) * width + (x)) * 3) + (col))
#define ARR_IDX4(x, y, col) ((((y) * width + (x)) * 4) + (col))

typedef struct {
    float x;
    float y;
    float z;
} Point3f;

typedef struct {
    float r;
    float g;
    float b;
    float a;
} Color4f;

#define Empty (-1)
#define Solid (-2)

typedef struct {
    float center[3];
    float radius;
    union {
        struct {
            Color4f color;
            short garbage[7];
            // type must overlap with significant bits of nodes
            // this won't work with big endian systems
            short type;
        };
        int nodes[2][2][2];
        struct {
            int neighbors[3][2];
            // 0 for root, -1 for its children, and so on
            char levels[3][2];
            char padding[2];
        };
    };
} OctTreeNode;

typedef struct {
    int id;
    int parent;
    int neighbors[3][2];
    int level;
} CheatSheet;

typedef struct {
    float dist;
    int plane;
} dist_data;

typedef enum { Stacking, Stackless, TracerCL } RenderMethod;
