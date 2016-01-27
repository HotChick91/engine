#define NULL ((void *)0)

#define Empty 0
#define Solid 1
#define Partial 2

typedef struct OctTreeNode {
    char x, y, z;
    char type;
    int parent;
    union {
        float4 color;
        int nodes[2][2][2];
    };
} __attribute__((packed)) OctTreeNode;

typedef struct Matrix {
    float v[3][3];
} Matrix3f;

float3 multiplyVectMatrix(float3 v, Matrix3f m)
{
    float3 res;
    res.x = v.x * m.v[0][0] + v.y * m.v[1][0] + v.z * m.v[2][0];
    res.y = v.x * m.v[0][1] + v.y * m.v[1][1] + v.z * m.v[2][1];
    res.z = v.x * m.v[0][2] + v.y * m.v[1][2] + v.z * m.v[2][2];
    return res;
}

Matrix3f createRotationMatrix(float3 axis, float angle)
{
    Matrix3f ret;
    float l = axis.x;
    float m = axis.y;
    float n = axis.z;
    float c = cos(angle);
    float s = sin(angle);
    float c_ = 1 - c;

    ret.v[0][0] = l * l * c_ + c;
    ret.v[0][1] = m * l * c_ - n * s;
    ret.v[0][2] = n * l * c_ + m * s;

    ret.v[1][0] = l * m * c_ + n * s;
    ret.v[1][1] = m * m * c_ + c;
    ret.v[1][2] = n * m * c_ - l * s;

    ret.v[2][0] = l * n * c_ - m * s;
    ret.v[2][1] = m * n * c_ + l * s;
    ret.v[2][2] = n * n * c_ + c;

    return ret;
}

// TODO: see if using sign, step, mix, etc. would be a good idea
kernel void ray_cl(float3 origin, float3 target, float3 light, int width, int height, float horizontal_AOV, float vertical_AOV, float3 up, float3 right, global OctTreeNode *trees, write_only image2d_t image)
{
    int2 my_px;
    float3 relative, new_relative;
    float xdist, ydist, zdist, mindist;
    int dx, dy, dz;
    // TODO: see if OctTreeNode instead of a pointer makes any difference
    global OctTreeNode *tree = trees;

    my_px = (int2)(get_global_id(0), get_global_id(1));

    // TODO better distribution of rays (currently there is always one more to one side I think)
    float target_v_angle = (-0.5 + my_px.y / (float)height) * vertical_AOV;
    float target_h_angle = (-0.5 + my_px.x / (float)width) * horizontal_AOV;

    // If we want positive angle we need "left" vector
    Matrix3f up_rotation = createRotationMatrix(right, -target_v_angle);
    Matrix3f right_rotation = createRotationMatrix(up, target_h_angle);

    float3 direction = target;
    direction = multiplyVectMatrix(direction, up_rotation);
    direction = multiplyVectMatrix(direction, right_rotation);


    float3 center = (float3)(0,0,0);
    float radius = 1;

    for (;;) {
        relative = origin - center;
        
        while (tree->type == Partial) {
            dx = relative.x > 0;
            dy = relative.y > 0;
            dz = relative.z > 0;
            tree = trees + tree->nodes[dx][dy][dz];
            radius /= 2.f;
            center = (float3)( center.x + (2 * dx - 1) * radius,
                               center.y + (2 * dy - 1) * radius,
                               center.z + (2 * dz - 1) * radius );
            relative = origin - center;
        }

        if (tree->type == Solid) {
            float l_dist = distance(light, origin);
            float intensity = 2.f / (2.f + l_dist * l_dist);
            write_imagef(image, my_px, tree->color * intensity);
            return;
        }

        relative = clamp(relative, -radius, radius);

        xdist = max((radius - relative.x) / direction.x, (-radius - relative.x) / direction.x);
        ydist = max((radius - relative.y) / direction.y, (-radius - relative.y) / direction.y);
        zdist = max((radius - relative.z) / direction.z, (-radius - relative.z) / direction.z);
        dx = 0, dy = 0, dz = 0;
        if (xdist < ydist && xdist < zdist) {
            mindist = xdist;
            dx = direction.x > 0 ? 1 : -1;
        } else if (ydist < zdist) {
            mindist = ydist;
            dy = direction.y > 0 ? 1 : -1;
        } else {
            mindist = zdist;
            dz = direction.z > 0 ? 1 : -1;
        }
        new_relative = relative + direction * mindist;
        origin = new_relative + center;

        while (tree->parent != -1) {
            int nx = tree->x + dx;
            int ny = tree->y + dy;
            int nz = tree->z + dz;
            if (((nx | ny | nz) & (~1)) == 0) {
                tree = trees + trees[tree->parent].nodes[nx][ny][nz];
                // radius stays the same
                center = (float3)( center.x + (2 * dx) * radius,
                                   center.y + (2 * dy) * radius,
                                   center.z + (2 * dz) * radius );
                break;
            }
            center = (float3)( center.x - (2 * tree->x - 1) * radius,
                               center.y - (2 * tree->y - 1) * radius,
                               center.z - (2 * tree->z - 1) * radius );
            radius *= 2.f;
            tree = trees + tree->parent;
        }

        if (tree->parent == -1) {
            write_imagef(image, my_px, (float4)(0, 0, 0, 0));
            return;
        }
    }
}
