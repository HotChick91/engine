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

kernel void ray_cl(float3 origin, float3 bottom_left_vec, float3 dup, float3 dright, global OctTreeNode *trees, write_only image2d_t image)
{
    int2 my_px;
    float3 relative, new_relative;
    float xdist, ydist, zdist, mindist;
    int dx, dy, dz;
    // TODO: see if OctTreeNode instead of a pointer makes any difference
    global OctTreeNode *tree = trees;

    my_px = (int2)(get_global_id(0), get_global_id(1));
    float3 direction = bottom_left_vec + dup * my_px.y + dright * my_px.x;

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
            write_imagef(image, my_px, tree->color);
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

        while (tree->parent != -1) {
            int nx = tree->x + dx;
            int ny = tree->y + dy;
            int nz = tree->z + dz;
            if (((nx | ny | nz) & (~1)) == 0) {
                origin = new_relative + center;
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
