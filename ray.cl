#define NULL ((void *)0)

#define Empty 0
#define Solid 1
#define Partial 2

typedef struct {
    char x, y, z;
    char type;
    int parent;
    union {
        float4 color;
        int nodes[2][2][2];
    };
} __attribute__((packed)) OctTreeNode;

constant float ambient = 0.1f;

// TODO: see if using sign, step, mix, etc. would be a good idea
kernel void ray_cl(float3 origin, float3 light, float3 bottom_left_vec, float3 dup, float3 dright, global OctTreeNode *trees, write_only image2d_t image)
{
    int2 my_px;
    float3 relative, new_relative;
    float xdist, ydist, zdist, mindist;
    int dx, dy, dz;
    // TODO: see if OctTreeNode instead of a pointer makes any difference
    global OctTreeNode *tree = trees;
    global OctTreeNode *last_empty_tree = trees;

    my_px = (int2)(get_global_id(0), get_global_id(1));
    float3 direction = bottom_left_vec + dup * my_px.y + dright * my_px.x;

    float3 center = (float3)(0,0,0);
    float radius = 1;

    bool chasing_light = 0;
    float4 color = (float4)(0,0,0,0);
    float intensity;

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

        if (chasing_light) {
            float3 relative_light = light - center;
            if (all(relative_light == clamp(relative_light, -radius, radius))) {
                write_imagef(image, my_px, color * max(intensity, ambient));
                return;
            } else if (tree->type == Solid) {
                write_imagef(image, my_px, color * ambient);
                return;
            }
        } else if (tree->type == Solid) {
            float dist = distance(light, origin);
            intensity = 2.f / (2.f + dist * dist);
            color = tree->color;
            chasing_light = 1;
            direction = light - origin;
            tree = last_empty_tree;
        }

        last_empty_tree = tree;
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
            write_imagef(image, my_px, color * ambient);
            return;
        }
    }
}
