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

// TODO: see if using sign, step, mix, etc. would be a good idea
kernel void ray_cl(float3 origin, float3 light, float3 bottom_left_vec, float3 dup, float3 dright, global OctTreeNode *trees, write_only image2d_t image, float radius, float3 center, int offset)
{
    int2 my_px;
    float3 relative;
    // TODO: see if OctTreeNode instead of a pointer makes any difference
    global OctTreeNode *tree = trees + offset;

    my_px = (int2)(get_global_id(0), get_global_id(1));
    float3 direction = bottom_left_vec + dup * my_px.y + dright * my_px.x;
    float3 dist;
    int3 d;

    for (;;) {
        relative = origin - center;
        
        while (tree->type == Partial) {
            d = -(relative > 0);
            tree = trees + tree->nodes[d.x][d.y][d.z];
            radius /= 2.f;
            center += convert_float3(2 * d - 1) * radius;
            relative = origin - center;
        }

        if (tree->type == Solid) {
            float l_dist = distance(light, origin);
            float intensity = 2.f / (2.f + l_dist * l_dist);
            write_imagef(image, my_px, tree->color * intensity);
            return;
        }

        relative = clamp(relative, -radius, radius);

        dist = max((radius - relative) / direction, (-radius - relative) / direction);
        d = 0;
        if (dist.x < dist.y && dist.x < dist.z) {
            dist.z = dist.x;
            d.x = direction.x > 0 ? 1 : -1;
        } else if (dist.y < dist.z) {
            dist.z = dist.y;
            d.y = direction.y > 0 ? 1 : -1;
        } else {
            d.z = direction.z > 0 ? 1 : -1;
        }
        relative += direction * dist.z;
        origin = relative + center;

        while (tree->parent != -1) {
            int nx = tree->x + d.x;
            int ny = tree->y + d.y;
            int nz = tree->z + d.z;
            if (((nx | ny | nz) & (~1)) == 0) {
                tree = trees + trees[tree->parent].nodes[nx][ny][nz];
                // radius stays the same
                center += convert_float3(2 * d) * radius;
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
