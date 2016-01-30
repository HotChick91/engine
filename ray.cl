#define NULL ((void *)0)

#define Empty (-1)
#define Solid (-2)

typedef union {
    struct {
        short type;
        short garbage[7];
        float4 color;
    };
    int nodes[2][2][2];
    struct {
        char padding[2];
        // 0 for root, -1 for its children, and so on
        char levels[3][2];
        int neighbors[3][2];
    };
} __attribute__((packed)) OctTreeNode;

constant float ambient = 0.1f;

// TODO: see if using sign, step, mix, etc. would be a good idea
kernel void ray_cl(float3 origin, float3 light, float3 bottom_left_vec, float3 dup, float3 dright, global OctTreeNode *trees, write_only image2d_t image)
{
    int2 my_px;
    float3 relative;
    float xdist, ydist, zdist;
    // TODO: see if OctTreeNode instead of a pointer makes any difference
    global OctTreeNode *tree = trees;
    global OctTreeNode *last_empty_tree = trees;

    my_px = (int2)(get_global_id(0), get_global_id(1));
    float3 direction = bottom_left_vec + dup * my_px.y + dright * my_px.x;

    float3 center = (float3)(1,1,1);
    float radius = 1.f;
    float3 last_center = center;
    float last_radius = radius;

    origin += center;
    light += center;

    bool chasing_light = 0;
    float4 color = (float4)(0,0,0,0);
    float intensity;
    //printf("sizeof(OctTreeNode)=%d\n", (int)sizeof(OctTreeNode));

    for (;;) {
        relative = origin - center;
        
        while (tree->type >= 0) {
            int dx = relative.x > 0;
            int dy = relative.y > 0;
            int dz = relative.z > 0;
            tree = trees + tree->nodes[dx][dy][dz];
            radius /= 2.f;
            center = (float3)( center.x + (2 * dx - 1) * radius,
                               center.y + (2 * dy - 1) * radius,
                               center.z + (2 * dz - 1) * radius );
            relative = origin - center;
        }

        
        if (!chasing_light && tree->type == Solid) {
            float dist = distance(light, origin);
            intensity = 2.f / (2.f + dist * dist);
            color = tree->color;
            chasing_light = 1;
            direction = light - origin;
            tree = last_empty_tree;
            center = last_center;
            radius = last_radius;
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
        }

        last_empty_tree = tree;
        last_center = center;
        last_radius = radius;
        relative = clamp(relative, -radius, radius);

        xdist = max((radius - relative.x) / direction.x, (-radius - relative.x) / direction.x);
        ydist = max((radius - relative.y) / direction.y, (-radius - relative.y) / direction.y);
        zdist = max((radius - relative.z) / direction.z, (-radius - relative.z) / direction.z);
        if (xdist < ydist && xdist < zdist) {
            int go_positive = direction.x > 0;
            origin = relative + direction * xdist + center;
            center.x += (go_positive * 2 - 1) * radius;
            /*center.y -= radius;
            center.z -= radius;*/
            radius = pown(2.0f, tree->levels[0][go_positive]);
            center.x += (go_positive * 2 - 1) * radius;

            center.y /= 2 * radius;
            center.z /= 2 * radius;
            center.y = floor(center.y);
            center.z = floor(center.z);
            center.y *= 2 * radius;
            center.z *= 2 * radius;
            center.y += radius;
            center.z += radius;

            tree = trees + tree->neighbors[0][go_positive];
        } else if (ydist < zdist) {
            int go_positive = direction.y > 0;
            origin = relative + direction * ydist + center;
            center.y += (go_positive * 2 - 1) * radius;
            /*center.x -= radius;
            center.z -= radius;*/
            radius = pown(2.0f, tree->levels[1][go_positive]);
            center.y += (go_positive * 2 - 1) * radius;

            center.x /= 2 * radius;
            center.z /= 2 * radius;
            center.x = floor(center.x);
            center.z = floor(center.z);
            center.x *= 2 * radius;
            center.z *= 2 * radius;
            center.x += radius;
            center.z += radius;

            tree = trees + tree->neighbors[1][go_positive];
        } else {
            int go_positive = direction.z > 0;
            origin = relative + direction * zdist + center;
            center.z += (go_positive * 2 - 1) * radius;
            /*center.y -= radius;
            center.x -= radius;*/
            radius = pown(2.0f, tree->levels[2][go_positive]);
            center.z += (go_positive * 2 - 1) * radius;

            center.y /= 2 * radius;
            center.x /= 2 * radius;
            center.y = floor(center.y);
            center.x = floor(center.x);
            center.y *= 2 * radius;
            center.x *= 2 * radius;
            center.y += radius;
            center.x += radius;

            tree = trees + tree->neighbors[2][go_positive];
        }

        // this is bit ugly
        if (tree < trees) {
            write_imagef(image, my_px, color * ambient);
            return;
        }
    }
}
