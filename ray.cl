#define NULL ((void *)0)

#define Empty (-1)
#define Solid (-2)

typedef union {
    struct {
        float4 color;
        short garbage[7];
        short type;
    };
    int nodes[8];
    struct {
        int neighbors[3][2];
        // 0 for root, -1 for its children, and so on
        char levels[3][2];
        char padding[2];
    };
} __attribute__((packed)) OctTreeNode;

constant float ambient = 0.1f;

// specify group size to aid in register allocation
kernel __attribute__((reqd_work_group_size(8, 8, 1)))
void ray_cl(float3 origin, float3 light, float3 bottom_left_vec, float3 dup, float3 dright, global OctTreeNode *trees, write_only image2d_t image)
{
    float3 relative;
    // TODO: see if int instead of a pointer makes any difference
    global OctTreeNode *tree = trees;
    global OctTreeNode *last_empty_tree = trees;

    float3 direction = bottom_left_vec + dup * get_global_id(1) + dright * get_global_id(0);

    float3 center = (float3)(1,1,1);
    float radius = 1.f;
    // TODO: recalculate last_center and last_radius from some simpler thingy
    //       or just add them to OctTreeNode
    float3 last_center = center;
    float last_radius = radius;

    bool chasing_light = 0;
    // TODO: maybe store an octree pointer instead?
    float4 color = (float4)(0,0,0,0);
    float intensity;

    for (;;) {
        relative = origin - center;
        
        while (tree->type >= 0) {
            bool dx = relative.x > 0;
            bool dy = relative.y > 0;
            bool dz = relative.z > 0;
            tree = trees + tree->nodes[dx * 4 + dy * 2 + dz];
            radius *= 0.5f;
            center = (float3)( center.x + (2 * dx - 1) * radius,
                               center.y + (2 * dy - 1) * radius,
                               center.z + (2 * dz - 1) * radius );
            relative = origin - center;
        }
        
        bool solid = tree->type == Solid;
        if (!chasing_light && solid) {
            float3 diff = light - origin;
            diff *= diff;
            float dist2 = diff.x + diff.y + diff.z;
            intensity = native_divide(2.f, 2.f + dist2);
            color = tree->color;
            chasing_light = 1;
            direction = light - origin;
            tree = last_empty_tree;
            center = last_center;
            radius = last_radius;
            relative = origin - center;
        }
        
        float3 relative_light = light - center;
        bool b = all(relative_light == clamp(relative_light, -radius, radius));
        solid = tree->type == Solid;
        if (chasing_light && solid) {
            write_imagef(image, (int2)(get_global_id(0), get_global_id(1)), color * ambient);
            return;
        }
        if (chasing_light && b) {
            write_imagef(image, (int2)(get_global_id(0), get_global_id(1)), color * max(intensity, ambient));
            return;
        }

        last_empty_tree = tree;
        last_center = center;
        last_radius = radius;
        relative = clamp(relative, -radius, radius);

        float3 dist = max(native_divide(radius - relative, direction), native_divide(-radius - relative, direction));
        if (dist.x < dist.y && dist.x < dist.z) {
            bool go_positive = direction.x > 0;
            radius = pown(2.0f, tree->levels[0][go_positive]);
            tree = trees + tree->neighbors[0][go_positive];
            origin += direction * dist.x;
            center.x += (go_positive * 2 - 1) * 2 * radius;
        } else if (dist.y < dist.z) {
            bool go_positive = direction.y > 0;
            radius = pown(2.0f, tree->levels[1][go_positive]);
            tree = trees + tree->neighbors[1][go_positive];
            origin += direction * dist.y;
            center.y += (go_positive * 2 - 1) * 2 * radius;
        } else {
            bool go_positive = direction.z > 0;
            radius = pown(2.0f, tree->levels[2][go_positive]);
            tree = trees + tree->neighbors[2][go_positive];
            origin += direction * dist.z;
            center.z += (go_positive * 2 - 1) * 2 * radius;
        }
        prefetch((global int *)tree, 8);
        center = native_divide(center, 2 * radius);
        center = floor(center);
        center *= 2 * radius;
        center += radius;

        // this is bit ugly
        if (tree < trees) {
            write_imagef(image, (int2)(get_global_id(0), get_global_id(1)), color * ambient);
            return;
        }
    }
}

#define GO_UP do { \
        int nextNode = aux->parent; \
        tree = trees + nextNode; \
        aux = auxes + nextNode; \
    } while (0)

#define GO_DOWN(i) do { \
        int nextNode = tree->nodes[i]; \
        tree = trees + nextNode; \
        aux = auxes + nextNode; \
    } while (0)

#define PARENT (-1)

typedef struct {
    int id;
    int parent;
    int neighbors[3][2];
    int level;
} __attribute__((packed)) CheatSheet;

// TODO: rename id back to index...
// XXX: is writing to auxes and trees a good idea?
//      i think it's fine, as there's no cache coherence?
// precondition: tasks are `Partial` nodes, and their parents' neighbors have to be filled in `auxes`
kernel void find_neighbors(global OctTreeNode *trees, global CheatSheet *auxes, global int *tasks)
{
    int task = tasks[get_global_id(0)];
    global OctTreeNode *tree = trees + task;
    global CheatSheet *aux = auxes + task;
    int prev = PARENT;
    for (int x=0; x<1000000; x++) {
        if (tree->type == Solid) {
            // nothing to do for Solid nodes, just go back up
            prev = aux->id;
            GO_UP;
        } else if (tree->type == Empty) {
            // calculate neighbors, then go back up
            // 0 is x; 1 is y; 2 is z
            for (int i=0; i<3; i++) {
                // sibling
                global OctTreeNode *parentTree = trees + aux->parent;
                int iThBit = ((aux->id << i) >> 2) & 1;
                int flipITh = aux->id ^ (4 >> i);
                tree->neighbors[i][1 - iThBit] = parentTree->nodes[flipITh];
                tree->levels[i][1 - iThBit] = aux->level;
                // outside of parent
                global CheatSheet *parentAux = auxes + aux->parent;
                int uncleId = parentAux->neighbors[i][iThBit];
                if (uncleId == -1) {
                    tree->neighbors[i][iThBit] = -1;
                    tree->levels[i][iThBit] = 0;
                } else {
                    global OctTreeNode *uncle = trees + uncleId;
                    tree->neighbors[i][iThBit] = uncle->type >= 0 ? uncle->nodes[flipITh] : uncleId;
                    tree->levels[i][iThBit] = uncle->type >= 0 ? auxes[uncleId].level - 1 : auxes[uncleId].level;
                }
            }
            prev = aux->id;
            GO_UP;
        } else if (prev == PARENT) {
            // we've descended to a Partial node
            // calculate neighbors, then descend to first child
            for (int i=0; i<3; i++) {
                // sibling
                global OctTreeNode *parentTree = trees + aux->parent;
                int iThBit = ((aux->id << i) >> 2) & 1;
                int flipITh = aux->id ^ (4 >> i);
                aux->neighbors[i][1 - iThBit] = parentTree->nodes[flipITh];
                // outside of parent
                global CheatSheet *parentAux = auxes + aux->parent;
                int uncleId = parentAux->neighbors[i][iThBit];
                if (uncleId == -1) {
                    aux->neighbors[i][iThBit] = -1;
                } else {
                    global OctTreeNode *uncle = trees + uncleId;
                    aux->neighbors[i][iThBit] = uncle->type >= 0 ? uncle->nodes[flipITh] : uncleId;
                }
            }
            GO_DOWN(0);
        } else if (prev < 7) {
            // we've ascended from a child node (but not the last one)
            // no need to recalculate neighbors, just go to the next child
            GO_DOWN(prev + 1);
            prev = PARENT;
        } else if (tree != trees + task) {
            // we've ascended from our last child
            // we can still go up, so do just that
            prev = aux->id;
            GO_UP;
        } else {
            // dfs complete, we're done
            return;
        }
    }
}
