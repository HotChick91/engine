#define NULL ((void *)0)

enum OctTreeNodeType { Empty, Solid, Partial };

typedef struct OctTreeNode {
	int x, y, z;
	int parent;
	float3 center; // center point of cube
	float radius; // cube radius
	enum OctTreeNodeType type;
	union {
		float4 color;
		int nodes[2][2][2];
	};
} OctTreeNode;

kernel void ray_cl(float3 origin, float3 bottom_left_vec, float3 dup, float3 dright, global OctTreeNode *trees, write_only image2d_t image)
{
	int2 my_px;
	float3 relative, new_relative;
	float xdist, ydist, zdist, mindist;
	int dx, dy, dz;
	// TODO: see if OctTreeNode instead of a pointer makes any difference
	global OctTreeNode *tree = trees;

	my_px = (get_global_id(0), get_global_id(1));
	float3 direction = bottom_left_vec + dup * my_px.y + dright * my_px.x;

	for (;;) {
		relative = origin - tree->center;

		while (tree->type == Partial) {
			tree = trees + tree->nodes[relative.x > 0][relative.y > 0][relative.z > 0];
			relative = origin - tree->center;
		}

		if (tree->type == Solid) {
			write_imagef(image, my_px, tree->color);
			return;
		}

		relative = clamp(relative, -tree->radius, tree->radius);

		xdist = max((tree->radius - relative.x) / direction.x, (-tree->radius - relative.x) / direction.x);
		ydist = max((tree->radius - relative.y) / direction.y, (-tree->radius - relative.y) / direction.y);
		zdist = max((tree->radius - relative.z) / direction.z, (-tree->radius - relative.z) / direction.z);
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
				origin = new_relative + tree->center;
				tree = trees + trees[tree->parent].nodes[nx][ny][nz];
				break;
			}
			tree = trees + tree->parent;
		}

		if (tree->parent == -1) {
			write_imagef(image, my_px, (float4)(1, 1, 1, 0));
			return;
		}
	}
}
