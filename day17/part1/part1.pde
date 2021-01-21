import peasy.*;

PeasyCam cam;

int t = 0;
int n = 0;

static final int SIZE = 21;
static final int VOXEL_SIZE = 40;
static final boolean[][] initial = {
    {false, false,  true, false, false,  true, false,  true},
    { true,  true, false,  true, false, false,  true, false},
    { true, false, false, false, false,  true, false, false},
    {false,  true, false, false,  true,  true,  true,  true},
    {false, false, false, false, false,  true, false, false},
    {false, false, false,  true,  true, false, false, false},
    {false,  true, false,  true,  true, false, false,  true},
    {false,  true, false,  true, false,  true, false,  true}
};

Voxel[][][][] space;

Voxel[] getNeighbors(Voxel[][][][] space, int m, int i, int j, int k) {
    // No need to check if we're at the edge of the space, since the space must have a shell of permanently dead voxels around it.
    int[] offs = {-1, 0, 1};
    Voxel[] neighbors = new Voxel[80];

    int n = 0;
    for (int woff: offs) {
        for (int xoff: offs) {
            for (int yoff: offs) {
                for (int zoff: offs) {
                    if (xoff == 0 && yoff == 0 && zoff == 0 && woff == 0) {
                        continue;
                    }
                    neighbors[n++] = space[m+woff][i+xoff][j+yoff][k+zoff];
                }
            }
        }
    }
    return neighbors;
}

void setup() {
    //frameRate(15);
    size(1600, 900, P3D);
    cam = new PeasyCam(this, 400);
    cam.lookAt((double) SIZE * VOXEL_SIZE / 2, (double) SIZE * VOXEL_SIZE / 2, (double) SIZE * VOXEL_SIZE / 2);

    background(0);
    colorMode(HSB, SIZE * 3, 100, 100);

    // Generate the space full of voxels.
    space = new Voxel[SIZE][SIZE][SIZE][SIZE];


    for (int m=0; m<SIZE; m++) {
        for (int i=0; i<SIZE; i++) {
            for (int j=0; j<SIZE; j++) {
                for (int k=0; k<SIZE; k++) {
                    if (i==10 && m==10 && j>=6 && j<14 && k>=6 && k<14) {
                        space[m][i][j][k] = new Voxel(initial[j-6][k-6]);
                    } else {
                        space[m][i][j][k] = new Voxel(false);
                    }
                }
            }
        }
    }

    // Assign neighbors.
    for (int m=1; m<SIZE-1; m++) {
        for (int i=1; i<SIZE-1; i++) {
            for (int j=1; j<SIZE-1; j++) {
                for (int k=1; k<SIZE-1; k++) {
                    space[m][i][j][k].setNeighbors(getNeighbors(space, m, i, j, k));
                }
            }
        }
    }
}

void draw() {
    background(0);

    // Draw all voxels and prepare their next state.
    for (int m=1; m<SIZE-1; m++) {
        for (int i=1; i<SIZE-1; i++) {
            for (int j=1; j<SIZE-1; j++) {
                for (int k=1; k<SIZE-1; k++) {
                    space[m][i][j][k].prepareNextState();
                    if (false){
                    if (space[m][i][j][k].getCurrentState()) {
                        pushMatrix();
                        translate(i*VOXEL_SIZE,j*VOXEL_SIZE,k*VOXEL_SIZE);
                        noFill();
                        stroke(100);
                        fill((i + j + k - 1) * 1.2, 75, 75);
                        box(VOXEL_SIZE*0.7);
                        popMatrix();
                    }
                    }
                }
            }
        }
    }

    //if (t % frameRate == 0) {

    // Update all voxels.
    int totalActive = 0;

    for (int i=1; i<SIZE-1; i++) {
        for (int j=1; j<SIZE-1; j++) {
            for (int k=1; k<SIZE-1; k++) {
                for (int m=1; m<SIZE-1; m++) {
                    space[i][j][k][m].updateState();
                    if (space[i][j][k][m].getCurrentState()) {
                        totalActive++;
                    }
                }
            }
        }
    }

    if (n < 6) {
        println(n, totalActive);
    }
    n++;

    //}

    t++;
}
