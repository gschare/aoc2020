public class Voxel {

    private boolean currentState;
    private boolean nextState;
    private Voxel[] neighbors = new Voxel[26];

    public Voxel(boolean state) {
        this.currentState = state;
    }

    public void setNeighbors(Voxel[] neighbors) {
        this.neighbors = neighbors;
    }

    public boolean getCurrentState() {
        return currentState;
    }

    public void prepareNextState() {
        int activeNeighbors = 0;

        for (Voxel neighbor: this.neighbors) {
            if (neighbor.getCurrentState()) {
                activeNeighbors++;
            }
        }

        if (this.currentState) {
            if (activeNeighbors == 2 || activeNeighbors == 3) {
                this.nextState = true;
            } else {
                this.nextState = false;
            }
        } else {
            if (activeNeighbors == 3) {
                this.nextState = true;
            } else {
                this.nextState = false;
            }
        }
    }

    public void updateState() {
        this.currentState = this.nextState;
    }

}
