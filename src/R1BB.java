import java.io.FileWriter;
import java.io.IOException;
import java.util.PriorityQueue;

/**
 * User: jlee
 * Date: 11/1/12
 * Time: 11:30 AM
 */
public class R1BB extends CodeJam {

    double[][] availableTime;
    double[][] arrivalTime;
    int[][] ceil;
    int[][] floor;
    int H, N, M;
    PriorityQueue<triple> pq;
    double inf = Double.MAX_VALUE;

    public R1BB(String fileName) throws IOException {
        super(fileName);

    }

    protected void init() {

    }

    class triple implements Comparable<triple>{
        int i;
        int j;
        double time;

        public triple(int i, int j, double time) {
            this.i = i;
            this.j = j;
            this.time = time;
        }

        public int compareTo(triple t) {
            if  (this.time < t.time) {
                return -1;
            } else if (this.time > t.time) {
                return 1;
            } else {
                return 0;
            }
        }

        public boolean equals(Object o) {
            if (o != null && o instanceof triple) {
                triple t = (triple)o;
                return this.i == t.i && this.j == t.j;
            } else {
                return super.equals(o);
            }
        }

        public String toString() {
            return "(" + i + ", " + j + ", " + time + ")";
        }
    }

    protected void solve() {
        try {
            String[] row = br.readLine().split("\\s+");
            H = Integer.parseInt(row[0]);
            N = Integer.parseInt(row[1]);
            M = Integer.parseInt(row[2]);
            ceil = readMatrix(N, M);
            floor = readMatrix(N, M);
            availableTime = new double[N][M];

            for (int i=0; i<availableTime.length; i++) {
                for (int j=0; j<availableTime[i].length; j++) {
                    if (ceil[i][j] < floor[i][j] + 50) {
                        availableTime[i][j] = -1;
                    } else {
                        double time = (double)(H - (ceil[i][j] - 50)) / 10.0;
                        if (time < 0) {
                            availableTime[i][j] = 0;
                        } else {
                            availableTime[i][j] = time;
                        }
                    }
                    //System.out.print(availableTime[i][j] + " ");
                }
                //System.out.println();
            }

            arrivalTime = new double[N][M];


            pq = new PriorityQueue<triple>();
            for (int i=0; i<arrivalTime.length; i++) {
                for (int j=0; j<arrivalTime[i].length; j++) {
                    if (i==0 && j==0) {
                        arrivalTime[i][j] = 0;
                    } else {
                        arrivalTime[i][j] = inf;
                    }
                    pq.add(new triple(i, j, arrivalTime[i][j]));
                }
            }

            while (pq.size() > 0) {

                //System.out.println(pq);
                triple t = pq.remove();
                //System.out.println(t);

                if (t.i == N-1 && t.j == M-1) {
                    printResult("" + t.time);
                    break;
                }

                if (t.time >= inf) {
                    System.out.println("case :" + currentCase + " no answer ");
                    break;
                }

                //east
                if (t.j + 1 < M) {
                    update(t.i, t.j, t.i, t.j + 1, t.time);
                }
                //west
                if (t.j - 1 >= 0) {
                    update(t.i, t.j, t.i, t.j - 1, t.time);
                }
                //south
                if (t.i - 1 >= 0) {
                    update(t.i, t.j, t.i - 1, t.j, t.time);
                }
                //north
                if (t.i +1 < N) {
                    update(t.i, t.j, t.i + 1, t.j, t.time);
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void update(int i1, int j1, int i2, int j2, double time) {
        if (availableTime[i2][j2] >= 0 && ceil[i1][j1] >= floor[i2][j2] + 50 && floor[i1][j1] + 50 <= ceil[i2][j2]) {
            double alt;
            if (availableTime[i2][j2] == 0 && time == 0) {
                alt = 0;
            } else if (availableTime[i2][j2] <= time) {
                if ( H - 10 * time >= floor[i1][j1] + 20) {
                    alt = time + 1;
                } else {
                    alt = time + 10;
                }
            } else {
                if ( H - 10 * availableTime[i2][j2] >= floor[i1][j1] + 20) {
                    alt = availableTime[i2][j2] + 1;
                } else {
                    alt = availableTime[i2][j2] + 10;
                }
            }

            if (alt < arrivalTime[i2][j2]) {
                arrivalTime[i2][j2] = alt;
                pq.remove(new triple(i2, j2, 0));
                pq.add(new triple(i2, j2, alt));
            }
        }
    }

    public static void main(String[] args) {

        String fileName = "B-";

        if (args[0].compareTo("small") == 0) {
            fileName += "small";
        } else if (args[0].compareTo("large") == 0) {
            fileName += "large";
        } else if (args[0].compareTo("sample") == 0) {
            fileName += "sample";
        } else {
            System.out.println("small or large or sample");
            return;
        }

        if (args[1].compareTo("1") == 0) {
            fileName += "-practice.in.txt";
        } else {
            fileName += ".in.txt";
        }

        try {
            R1BB codeJam = new R1BB(fileName);
            codeJam.run();
        } catch (IOException e) {
            System.err.print(e);
        }
    }
}
