import java.util.*;
import java.io.*;

/**
 * NOT CORRECT
 */
public class A {
    public static void main(String[] args) {
        Scanner in = new Scanner(new BufferedReader(new InputStreamReader(System.in)));
        int t = in.nextInt();  // Scanner has functions to read ints, longs, strings, chars, etc.
        for (int T = 1; T <= t; ++T) {
            int R = in.nextInt();
            int C = in.nextInt();
            int H = in.nextInt();
            int V = in.nextInt();
            int total = 0;
            int[][] matrix = new int[R][C];
            for (int i = 0; i < R; i ++) {
                String line = in.nextLine();
                for (int j = 0; j < C; j++) {
                    if (line.charAt(j) == '@') {
                        matrix[i][j] = 1;
                        total++;
                    }
                }
            }


            int share = total / ((H + 1) * (V + 1));
            int vshare = share * (H + 1);

            int[] vs = new int[C];
            int[] hs = new int[R];

            int[][] sumM = new int[R][C];

            for (int i = 0; i < R; i ++) {
                for (int j = 0; j < C; j++) {
                    if (i==0) {
                        if (j==0) {
                            sumM[i][j] = matrix[i][j];
                        } else {
                            sumM[i][j] = sumM[i][j-1] + matrix[i][j];
                        }
                    } else {
                        sumM[i][j] = sumM[i][j-1] + sumM[i-1][j] - sumM[i-1][j-1] + matrix[i][j];
                    }
                }
            }

            int lasti = 0;
            int i = 0;
            for (int h = 1; h <= H; h++) {
                int j = 0;
                int lastj = 0;
                for (int v = 1; v <= V; v++) {
                    while (sumM[R-1][j] < v * vshare) {
                        j++;
                    }
                    while (sumM[R-1][j] == v * vshare) {
                        while (sumM[i][j] - sumM[lasti][j] - sumM[i][lastj] + matrix[lasti][lastj] < share) {
                            i++;
                        }
                        while (sumM[i][j] - sumM[lasti][j] - sumM[i][lastj] + matrix[lasti][lastj] == share) {
                            vs[j] = v;
                            i++;
                        }
                    }
                }


            }

            boolean ans = solve(sumM, 0,0, R, C, 1, 1);
            System.out.println("Case #" + T + ": " + (ans ? "POSSIBLE" : "IMPOSSIBLE"));
        }
    }

    public static boolean solve(int[][] sumM, int rs, int cs, int re, int ce, int h, int v) {
        if (h == 1) {
            if (v == 1) {
                for (int i = rs; i < re; i++) {
                    for (int j = cs; j < ce; j++) {
                        int UL = sumM[i][j];
                        int UR = sumM[i][ce-1] - sumM[i][j];
                    }
                }
            }
        }
        return false;
    }
}