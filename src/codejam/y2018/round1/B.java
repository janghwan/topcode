
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.Scanner;

public class B {
    public static void main(String[] args) {
        Scanner in = new Scanner(new BufferedReader(new InputStreamReader(System.in)));
        int t = in.nextInt();  // Scanner has functions to read ints, longs, strings, chars, etc.
        for (int T = 1; T <= t; ++T) {
            int R = in.nextInt();
            int B = in.nextInt();
            int C = in.nextInt();
            int[] M = new int[C];
            int[] S = new int[C];
            int[] P = new int[C];

            for (int i = 0; i < C; i ++) {
                M[i] = in.nextInt();
                S[i] = in.nextInt();
                P[i] = in.nextInt();
            }

            long time = 5000;

            while(!check(time, M, S, P, R, B, C)) {
                time = time * 2;
            }
            long a = 0;
            long b = time;

            while(true) {
                if (check((a + b) / 2, M, S, P, R, B, C)) {
                    b = (a + b) / 2;
                    if (b <= a + 1) {
                        System.out.println("Case #" + T + ": " + b);

                        break;
                    }
                } else {
                    a = (a + b) / 2;
                    if (a >= b - 1) {
                        System.out.println("Case #" + T + ": " + b);
                        break;
                    }
                }
            }

        }
    }

    private static boolean check(long time, int[] M, int[] S, int[] P, int R, int B, int C) {
        Long[] max = new Long[C];
        for (int i=0; i<C; i++ ) {
            max[i] = (time - P[i]) / S[i];
            if (max[i] > M[i])
                max[i] = (long)M[i];
        }

        Arrays.sort(max, Collections.reverseOrder());

        long sum = 0;
        for (int i=0; i<R; i++) {
            sum += max[i];
        }

        return sum >= B;
    }

}