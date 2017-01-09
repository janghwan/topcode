package lib;

import java.io.*;

/**
 * User: jlee
 * Date: 10/29/12
 * Time: 9:08 PM
 */
public abstract class CodeJam {

    protected BufferedReader br;
    protected int totalCases;
    protected int currentCase;
    BufferedWriter o;

    public CodeJam() {
        this.br = null;
        totalCases = 10;
        currentCase = 0;
    }



    public CodeJam(String filename) throws IOException {
        br = new BufferedReader(new FileReader(filename));
        String line = br.readLine();
        totalCases = Integer.parseInt(line);
        o = new BufferedWriter(new FileWriter("output.txt"));
    }

    protected int[][] readMatrix(int row, int col) throws IOException {
        int[][] matrix = new int[row][col];
        for (int i=0; i < row; i++) {
            String[] temp = br.readLine().split("\\s+");


            for (int j=0; j < col; j++) {
                matrix[i][j] = Integer.parseInt(temp[j]);
            }
        }

        return matrix;
    }

    protected char[][] readCharMatrix(int row, int col) throws IOException {
        char[][] matrix = new char[row][col];

        for (int i=0; i < row; i++) {
            char[] temp = br.readLine().toCharArray();
            System.arraycopy(temp, 0, matrix[i], 0, matrix[i].length);
        }
        br.readLine();

        return matrix;
    }

    abstract protected void solve();

    abstract protected void init();

    public void printResult(String answer) {
        try {
            o.write("Case #" + (currentCase) + ": " + answer + "\n");
            System.out.print("Case #" + (currentCase) + ": " + answer + "\n");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void run() {
        init();
        for (int i=0; i < totalCases; i++) {
            currentCase = i+1;
            solve();
        }
        try {
            o.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


}
