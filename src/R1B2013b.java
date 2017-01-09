import lib.CodeJam;

import java.io.IOException;
import java.math.BigInteger;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * User: jlee
 * Date: 11/1/12
 * Time: 11:30 AM
 */
public class R1B2013b extends CodeJam {


    double inf = Double.MAX_VALUE;
    LinkedList<LinkedList<String>> list = new LinkedList<LinkedList<String>>();

    LinkedList<String> flatList = new LinkedList<String>();

    public R1B2013b(String fileName) throws IOException {
        super(fileName);

    }

    protected void init() {

        LinkedList<String> temp = new LinkedList<String>();
        temp.add(0, "0");
        temp.add(1, "1");
        temp.add(2, "2");
        temp.add(3, "3");
        list.add(0, temp);
        temp = new LinkedList<String>();
        temp.add(0, "1");
        temp.add(1, "2");
        list.add(1, temp);

        for (int i = 2; i <= 51; i++) {
            int index;
            if ((i + 1) % 2 == 0) {

                LinkedList<String> sub = list.get(i - 1);

                Iterator<String> iter1 = sub.listIterator();
                temp = new LinkedList<String>();

                int count = 0;

                while(iter1.hasNext()) {
                    String prefix = iter1.next();
                    if (squareSum(prefix) * 2 < 10) {
                        temp.add(count++, prefix);
                    } else {
                    }
                }

                list.add(i, temp);
            } else {

                LinkedList<String> sub1 = list.get(i - 1);
                LinkedList<String> sub2 = list.get(0);

                Iterator<String> iter1 = sub1.listIterator();

                temp = new LinkedList<String>();

                int count = 0;

                while(iter1.hasNext()) {
                    String prefix = iter1.next();
                    Iterator<String> iter2 = sub2.listIterator();
                    while(iter2.hasNext()) {
                        String postfix = iter2.next();
                        if (squareSum(prefix) * 2 + squareSum(postfix) < 10) {
                            temp.add(count++, prefix + postfix);
                        } else {
                        }
                    }
                }

                list.add(i, temp);
            }
        }

        int count = 0;
        for (LinkedList<String> item : list) {
            if (count == 0) {
                for (String str : item) {
                    if (!str.equals("0")) {
                        flatList.add(str);
                        //printResult(str);
                    }


                }
                count++;
                continue;
            }

            if (count==1) {
                for (String str : item) {
                    flatList.add(str + str);
                    //printResult(str + str);
                }
                count++;
                continue;
            }

            for (String str : item) {
                String reverse = new StringBuilder(str).reverse().toString();


                if (count % 2 == 1) {
                    str = str + reverse;
                } else {
                    String pivot = str.substring(str.length()-1, str.length());
                    str = str.substring(0, str.length() - 1) + pivot + reverse.substring(1, reverse.length());
                }

                flatList.add(str);
                //printResult(str);
                //System.out.println(str);
            }
            count++;

        }

    }

    protected int squareSum(String in) {
        int sum = 0;

        for (int i = 0; i < in.length(); i++) {
            int digit = (Integer.parseInt(in.charAt(i) + ""));
            sum += digit * digit;
        }

        return sum;
    }


    protected void solve() {
        try {
            String[] row = br.readLine().split("\\s+");

            BigInteger start = new BigInteger(row[0]);
            BigInteger end = new BigInteger(row[1]);

            int count = 0;

            for (String str : flatList) {


                BigInteger fair = new BigInteger(str);
                BigInteger fairSquare = fair.multiply(fair);
                //System.out.println(str + " " + fairSquare);

                if (fairSquare.compareTo(start) >= 0) {
                    if (fairSquare.compareTo(end) <= 0) {
                        count++;

                    } else {
                        break;
                    }
                }
            }


            printResult(count + "");

        } catch (Exception e) {
            e.printStackTrace();
        }


    }

    protected boolean isPalindrome(long in) {
        String number = in + "";

        for (int i = 0; i < number.length() / 2; i++ ) {
            if (number.charAt(i) == number.charAt(number.length() - i - 1)) {
            } else {
                return false;
            }
        }

        return true;
    }

    public static void main(String[] args) {

        String fileName = "A-";

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
            R1B2013b codeJam = new R1B2013b(fileName);
            codeJam.run();
        } catch (IOException e) {
            System.err.print(e);
        }
    }
}
