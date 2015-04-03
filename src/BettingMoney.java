/**
 *
 */
public class BettingMoney {
    int moneyMade(int[] amounts, int[] centsPerDollar, int finalResult) {
        int sum = 0;

        for (int i = 0; i < amounts.length; i++) {
            if (i == finalResult) {
                sum -= (centsPerDollar[i]) * amounts[i];
            } else {
                sum += amounts[i] * 100;
            }
        }

        return sum;
    }

    public static void main(String[] args) {
        BettingMoney b = new BettingMoney();
        int[] amounts = {100};
        int[] centsPerDollar = {10};
        System.out.println(b.moneyMade(amounts, centsPerDollar, 0));
    }
}
