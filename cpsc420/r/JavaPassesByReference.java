
import java.util.ArrayList;

public class JavaPassesByReference {
    public static void main(String args[]) {
        ArrayList<Integer> fred = new ArrayList<Integer>();
        fred.add(5);
        fred.add(7);
        fred.add(9);
        fred.add(3);

        System.out.println("fred has " + fred.size() + " elements.");
        myfunc(fred);
        System.out.println("fred has " + fred.size() + " elements.");
    }

    static void myfunc(ArrayList<Integer> avec) {
        avec.remove(2);
    }
}
