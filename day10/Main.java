import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.lang.Exception;
import java.util.HashSet;

final class Crt {
    private int cycleCount;
    
    public Crt() {
        cycleCount = 0;
    }
    
    public void tick(int reg) {
        var x = cycleCount % 40;
        if (x == 0)
            System.out.println();
        System.out.print(Math.abs(x - reg) < 2 ? '#' : '.');
        cycleCount += 1;
    }
}

final class Cpu {
    private int cycleCount;
    private int reg;
    private int signalStrengthsSum;
    private Crt display;
    
    public Cpu() {
        cycleCount = 0;
        reg = 1;
        signalStrengthsSum = 0;
        display = new Crt();
    }
    
    private void tick(int diff) {
        var count = cycleCount - 20;
        for (int i = 0; i < diff; i += 1) {
            display.tick(reg);
            count += 1;
            if (count % 40 == 0) {
                signalStrengthsSum += (count + 20) * reg;
            }
        }
        cycleCount = count + 20;
    }
    
    private void noop() {
        this.tick(1);
    }
    
    private void addx(int v) {
        this.tick(2);
        reg += v;
    }
    
    public void exec(String line) {
        var l = line.split(" ");
        var ins = l[0];
        switch (ins) {
            case "noop":
                this.noop();
                break;
            case "addx":
                var arg = Integer.parseInt(l[1]);
                this.addx(arg);
                break;
        }
    }
    
    public int getResult() {
        return signalStrengthsSum;
    }
}

public class Main {
    private static void solve() throws FileNotFoundException {
        var f = new File("../assets/day10");
        var s = new Scanner(f);
        
        System.out.println("Part 2:");
        var cpu = new Cpu();
        
        String line;
        while (s.hasNextLine() && !(line = s.nextLine().trim()).isEmpty())
            cpu.exec(line);

        System.out.println();
        System.out.println("Part 1: " + cpu.getResult());
    }

    public static void main(String[] args) {
        try {
            solve();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
