import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.lang.Exception;
import java.util.HashSet;

final class Point {
    int x, y;
    
    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }
    
    public static Point fromChar(char c) {
        switch (c) {
            case 'R':
                return new Point(1, 0);
            case 'L':
                return new Point(-1, 0);
            case 'U':
                return new Point(0, -1);
            case 'D':
                return new Point(0, 1);
            default:
                return new Point(0, 0);
        }
    }
    
    public Point reverse() {
        return new Point(-this.x, -this.y);
    }
    
    public Point add(Point other) {
        return new Point(this.x + other.x, this.y + other.y);
    }
    
    public Point sub(Point other) {
        return new Point(this.x - other.x, this.y - other.y);
    }
    
    public double length() {
        return Math.sqrt(x * x + y * y);
    }
    
    public double distance(Point other) {
        return this.sub(other).length();
    }
    
    private static int clamp(int val, int min, int max) {
        return Math.min(Math.max(val, min), max);
    }
    
    public Point clamp(int min, int max) {
        return new Point(
            Point.clamp(this.x, min, max),
            Point.clamp(this.y, min, max)
        );
    }
    
    public boolean equals(Object other) {
        if (this == other)
            return true;
        if (other instanceof Point) {
            var o = (Point)other;
            return this.x == o.x && this.y == o.y;
        }
        return false;
    }
    
    public int hashCode() {
        return 1;
    }
}

final class Rope {
    private Point[] knots;
    private HashSet<Point> visited = new HashSet();
    
    public Rope(int knotCount) {
        this.knots = new Point[knotCount];
        for (int i = 0; i < knotCount; i += 1)
            this.knots[i] = new Point(0, 0);
    }
    
    public void tug(Point direction) {
        knots[0] = knots[0].add(direction);
        for (int i = 1; i < knots.length; i += 1) {
            var dist = knots[i - 1].sub(knots[i]);
            if (dist.length() >= 2)
                knots[i] = knots[i].add(dist.clamp(-1, 1));
        }
        visited.add(knots[knots.length - 1]);
    }
    
    public int visitedPoints() {
        return visited.size();
    }
}

public class Main {
    private static void solve() throws FileNotFoundException {
        var f = new File("../assets/day9");
        var s = new Scanner(f);
        
        var rope1 = new Rope(2);
        var rope2 = new Rope(10);
        
        String line;
        while (s.hasNextLine() && !(line = s.nextLine().trim()).isEmpty()) {
            var l = line.split(" ");
            var dir = Point.fromChar(l[0].charAt(0));
            var times = Integer.parseInt(l[1]);
            for (int i = 0; i < times; i += 1) {
                rope1.tug(dir);
                rope2.tug(dir);
            }
        }
        System.out.println("Part 1: " + rope1.visitedPoints());
        System.out.println("Part 2: " + rope2.visitedPoints());
    }

    public static void main(String[] args) {
        try {
            solve();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
