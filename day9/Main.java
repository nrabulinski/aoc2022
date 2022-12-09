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
    
    public double distance(Point other) {
        var x = (double)(this.x - other.x);
        var y = (double)(this.y - other.y);
        return Math.sqrt(x * x + y * y);
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
    private Point head = new Point(0, 0);
    private Point tail = new Point(0, 0);
    private HashSet<Point> visited = new HashSet();
    
    public void tug(Point direction) {
        var oldHead = head;
        head = head.add(direction);
        if (head.distance(tail) >= 2)
            tail = oldHead;
        visited.add(tail);
    }
    
    public int visitedPoints() {
        return visited.size();
    }
}

public class Main {
    private static void solve() throws FileNotFoundException {
        var f = new File("../assets/day9");
        var s = new Scanner(f);
        
        var rope = new Rope();
        
        String line;
        while (s.hasNextLine() && !(line = s.nextLine().trim()).isEmpty()) {
            var l = line.split(" ");
            var dir = Point.fromChar(l[0].charAt(0));
            var times = Integer.parseInt(l[1]);
            for (int i = 0; i < times; i += 1)
                rope.tug(dir);
        }
        System.out.println("Part 1: " + rope.visitedPoints());
    }

    public static void main(String[] args) {
        try {
            solve();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
