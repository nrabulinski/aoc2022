import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Test(div: Long, t: Int, f: Int)

class Monkey(
	modBy: Long,
	items: ArrayBuffer[Long],
	op: Long => Long,
	test: Test,
	var divBy: Long = 3,
	var inspected: Long = 0,
):
	def this(
		modBy: Long,
		startingItems: Array[Long],
		op: Long => Long,
		test: Test,
	) = 
		this(
			modBy,
			ArrayBuffer.from(startingItems),
			op,
			test,
		)
	
	def dup(div: Long) =
		Monkey(
			modBy,
			items.clone(),
			op,
			test,
			div,
			0,
		)

	def throwItems() =
		val res = items
			.map { (item) =>
				val Test(div, t, f) = test
				val lvl = op(item) % modBy
				inspected += 1L
				val newLvl = lvl / divBy
				if newLvl % div == 0 then
					(newLvl, t)
				else
					(newLvl, f)
			}
			.toArray
		items.clear
		res
	
	def catchItem(v: Long) =
		items.addOne(v)
end Monkey

sealed trait Arg
case class ConstArg(v: Long) extends Arg
case class OldArg() extends Arg

def parseArg(src: String): Arg =
	src match
		case "old" => OldArg()
		case _ => ConstArg(src.toLong)

def argVal(arg: Arg, old: Long): Long =
	arg match
		case ConstArg(v) => v
		case OldArg() => old

def parseOp(src: String): Long => Long =
	val s = src
		.split(" = ")(1)
		.split(" ")
	val arg1 = parseArg(s(0))
	val op = s(1)
	val arg2 = parseArg(s(2))
	{(old: Long) =>
		val a = argVal(arg1, old)
		val f: Long => Long = op match
			case "+" => a.+
			case "*" => a.*
			case _ => (_) => 0
		f(argVal(arg2, old))
	}

def parseMonkey(modBy: Long, src: Seq[String]): Monkey =
	val items = src(1)
		.split(": ")(1)
		.split(", ")
		.map(_.toLong)
	val div = src(3)
		.split(": ")(1)
		.split(" ")(2)
		.toLong
	val t = src(4)(src(4).length - 1).toInt - 48
	val f = src(5)(src(5).length - 1).toInt - 48
	Monkey(
		modBy,
		items,
		parseOp(src(2)),
		Test(div, t, f)
	)

def gcd(a: Long, b: Long): Long =
	if b == 0 then
		a
	else
		gcd(b, a % b)

def lcm(a: Long, b: Long): Long = a / gcd(a, b) * b

@main
def main() =
	val lines = Source
		.fromFile("assets/day11")
		.getLines
		.grouped(7)
		.toList
	// Collect all divisors so we can reduce the worry level without affecting the tests
	val modBy = lines
		.map(
			_(3)
				.split(": ")(1)
				.split(" ")(2)
				.toLong
		)
		.foldLeft(1L)(lcm)
	var monkeys = lines
		.map(parseMonkey(modBy, _))
		.toArray
	var newMonkeys = monkeys.map(_.dup(1))
	for _ <- 1 to 20 do
		for idx <- 0 to (monkeys.length - 1) do
			for (lvl, i) <- monkeys(idx).throwItems() do
				monkeys(i).catchItem(lvl)
	println(
		monkeys
			.map(_.inspected)
			.sortWith(Ordering[Long].gt)
			.take(2)
			.foldLeft(1L)(_ * _)
	)
	for _ <- 1 to 10000 do
		for idx <- 0 to (newMonkeys.length - 1) do
			for (lvl, i) <- newMonkeys(idx).throwItems() do
				newMonkeys(i).catchItem(lvl)
	println(
		newMonkeys
			.map(_.inspected)
			.sortWith(Ordering[Long].gt)
			.take(2)
			.foldLeft(1L)(_ * _)
	)
