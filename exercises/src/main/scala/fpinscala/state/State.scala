package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r): (Int, RNG) = rng.nextInt
    val posN: Int = if (n < 0) (n + 1) * -1 else n
    (posN, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, r): (Int, RNG) = rng.nextInt
    (n / Int.MaxValue, r)
  }

  def double2: Rand[Double] =
    map(_.nextInt)(_ / Int.MaxValue.toDouble)

  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng: RNG =>
    val (a, rnga): (A, RNG) = ra(rng)
    val (b, rngb): (B, RNG) = rb(rnga)
    (f(a, b), rngb)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng: RNG =>
    def loop(rest: List[Rand[A]], acc: List[A], oldRng: RNG): (List[A], RNG) = rest match {
      case Nil => (acc, oldRng)
      case head :: next =>
        val (a, newRng): (A, RNG) = head(oldRng)
        loop(next, acc :+ a, newRng) }

    loop(fs, Nil, rng)
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng: RNG =>
    val (fa, frng): (A, RNG) = f(rng)
    g(fa)(frng)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def unit[A](a: A): State[S, A] =
    State { s => (a, s) }

  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, newS) = run(s)
    (f(a), newS)
  }

  def mapfp[B](f: A => B): State[S, B] =
    flatMap{ a => unit(f(a)) }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (a, sta) = run(s)
    val (b, stb) = sb.run(sta)
    (f(a, b), stb)
  }

  def map2fp[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, newS) = run(s)
    f(a).run(newS)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { s =>
    val finalState: Machine = inputs.foldLeft(s){ (state, input) =>
      println(s"\ninput: $input \n")
      println(s"\nstate: $state \n")
      if (state.candies == 0) state
      else input match {
        case Coin => if (state.locked) state.copy(locked = false, coins = state.coins + 1) else state
        case Turn => if (state.locked) state else state.copy(candies = state.candies - 1, locked = true)
    }}
    ((finalState.candies, finalState.coins), finalState)
  }
}
