package me.shreyasr

package object evolution {
  implicit class ArrayImprovements[T](val arr: Array[T]) {
    def shuffle: Array[T] = {
      java.util.Collections.shuffle(java.util.Arrays.asList(arr))
      arr
    }
    def swap(i1: Int, i2: Int): Array[T] = {
      val temp = arr(i1)
      arr(i1) = arr(i2)
      arr(i2) = temp
      arr
    }
    def swapRandom(times: Int): Array[T] = {
      (0 until times).foreach(swap(util.Random.nextInt(arr.length), util.Random.nextInt(arr.length)))
      arr
    }
  }
}
