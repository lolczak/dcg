package org.lolczak.util

import scala.annotation.tailrec

object Generators {

  //todo refactor, generic version
  def generate[A](f: A => Set[A], source: Set[A]): Set[A] = {
    @tailrec
    def recGen(oldSet: Set[A], newSet: Set[A]): Set[A] = {
      val generated = newSet.flatMap(f)
      if (generated.isEmpty) oldSet
      else recGen(generated ++ oldSet, generated -- oldSet)
    }
    recGen(source, source)
  }

}
