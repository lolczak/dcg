package org.lolczak.dcg.model

import scala.util.Try

sealed trait FeatureCrumb
case class NameCrumb(name: String, parent: FeatureStruct) extends FeatureCrumb
case class IndexCrumb(index: Int, parent: FList) extends FeatureCrumb

case class FeatureZipper(item: FeatureItem, breadcrumbs: List[FeatureCrumb])

object FeatureZipper {

  def zipStructs = ???

//  def fold[A](f: (A, FeatureItem) => A)(first: A)(item: FeatureItem): A = {
//    val firstBis = f(first, item)
//    item match {
//      case FeatureStruct(elems) => elems.values.foldLeft[A](firstBis)(f)
//      case FList(elems)         => elems.foldLeft[A](firstBis)(f)
//      case _: FConst            => firstBis
//      case FPlaceholder         => firstBis
//      case _: FVariable         => firstBis
//    }
//  }

//  def traverseWithNavigation()

  def fold[A](f: (A, FeatureZipper) => A)(first: A)(root: FeatureZipper): A = {
    val firstBis = f(first, root)
    root.item match {
      case s@FeatureStruct(elems) =>
        val zippers = elems.toList.map { case (name, value) => FeatureZipper(value, NameCrumb(name, s) :: root.breadcrumbs) }
        zippers.foldLeft(firstBis) { case (acc, zipper) => fold(f)(acc)(zipper) }
      case l@FList(elems)         => elems.zipWithIndex.map { case (item, index) => FeatureZipper(item, IndexCrumb(index, l) :: root.breadcrumbs) }.foldLeft[A](firstBis)(f) //todo enable nesting in list
      case _                      => firstBis
    }
  }

  def find(f: FeatureItem => Boolean)(root: FeatureZipper): List[FeatureZipper] = {
    fold[List[FeatureZipper]] {
      case (acc, z@FeatureZipper(item, path)) => if (f(item)) z :: acc else acc
    } (List.empty[FeatureZipper]) (root)
  }

  def filter(f: FeatureItem => Boolean)(root: FeatureItem): List[FeatureZipper] = find(f)(FeatureZipper(root, List.empty))

  def goto(breadcrumbs: List[FeatureCrumb])(root: FeatureItem): Option[FeatureItem] = {
    breadcrumbs.reverse.foldLeft[Option[FeatureItem]](Some(root)) {
      case (Some(f:FeatureStruct), NameCrumb(name, _)) => f(name)
      case (Some(FList(elems)), IndexCrumb(index, _))  => Try(elems(index)).toOption
      case _                                           => None
    }
  }

}