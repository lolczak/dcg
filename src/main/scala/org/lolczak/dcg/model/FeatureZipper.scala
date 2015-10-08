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

  def zip(breadcrumbs: List[FeatureCrumb])(root: FeatureItem): Option[FeatureZipper] = {
    breadcrumbs.reverse.foldLeft[Option[FeatureZipper]](Some(FeatureZipper(root, List.empty))) {
      case (Some(FeatureZipper(f:FeatureStruct, crumbs)), NameCrumb(name, _))  => f(name) map (FeatureZipper(_, NameCrumb(name, f) :: crumbs))
      case (Some(FeatureZipper(l@FList(elems), crumbs)), IndexCrumb(index, _)) => Try(elems(index)).toOption map  (FeatureZipper(_, IndexCrumb(index, l) :: crumbs))
      case _                                           => None
    }
  }

  def topMost(root: FeatureZipper): FeatureZipper = root match {
    case root@FeatureZipper(item, List()) => root
    case node => topMost(goUp(node))
  }

  def goUp(root: FeatureZipper): FeatureZipper = root match {
    case FeatureZipper(_, List())                               => root //or throw exception
    case FeatureZipper(value, NameCrumb(name, parent) :: tail)  => FeatureZipper(FeatureStruct(parent.features.updated(name, value)), tail)
    case FeatureZipper(item, IndexCrumb(index, parent) :: tail) => FeatureZipper(FList(parent.elements.patch(index, Seq(item), 1)), tail)
  }

  def modify(f: FeatureItem => FeatureItem)(root: FeatureZipper): FeatureZipper = root.copy(item = f(root.item))

  def alter(f: FeatureItem => FeatureItem)(breadcrumbs: List[FeatureCrumb])(root: FeatureItem): FeatureItem = {
    val zipper = zip(breadcrumbs)(root).getOrElse(throw new IllegalArgumentException(s"Cannot follow path $breadcrumbs"))
    topMost(modify(f)(zipper)).item
  }

}