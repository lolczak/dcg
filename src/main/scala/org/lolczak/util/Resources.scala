package org.lolczak.util

import java.net.URL

import scala.io.Source

object Resources {

  def findUrl(path: String): Option[URL] = Option(Thread.currentThread().getContextClassLoader.getResource(path))

  def load(path: String): Option[String] = findUrl(path) map (Source.fromURL(_).mkString)

}
