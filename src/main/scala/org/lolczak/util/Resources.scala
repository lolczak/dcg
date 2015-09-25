package org.lolczak.util

import java.net.URL

object Resources {

  def load(path: String): Option[String] = {
    ???
  }

  def findUrl(path: String): Option[URL] = {
    Option(Thread.currentThread().getContextClassLoader.getResource(path))

  }

}
