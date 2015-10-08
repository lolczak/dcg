package org.lolczak.util

import java.net.URL

object Resources {

  def findUrl(path: String): Option[URL] = Option(Thread.currentThread().getContextClassLoader.getResource(path))

}
