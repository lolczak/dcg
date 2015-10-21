package org.lolczak.dcg.loader

import java.io.File

import org.lolczak.util.Resources

import scala.io.Source

trait ResourceLoader {
  self =>

  def loadResource(path: String): Option[String]

  def +(that: ResourceLoader): ResourceLoader = new ResourceLoader {
    override def loadResource(path: String): Option[String] = self.loadResource(path).orElse(that.loadResource(path))
  }

}

object classpathLoader extends ResourceLoader {

  override def loadResource(path: String): Option[String] =
    for {
      url     <- Resources.findUrl(path)
      content = Source.fromURL(url).mkString
    } yield content

}

class FileSystemLoader(directories: List[String]) extends ResourceLoader {

  private val dirs: List[File] = directories.map(new File(_)).filter(_.isDirectory)

  override def loadResource(path: String): Option[String] = {
    val file = new File(path)
    if (file.isAbsolute)
      Option(Source.fromFile(file).mkString)
    else
      dirs.map(new File(_, path)).find(f => f.exists() && f.isFile).map(Source.fromFile(_).mkString)
  }

}
