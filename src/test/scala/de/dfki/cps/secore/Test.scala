package de.dfki.cps.secore

import java.io.File

import de.dfki.cps.specific.sysml.{Model, Synthesis}
import de.dfki.cps.stools.{STools}
import org.eclipse.emf.ecore.resource.impl.{ResourceImpl, ResourceSetImpl}
import org.scalatest.FunSuite
import scala.collection.JavaConverters._

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class Test extends FunSuite {
  val files = SimSpecGen.getClass.getClassLoader.getResources("de/dfki/cps/secore").asScala
    .map(url => new File(url.getFile))
    .flatMap(_.listFiles().toIterable)
    .filter(_.getName.endsWith(".simeq"))

  val stools = new STools(files.toSeq :_*)

  implicit val lib = new ResourceSetImpl
  Synthesis.prepareLibrary(lib)

  test("blub") {
    val fileA = getClass.getClassLoader.getResource("modelA.sysml")
    val fileB = getClass.getClassLoader.getResource("modelB.sysml")

    val resA = new ResourceImpl()
    val resB = new ResourceImpl()

    val modelA = Model.load(fileA.toURI,"model",resA)
    val modelB = Model.load(fileB.toURI,"model",resB)

    val sresA = new SResource(resA)
    val sresB = new SResource(resB)

    val stool = stools.getSTool("SysML")

    val script = stool.sdiff(sresA,sresB)

    script.entries.foreach(println)

    println("APPLYING SCRIPT")
    applyEditScript(script)

    val script2 = stool.sdiff(sresA,sresB)

    script2.entries.foreach(println)

  }
}
