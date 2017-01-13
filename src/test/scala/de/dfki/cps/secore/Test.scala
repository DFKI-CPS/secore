package de.dfki.cps.secore

import java.io.File
import java.util

import de.dfki.cps.specific.sysml.{Model, Synthesis}
import de.dfki.cps.stools.{STool, STools}
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.impl.{ResourceImpl, ResourceSetImpl}
import org.scalatest.FunSuite

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class Test extends FunSuite {
  val stools = new STools(new File(getClass.getResource("/ecore.simeq").getFile))

  implicit val lib = new ResourceSetImpl
  Synthesis.prepareLibrary(lib)

  test("blub") {
    val fileA = getClass.getClassLoader.getResource("modelA.sysml")
    val fileB = getClass.getClassLoader.getResource("modelB.sysml")

    val resA = new ResourceImpl()
    val resB = new ResourceImpl()

    val modelA = Model.load(fileA.toURI,resA)
    val modelB = Model.load(fileB.toURI,resB)

    val sresA = new SResource(resA)
    val sresB = new SResource(resB)

    val stool = stools.getSTool("ecore")

    stool.sdiff(sresA,sresB).entries.foreach(println)
  }
}
