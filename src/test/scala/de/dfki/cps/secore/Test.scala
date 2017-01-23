package de.dfki.cps.secore

import de.dfki.cps.specific.sysml.{Model, Synthesis}
import org.eclipse.emf.ecore.resource.impl.{ResourceImpl, ResourceSetImpl}
import org.scalatest.FunSuite

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class Test extends FunSuite {
  val stools = de.dfki.cps.secore.stools

  implicit val lib = new ResourceSetImpl
  Synthesis.prepareLibrary(lib)

  test("blub") {
    val fileA = getClass.getClassLoader.getResource("modelA.sysml")
    val fileB = getClass.getClassLoader.getResource("modelB.sysml")

    val resA = new ResourceImpl()
    val resB = new ResourceImpl()

    val modelA = Model.load(fileA.toURI,"model",resA)
    val modelB = Model.load(fileB.toURI,"model",resB)

    val stool = stools.getSTool("uml")

    val script = {
      val sresA = new SResource(resA)
      val sresB = new SResource(resB)
      stool.sdiff(sresA,sresB)
    }

    script.entries.foreach(println)

    println("APPLYING SCRIPT")
    applyEditScript(script)

    val resB2 = new ResourceImpl()
    resB2.getContents.addAll(resB.getContents)

    val script2 = {
      val sresA = new SResource(resA)
      val sresB = new SResource(resB2)
      stool.sdiff(sresA,sresB)
    }

    script2.entries.foreach(println)

  }
}
