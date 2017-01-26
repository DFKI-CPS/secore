package de.dfki.cps.secore

import java.io.{File, FileOutputStream, PrintStream}

import org.eclipse.emf.ecore._
import org.eclipse.uml2.uml.UMLPackage

import scala.collection.JavaConverters._

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
object SimSpecGen {
  val globalFeatureFilter = Set[EStructuralFeature](
    EcorePackage.eINSTANCE.getEModelElement_EAnnotations,
    UMLPackage.eINSTANCE.getElement_OwnedComment,
    UMLPackage.eINSTANCE.getNamedElement_NameExpression,
    UMLPackage.eINSTANCE.getNamedElement_Visibility
  ) ++ UMLPackage.eINSTANCE.getNamespace.getEStructuralFeatures.asScala

  val nameAttributes = Seq[EStructuralFeature](
    UMLPackage.eINSTANCE.getNamedElement_Name
  )

  val named = Set[EClass](
    UMLPackage.eINSTANCE.getState
  )

  def relevantFeature(f: EStructuralFeature): Boolean = {
    !f.isDerived && !f.isTransient && !f.isVolatile && f.isChangeable && !globalFeatureFilter.contains(f)
  }

  def generateFromPackages(name: String, pkgs: Iterable[EPackage], out: PrintStream): Unit = {
    out.println(s"equivspec $name {")
    /*pkgs.foreach { pkg =>
      val prefix = pkg.getNsPrefix
      val uri = pkg.getNsURI
      out.println(s"  xmlns:$prefix = '$uri'")
    }*/

    out.println()
    val classes = pkgs.flatMap(_.getEClassifiers.asScala.collect {
      case cls: EClass => cls
    })
    val subclasses = classes.map { cls =>
      (cls, classes.filter(_.getEAllSuperTypes.contains(cls)).filter(!_.isAbstract).toSeq)
    }.toMap
    subclasses.foreach { case (cls,sub) =>
      println(cls.getName + ": ")
      sub.map(_.getName).foreach(x => println("  " + x))
    }
    pkgs.foreach { pkg =>
      val prefix = pkg.getNsPrefix
      pkg.getEClassifiers.asScala.foreach {
        case cls: EClass =>
          val elem = /*prefix + ":" +*/ cls.getName
          out.println(s"  element $elem {")
          val features = cls.getEAllStructuralFeatures.asScala.filter(relevantFeature)
          val singleAttrs = features.collect {
            case attr: EAttribute if named.contains(cls) && nameAttributes.contains(attr) =>
              attr.getEContainingClass.getName + "_" + attr.getName + "!"
            case attr: EAttribute if !attr.isMany =>
              //attr.getEContainingClass.getEPackage.getNsPrefix + ":" +
              attr.getEContainingClass.getName + "_" + attr.getName + (if (attr.isRequired) "!" else if (attr.isUnsettable) "?" else "")
            case ref: EReference if !ref.isMany && !ref.isContainment && !Option(ref.getEOpposite).exists(_.isContainment) =>
              //ref.getEContainingClass.getEPackage.getNsPrefix + ":" +
                ref.getEContainingClass.getName + "_" + ref.getName + (if (ref.isRequired) "!" else if (ref.isUnsettable) "?" else "")
          }
          val multiAttrs = features.collect {
            case attr: EAttribute if attr.isMany =>
              val nsPrefix = ""//attr.getEContainingClass.getEPackage.getNsPrefix + ":"
              nsPrefix + attr.getEContainingClass.getName + "_" + attr.getName
            case ref: EReference if ref.isContainment => //|| ref.isMany =>
              val nsPrefix = ""//ref.getEContainingClass.getEPackage.getNsPrefix + ":"
              nsPrefix + ref.getEContainingClass.getName + "_" + ref.getName
          }
          if (singleAttrs.nonEmpty) out.println(s"    annotations { ${singleAttrs.mkString(" ")} }")
          if (multiAttrs.nonEmpty) out.println(s"    constituents { unordered { ${multiAttrs.mkString(" ")} } }")
          out.println("  }")
          cls.getEStructuralFeatures.asScala.filter(relevantFeature).collect {
            case attr: EAttribute if attr.isMany =>
              val elem = /*prefix + ":" +*/ cls.getName + "_" + attr.getName
              out.println(s"  element $elem {")
              val ordering = if (attr.isOrdered) "ordered" else "unordered"
              out.println(s"    constituents { $ordering { <TEXT> } }")
              out.println(s"  }")
            case ref: EReference if ref.isContainment || ref.isMany =>
              val elem = /*prefix + ":" +*/ cls.getName + "_" + ref.getName
              out.println(s"  element $elem {")
              val ordering = if (ref.isOrdered) "ordered" else "unordered"
              if (ref.isContainment) {
                val tpe = ref.getEReferenceType
                val stpes = subclasses(tpe)
                val cs = (if (tpe.isAbstract) stpes else (tpe +: stpes)).map { tpe =>
                  val tpkg = tpe.getEPackage
                  val nsPrefix = tpkg.getNsPrefix
                  /*nsPrefix + ":" + */tpe.getName
                }
                out.println(s"    constituents { $ordering { ${cs.mkString(" ")} } }")
              } else {
                out.println(s"    constituents { $ordering { <TEXT> } }")
              }
              out.println(s"  }")
          }
        case tpe: EDataType =>
      }
    }
    out.println("}")
  }
}

object SimSpecGenTest extends App {
  val outDir = new File("src/main/resources/de/dfki/cps/secore")
  if (!outDir.exists()) outDir.mkdirs()
  val out = new FileOutputStream(s"src/main/resources/de/dfki/cps/secore/specific.simeq")
  val stream = new PrintStream(out)
  val packages = Seq(
    org.eclipse.emf.ecore.EcorePackage.eINSTANCE,
    org.eclipse.uml2.uml.UMLPackage.eINSTANCE,
    org.eclipse.papyrus.sysml.SysmlPackage.eINSTANCE,
    org.eclipse.papyrus.sysml.blocks.BlocksPackage.eINSTANCE,
    org.eclipse.papyrus.sysml.portandflows.PortandflowsPackage.eINSTANCE
  )
  SimSpecGen.generateFromPackages("specific", packages, stream)

  out.close()
}
