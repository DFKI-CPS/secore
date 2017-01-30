package de.dfki.cps.secore

import java.io.{File, FileOutputStream, PrintStream}

import org.eclipse.emf.ecore._
import org.eclipse.uml2.uml.UMLPackage

import scala.collection.JavaConverters._

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
object SimSpecGen {
  import EcorePackage.{ eINSTANCE => ecore }
  import UMLPackage.{ eINSTANCE => uml }

  val globalFeatureFilter = Set[EStructuralFeature](
    ecore.getEModelElement_EAnnotations,
    uml.getElement_OwnedComment,
    uml.getNamedElement_NameExpression,
    uml.getNamedElement_Visibility
  ) ++ uml.getNamespace.getEStructuralFeatures.asScala

  val nameAttributes = Seq[EStructuralFeature](
    uml.getNamedElement_Name
  )


  val ids = Map[EClass,Set[EStructuralFeature]](
    uml.getPackage -> Set(uml.getNamedElement_Name),
    uml.getClass_ -> Set(uml.getNamedElement_Name),
    uml.getProperty -> Set(uml.getNamedElement_Name),
    uml.getOperation -> Set(uml.getNamedElement_Name),
    uml.getStateMachine -> Set(uml.getNamedElement_Name),
    uml.getState -> Set(uml.getNamedElement_Name)
  )

  def relevantFeature(f: EStructuralFeature): Boolean = {
    !f.isDerived && !f.isTransient && !f.isVolatile && f.isChangeable
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
    pkgs.foreach { pkg =>
      val prefix = pkg.getNsPrefix
      pkg.getEClassifiers.asScala.foreach {
        case cls: EClass =>
          if (!cls.isAbstract) {
            /*if (subclasses(cls).nonEmpty)
              println("WARN: CANNOT HANDLE NONABSTRACT SUPERCLASSES: " + cls.getName)*/
            val elem = /*prefix + ":" +*/ cls.getName
            out.println(s"  element $elem {")
            val strong = ids.isDefinedAt(cls)
            val features = ids.getOrElse(cls,
              cls.getEAllStructuralFeatures.asScala
                .filter(relevantFeature)
                .filter(!globalFeatureFilter.contains(_))
                .toSet
            )
            val singleAttrs = features.collect {
              case attr: EAttribute if !attr.isMany =>
                //attr.getEContainingClass.getEPackage.getNsPrefix + ":" +
                attr.getEContainingClass.getName + "_" + attr.getName + (if (attr.isRequired || strong) "!" else if (attr.isUnsettable) "?" else "")
              case ref: EReference if !ref.isMany && !ref.isContainment && !Option(ref.getEOpposite).exists(_.isContainment) =>
                //ref.getEContainingClass.getEPackage.getNsPrefix + ":" +
                ref.getEContainingClass.getName + "_" + ref.getName + (if (ref.isRequired || strong) "!" else if (ref.isUnsettable) "?" else "")
            }
            val multiAttrs = features.collect {
              case attr: EAttribute if attr.isMany =>
                val nsPrefix = "" //attr.getEContainingClass.getEPackage.getNsPrefix + ":"
                nsPrefix + attr.getEContainingClass.getName + "_" + attr.getName
              case ref: EReference if ref.isContainment => //|| ref.isMany =>
                val nsPrefix = "" //ref.getEContainingClass.getEPackage.getNsPrefix + ":"
                nsPrefix + ref.getEContainingClass.getName + "_" + ref.getName
            }
            if (singleAttrs.nonEmpty) out.println(s"    annotations { ${singleAttrs.mkString(" ")} }")
            if (multiAttrs.nonEmpty) out.println(s"    constituents { unordered { ${multiAttrs.mkString(" ")} } }")
            out.println("  }")
          } else {

          }
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
                out.println(s"    constituents { $ordering { _ } }")
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
