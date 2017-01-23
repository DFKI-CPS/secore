package de.dfki.cps.secore

import java.io.{ByteArrayOutputStream, File, FileOutputStream, PrintStream}

import de.dfki.cps.stools.similarityspec.SimilaritySpec
import org.eclipse.emf.ecore._

import scala.collection.JavaConverters._

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
object SimSpecGen {
  def relevantFeature(f: EStructuralFeature): Boolean = {
    !f.isDerived && !f.isTransient && !f.isVolatile && f.isChangeable
  }

  def generateFromPackage(pkg: EPackage, out: PrintStream): Unit = {
    val spec = new SimilaritySpec
    val ns = pkg.getNsPrefix
    out.println(s"equivspec $ns xmlns '${pkg.getNsURI}' {")
    val entries = pkg.getEClassifiers.asScala.foreach {
      case cls: EClass =>
        out.println(s"  element ${cls.getName} {")
        val features = cls.getEAllStructuralFeatures.asScala.filter(relevantFeature)
        val singleAttrs = features.collect {
          case attr: EAttribute if !attr.isMany =>
            attr.getEContainingClass.getName + "_" + attr.getName + (if (attr.isRequired) "!" else if (attr.isUnsettable) "?" else "")
          case ref: EReference if !ref.isMany && !ref.isContainment =>
            ref.getEContainingClass.getName + "_" + ref.getName + (if (ref.isRequired) "!" else if (ref.isUnsettable) "?" else "")
        }
        val multiAttrs = features.collect {
          case attr: EAttribute if attr.isMany =>
            val nsPrefix = if (attr.getEContainingClass.getEPackage == pkg) "" else "[" + attr.getEContainingClass.getEPackage.getNsPrefix + "]"
            nsPrefix + attr.getEContainingClass.getName + "_" + attr.getName + nsPrefix
          case ref: EReference if ref.isContainment || ref.isMany =>
            val nsPrefix = if (ref.getEContainingClass.getEPackage == pkg) "" else "[" + ref.getEContainingClass.getEPackage.getNsPrefix + "]"
            ref.getEContainingClass.getName + "_" + ref.getName + nsPrefix
        }
        if (singleAttrs.nonEmpty) out.println(s"    annotations { ${singleAttrs.mkString(" ")} }")
        if (multiAttrs.nonEmpty) out.println(s"    constituents { unordered { ${multiAttrs.mkString(", ")} } }")
        out.println("  }")
        cls.getEStructuralFeatures.asScala.filter(relevantFeature).collect {
          case attr: EAttribute if attr.isMany =>
            out.println(s"  element ${cls.getName}_${attr.getName} {")
            val ordering = if (attr.isOrdered) "ordered" else "unordered"
            out.println(s"    constituents { $ordering { SLiteral } }")
            out.println(s"  }")
          case ref: EReference if ref.isContainment || ref.isMany =>
            out.println(s"  element ${cls.getName}_${ref.getName} {")
            val ordering = if (ref.isOrdered) "ordered" else "unordered"
            if (ref.isContainment) {
              val tpe = ref.getEReferenceType
              val tpkg = tpe.getEPackage
              val nsPrefix = if (tpkg == pkg) "" else "[" +tpkg.getNsPrefix+ "]"
              out.println(s"    constituents { $ordering { ${tpe.getName}$nsPrefix } }")
            } else {
              out.println(s"    constituents { $ordering { SLink } }")
            }
            out.println(s"  }")
        }
      case tpe: EDataType =>
    }
    out.println("}")
  }
}

object SimSpecGenTest extends App {
  def generate(p: EPackage) = {
    val outDir = new File("src/main/resources/de/dfki/cps/secore")
    if (!outDir.exists()) outDir.mkdirs()
    val out = new FileOutputStream(s"src/main/resources/de/dfki/cps/secore/${p.getNsPrefix}.simeq")
    val stream = new PrintStream(out)
    SimSpecGen.generateFromPackage(p, stream)
    out.close()
  }
  generate(org.eclipse.emf.ecore.EcorePackage.eINSTANCE)
  generate(org.eclipse.uml2.uml.UMLPackage.eINSTANCE)
  generate(org.eclipse.papyrus.sysml.SysmlPackage.eINSTANCE)
  generate(org.eclipse.papyrus.sysml.blocks.BlocksPackage.eINSTANCE)
  generate(org.eclipse.papyrus.sysml.portandflows.PortandflowsPackage.eINSTANCE)
}
