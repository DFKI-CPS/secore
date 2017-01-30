package de.dfki.cps

import java.io.FileInputStream

import de.dfki.cps.stools.{SAtomicString, STools}
import de.dfki.cps.stools.editscript._
import de.dfki.cps.stools.similarityspec.SimilaritySpec
import org.eclipse.emf.common.util.{EList, URI}
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.EObject

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.io.Source

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
package object secore {
  val stools = {
    val content = {
      val stream = getClass.getClassLoader.getResourceAsStream(s"de/dfki/cps/secore/specific.simeq")
      SimilaritySpec.fromString(Source.fromInputStream(stream).mkString)
    }
    new STools(content)
  }

  def applyEditScript(editScript: SEditScript) = {
    var deferredResolutions: mutable.Buffer[() => Unit] = mutable.Buffer.empty
    editScript.entries.foreach {
      case (res: SResource, entry) =>
      case (obj: SObject, entry) =>
        assert(entry.insertAfter.isEmpty)
        assert(entry.insertBefore.isEmpty)
        entry.updateAnnotations.foreach {
          case UpdateAnnotation(_,o: SAttributeValue,n: SAttributeValue) =>
            val tpe = n.underlying.getEAttributeType
            val nv = tpe.getEPackage.getEFactoryInstance.createFromString(tpe,n.value)
            obj.underlying.eSet(n.underlying,nv)
          case UpdateAnnotation(_,o: SReferenceValue,n: SReferenceValue) =>
            val uri = URI.createURI(n.value)
            if (!uri.hasAuthority) deferredResolutions.append { () =>
              val value = o.parent.underlying.eResource().getEObject(n.value)
              obj.underlying.eSet(o.underlying,value)
            } else {
              o.parent.underlying.eSet(o.underlying,o.parent.underlying.eResource().getResourceSet.getEObject(uri,true))
            }
        }
        entry.removeAnnotations.foreach {
          case RemoveAnnotations(ref,annons) =>
            annons.asScala.foreach {
              case s: SAttributeValue =>
                obj.underlying.eUnset(s.underlying)
              case s: SReferenceValue =>
                obj.underlying.eUnset(s.underlying)
            }
        }
        entry.appendAnnotations.foreach {
          case AppendAnnotations(ref,annons) =>
            annons.asScala.foreach {
              case s: SAttributeValue =>
                obj.underlying.eSet(s.underlying, s.parent.underlying.eGet(s.underlying))
            }
        }

      case (attr: SAttribute, entry) =>
        assert(entry.appendAnnotations.isEmpty)
        assert(entry.removeAnnotations.isEmpty)
        assert(entry.updateAnnotations.isEmpty)
        entry.removeElements.foreach {
          case RemoveElements(_,elems) =>
            val tpe = attr.underlying.getEAttributeType
            val factory = tpe.getEPackage.getEFactoryInstance
            val values = elems.asScala.map {
              case s: SLiteral =>
                s.underlying
            }
            val v = attr.parent.underlying.eGet(attr.underlying).asInstanceOf[EList[AnyRef]]
            v.removeAll(values.asJava)
        }
        entry.appendElements.foreach {
          case AppendElements(_,elems) =>
            val tpe = attr.underlying.getEAttributeType
            val factory = tpe.getEPackage.getEFactoryInstance
            val values = elems.asScala.map {
              case s: SLiteral => s.underlying
            }
            val v = attr.parent.underlying.eGet(attr.underlying).asInstanceOf[EList[AnyRef]]
            v.addAll(values.asJava)
        }
        entry.insertBefore.foreach {
          case (_,InsertBefore(ref: SLiteral, elems)) =>
            val values = elems.asScala.map {
              case s: SLiteral =>
                s.underlying
            }
            ref.parent.parent.underlying.eGet(ref.parent.underlying).asInstanceOf[EList[AnyRef]]
              .addAll(ref.index,values.asJava)
        }
        entry.insertAfter.foreach {
          case (_,InsertAfter(ref: SLiteral, elems)) =>
            val values = elems.asScala.map {
              case s: SLiteral =>
                s.underlying
            }
            ref.parent.parent.underlying.eGet(ref.parent.underlying).asInstanceOf[EList[AnyRef]]
              .addAll(ref.index + 1,values.asJava)
        }
      case (ref: SReference, entry) if ref.underlying.isContainment =>
        entry.appendElements.foreach {
          case AppendElements(ref: SReference,elems) =>
            val values = elems.asScala.map {
              case o: SObject => EcoreUtil.copy(o.underlying)
            }
            val v = ref.parent.underlying.eGet(ref.underlying).asInstanceOf[EList[EObject]]
            v.addAll(values.asJava)
        }
        entry.removeElements.foreach {
          case RemoveElements(ref2: SReference,elems) =>
            val values = elems.asScala.map {
              case o: SObject =>
                o.underlying
            }
            val v = ref.parent.underlying.eGet(ref2.underlying).asInstanceOf[EList[EObject]]
            v.removeAll(values.asJava)
        }
        /*entry.insertBefore.foreach {
          case (_,InsertBefore(refr: SObject, elems)) =>
            val values = elems.asScala.map {
              case s: SObject => EcoreUtil.copy(s.underlying)
            }
            ref.parent.underlying.eGet(ref.underlying).asInstanceOf[EList[EObject]]
              .addAll(ref.index,values.asJava)
        }
        entry.insertAfter.foreach {
          case (_,InsertAfter(refr: SObject, elems)) =>
            val values = elems.asScala.map {
              case s: SObject => EcoreUtil.copy(s.underlying)
             }
            ref.parent.underlying.eGet(ref.underlying).asInstanceOf[EList[EObject]]
              .addAll(ref.index + 1,values.asJava)
        }*/
      case (ref: SReference, entry) =>
        entry.removeElements.foreach {
          case RemoveElements(_,elems) => deferredResolutions.append { () =>
            val values = elems.asScala.map {
              case o: SAtomicString => o.line
            }
          }
        }
    }
    deferredResolutions.foreach(_())
  }
}
