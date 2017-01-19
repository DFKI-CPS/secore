package de.dfki.cps

import de.dfki.cps.stools.SAtomicString
import de.dfki.cps.stools.editscript._
import org.eclipse.emf.common.util.{EList, URI}
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.{EAttribute, EObject, EReference}

import scala.collection.JavaConverters._

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
package object secore {
  def applyEditScript(editScript: SEditScript) = {

    editScript.entries.foreach {
      case (res: SResource, entry) =>
      case (obj: SEObject, entry) =>
        entry.updateAnnotations.foreach {
          case UpdateAnnotation(_,o: SAttributeValue,n: SAttributeValue) =>
            val tpe = n.attr.getEAttributeType
            val nv = tpe.getEPackage.getEFactoryInstance.createFromString(tpe,n.getValue())
            obj.underlying.eSet(n.attr,nv)
        }
      case (attr: SEAttribute, entry) =>
        entry.appendElements.foreach {
          case AppendElements(_,elems) =>
            val tpe = attr.attr.getEAttributeType
            val factory = tpe.getEPackage.getEFactoryInstance
            val values = elems.asScala.map {
              case s: SAtomicString =>
                factory.createFromString(tpe,s.getObject())
            }
            val v = attr.owner.underlying.eGet(attr.attr).asInstanceOf[EList[AnyRef]]
            v.addAll(values.asJava)
        }
        entry.removeElements.foreach {
          case RemoveElements(_,elems) =>
            val tpe = attr.attr.getEAttributeType
            val factory = tpe.getEPackage.getEFactoryInstance
            val values = elems.asScala.map {
              case s: SAtomicString =>
                factory.createFromString(tpe,s.getObject())
            }
            val v = attr.owner.underlying.eGet(attr.attr).asInstanceOf[EList[AnyRef]]
            v.removeAll(values.asJava)
        }

      case (ref: SEReference, entry) =>
        entry.appendElements.foreach {
          case AppendElements(_,elems) =>
            val values = elems.asScala.collect {
              case o: SEObject => EcoreUtil.copy(o.underlying)
              case s: SAtomicString => ref.owner.underlying.eResource().getEObject(s.getObject())
            }
            if (ref.ref.isMany) {
              val v = ref.owner.underlying.eGet(ref.ref).asInstanceOf[EList[EObject]]
              v.addAll(values.asJava)
            }
        }
    }
    /*
    editScript.entries.foreach {
      case (obj: SEObject, entry) =>
        println((obj,entry))
        entry.appendAnnotations.foreach {
          case AppendAnnotations(el: SEObject,annons) =>
            annons.asScala.foreach {
              case attr: SAttributeValue =>
                val tpe = attr.attr.getEAttributeType
                val value = tpe.getEPackage.getEFactoryInstance.createFromString(tpe, attr.getValue())
                el.underlying.eSet(attr.attr, value)
            }
        }
        entry.removeAnnotations.foreach {
          case RemoveAnnotations(ref: SEObject,annons) =>
            annons.asScala.foreach {
              case attr: SAttributeValue =>
                ref.underlying.eUnset(attr.attr)
            }
        }
        entry.updateAnnotations.foreach {
          case UpdateAnnotation(ref: SEObject,_,attr: SAttributeValue) =>
            val tpe = attr.attr.getEAttributeType
            val value = tpe.getEPackage.getEFactoryInstance.createFromString(tpe,attr.getValue())
            ref.underlying.eSet(attr.attr,value)
        }
      case (attr: SEAttribute, entry) =>
        entry.appendElements.foreach {
          case AppendElements(el: SEAttribute, elems) =>
            val tpe = el.attr.getEAttributeType
            val factory = tpe.getEPackage.getEFactoryInstance
            val values = elems.asScala.map {
              case s: SAtomicString =>
                factory.createFromString(tpe,s.getLabel())
            }
            el.owner.underlying.eGet(el.attr).asInstanceOf[EList[AnyRef]].addAll(values.asJava)
        }
        entry.removeElements.foreach {
          case RemoveElements(el: SEAttribute, elems) =>
            val tpe = el.attr.getEAttributeType
            val factory = tpe.getEPackage.getEFactoryInstance
            val values = elems.asScala.map {
              case s: SAtomicString =>
                factory.createFromString(tpe,s.getLabel())
            }
            val list = el.owner.underlying.eGet(el.attr).asInstanceOf[EList[AnyRef]]
            list.removeAll(values.asJava)
        }
      case (ref: SEReference, entry) =>
        entry.appendElements.foreach {
          case AppendElements(el: SEReference, elems) =>
            val values = elems.asScala.map {
              case l: SEReferenceLink =>
                ref.owner.underlying.eResource().getResourceSet.getEObject(URI.createURI(l.uri), true)
              case s: SEObject =>
                s.underlying
            }
            if (el.ref.isMany)
              el.owner.underlying.eGet(ref.ref).asInstanceOf[EList[EObject]].addAll(values.asJava)
            else {
              assert(values.size == 1)
              el.owner.underlying.eSet(ref.ref, values.head)
            }
        }
        entry.removeElements.foreach {
          case RemoveElements(el: SEReference, elems) =>
            if (el.ref.isMany) {
              val r = ref.owner.underlying.eGet(el.ref).asInstanceOf[EList[EObject]]
              val values = elems.asScala.map {
                case l: SEReferenceLink =>
                  ref.owner.underlying.eResource().getResourceSet.getEObject(URI.createURI(l.uri), true)
                case obj: SEObject =>
                  obj.underlying
              }
              r.removeAll(values.asJava)
            } else {
              el.owner.underlying.eUnset(el.ref)
            }
        }
        entry.insertBefore.foreach {
          case (_, InsertBefore(a,elems)) =>
            val after = a match {
              case l: SEReferenceLink =>
                val link = URI.createURI(l.uri)
                ref.owner.underlying.eResource().getResourceSet.getEObject(link,true)
              case obj: SEObject =>
                obj.underlying
            }
            val r = ref.owner.underlying.eGet(ref.ref).asInstanceOf[EList[Object]]
            val values = elems.asScala.map {
              case l: SEReferenceLink =>
                ref.owner.underlying.eResource().getResourceSet.getEObject(URI.createURI(l.uri), true)
              case s: SEObject =>
                s.underlying
            }
            val i = r.indexOf(after)
            r.addAll(i,values.asJava)
        }
    }*/
  }
}
