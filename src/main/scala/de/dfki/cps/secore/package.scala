package de.dfki.cps

import de.dfki.cps.stools.SAtomicString
import de.dfki.cps.stools.editscript._
import org.eclipse.emf.common.util.{EList, URI}
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.{EAttribute, EObject, EReference}

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
package object secore {
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
            //println("uri: " + uri + " (" + uri.hasAuthority + ")")
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
            println(ref)
            println(ref2)
            val v = ref.parent.underlying.eGet(ref2.underlying).asInstanceOf[EList[EObject]]
            v.removeAll(values.asJava)
        }
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
