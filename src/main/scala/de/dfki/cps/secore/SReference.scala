package de.dfki.cps.secore

import de.dfki.cps.stools.{SAnnotation, SAtomicString, SElement}
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.{EObject, EReference}

import collection.JavaConverters._

class SReferenceValue(val parent: SObject, val underlying: EReference) extends SAnnotation[EReference] {
  def name: String = underlying.getEContainingClass.getName + "_" + underlying.getName
  def eObject = parent.underlying.eGet(underlying).asInstanceOf[EObject]
  def value: String = SLink.getUri(eObject,parent)
  def namespace: String = ""//underlying.getEContainingClass.getEPackage.getNsURI
  override def toString = underlying.getEContainingClass.getEPackage.getNsPrefix + ":" + name
}

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class SReference(val parent: SObject, val underlying: EReference) extends SElement[EReference] {
  override def getEquivSpec(): String = "specific"
  def children: Seq[SElement[_]] = if (underlying.isMany) {
    val list = parent.underlying.eGet(underlying).asInstanceOf[EList[EObject]].asScala
    if (underlying.isContainment) {
      list.map(new SObject(_))
    } else
      list.zipWithIndex.map {
        case (obj,i) => new SLink(this,i,obj)
      }
  } else if (underlying.isContainment) {
    val obj = Option(parent.underlying.eGet(underlying))
    obj.map(_.asInstanceOf[EObject]).map(new SObject(_)).toList
  } else sys.error("SReference may not be instantiated with single valued non-containment (Use SReferenceValue instead)")

  def label: String = underlying.getEContainingClass.getName + "_" + underlying.getName
  def namespace: String = ""//underlying.getEContainingClass.getEPackage.getNsURI
  def annotations: Seq[SAnnotation[_]] = Nil
  def copy(): SElement[EReference] = {
    val result = new SReference(parent,underlying)
    result.setEquivSpec(equivSpec)
    result.setSimilaritySpec(similaritySpec)
    result.setEditScript(getEditScript())
    result
  }

  override def equals(that: Any) = that match {
    case that: SReference => this.parent == that.parent && this.underlying == that.underlying
    case _ => false
  }

  override def toString = underlying.getEContainingClass.getEPackage.getNsPrefix + ":" + label
}
