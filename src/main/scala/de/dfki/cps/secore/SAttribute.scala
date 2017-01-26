package de.dfki.cps.secore

import de.dfki.cps.stools.{SAnnotation, SAtomicString, SElement}
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.{EAttribute}

import collection.JavaConverters._

class SAttributeValue(val parent: SObject, val underlying: EAttribute) extends SAnnotation[EAttribute] {
  def tpe = underlying.getEAttributeType
  def factory = tpe.getEPackage.getEFactoryInstance

  def name: String = underlying.getEContainingClass.getName + "_" + underlying.getName
  def value: String = factory.convertToString(tpe,Option(parent.underlying.eGet(underlying)).getOrElse(underlying.getDefaultValue))
  def namespace = ""//underlying.getEContainingClass.getEPackage.getNsURI

  override def toString = underlying.getEContainingClass.getEPackage.getNsPrefix + ":" + name

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: SAttributeValue => this.parent == that.parent && this.underlying == that.underlying
    case _ => false
  }
}

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class SAttribute(val parent: SObject, val underlying: EAttribute) extends SElement[EAttribute] {
  assert(underlying.isMany)
  override def getEquivSpec(): String = "specific"

  def children: Seq[SElement[_]] = {
    val list = parent.underlying.eGet(underlying).asInstanceOf[EList[AnyRef]].asScala
    list.zipWithIndex.map {
      case (v,i) => new SLiteral(this,i,v)
    }
  }

  def label: String = underlying.getEContainingClass.getName + "_" + underlying.getName

  def namespace: String = ""//underlying.getEContainingClass.getEPackage.getNsURI

  def annotations: Seq[SAnnotation[_]] = Nil

  def copy(): SElement[EAttribute] = {
    val result = new SAttribute(parent,underlying)
    result.setEquivSpec(equivSpec)
    result.setSimilaritySpec(similaritySpec)
    result.setEditScript(getEditScript())
    result
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: SAttribute => this.parent == that.parent && this.underlying == that.underlying
    case _ => false
  }

  override def toString = underlying.getEContainingClass.getEPackage.getNsPrefix + ":" + label
}
