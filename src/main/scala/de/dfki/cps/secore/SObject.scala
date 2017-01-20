package de.dfki.cps.secore

import de.dfki.cps.stools.{SAnnotation, SElement}
import de.dfki.cps.stools.similarityspec.ElementSimilaritySpec
import org.eclipse.emf.ecore.{EAttribute, EObject, EReference}
import scala.beans.BeanProperty
import scala.collection.JavaConverters._

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
class SObject(val underlying: EObject) extends SElement[EObject] {
  def getObject(): EObject = underlying

  def features = underlying.eClass().getEAllStructuralFeatures.asScala
    .filter(f => !f.isTransient && !f.isDerived && !f.isVolatile)
    .filter(f => underlying.eIsSet(f) || f.isMany || f.isInstanceOf[EAttribute] && !f.isUnsettable && f.getDefaultValue != null)

  def children =
    features.collect {
      case ref: EReference if ref.isContainment || ref.isMany =>
        new SReference(this,ref)
      case attr: EAttribute if attr.isMany =>
        new SAttribute(this,attr)
    }

  def parent =
    Option(underlying.eContainer()).map(new SObject(_)).getOrElse(new SResource(underlying.eResource()))


  def label = underlying.eClass().getName
  def namespace = underlying.eClass().getEPackage.getNsURI

  def annotations = features.collect {
    case attr: EAttribute if !attr.isMany =>
      new SAttributeValue(this,attr)
    case ref: EReference if !ref.isContainment && !ref.isMany =>
      new SReferenceValue(this,ref)
  }

  def copy(): SElement[EObject] = {
    val result = new SObject(underlying)
    result.setEquivSpec(this.equivSpec)
    result.setSimilaritySpec(this.similaritySpec)
    result.setEditScript(getEditScript())
    result
  }

  override def equals(that: Any) = that match {
    case that: SObject => that.underlying == this.underlying
    case _ => false
  }

  override def toString = {
    underlying.eClass().getEPackage.getNsPrefix + ":" + getType()
  }
}