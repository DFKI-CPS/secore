package de.dfki.cps.secore

import de.dfki.cps.stools.{ISElement, SAnnotation, SElement}
import de.dfki.cps.stools.similarityspec.ElementSimilaritySpec
import org.eclipse.emf.ecore.{EAttribute, ENamedElement, EObject, EReference}
import org.eclipse.uml2.uml.{Model, NamedElement}

import scala.beans.BeanProperty
import scala.collection.JavaConverters._

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
class SEObject(val underlying: EObject) extends SElement[EObject] {
  def getObject(): EObject = underlying

  lazy val features = underlying.eClass().getEAllStructuralFeatures.asScala
    .filter(underlying.eIsSet)
    .filter(f => !f.isTransient && !f.isDerived && !f.isVolatile)

  lazy val getChildren: java.util.List[ISElement[_]] =
    features.collect {
          case ref: EReference =>
            new SEReference(this,ref) : ISElement[_]
          case attr: EAttribute if attr.isMany =>
            new SEAttribute(this,attr) : ISElement[_]
        }.asJava

  lazy val getParent: ISElement[_] =
    Option(underlying.eContainer()).map(new SEObject(_)).getOrElse(new SResource(underlying.eResource()))

  def getType(): String = underlying match {
    case other => "EObject"
  }

  def getNamespace(): String =
    ""//underlying.eClass().getEPackage.getNsURI

  def getLabel(): String =
    underlying.eClass().getName()

  lazy val annotations = features.collect {
    case attr: EAttribute if !attr.isMany =>
      attr.getName -> (new SAttributeValue(this,attr) : SAnnotation[_])
  }.toMap[String,SAnnotation[_]]

  lazy val getAnnotations: java.util.List[SAnnotation[_]] =
    annotations.values.toList.asJava

  def hasAnnotation(namespace: String, name: String): java.lang.Boolean =
    annotations.isDefinedAt(name)

  def getAnnotation(namespace: String, name: String): SAnnotation[_] =
    annotations.getOrElse(name,null)

  @BeanProperty var equivSpec: String = null
  @BeanProperty var similaritySpec: ElementSimilaritySpec = null

  def copy(): SElement[EObject] = {
    val result = new SEObject(underlying)
    result.equivSpec = this.equivSpec
    result.similaritySpec = this.similaritySpec
    result.setEditScript(getEditScript())
    result
  }

  override def equals(that: Any) = that match {
    case that: SEObject => that.underlying == this.underlying
    case _ => false
  }

  override def toString = {
    s"$getType[$getLabel]"
  }
}