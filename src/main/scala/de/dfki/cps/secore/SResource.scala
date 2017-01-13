package de.dfki.cps.secore

import de.dfki.cps.stools.{ISElement, SAnnotation, SElement}
import de.dfki.cps.stools.similarityspec.ElementSimilaritySpec
import org.eclipse.emf.ecore.EPackage
import org.eclipse.emf.ecore.resource.Resource

import scala.beans.BeanProperty
import scala.collection.JavaConversions._

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
class SResource(underlying: Resource) extends SElement[Resource] {
  def getObject(): Resource = underlying

  lazy val getChildren: java.util.List[ISElement[_]] =
    underlying.getContents().map(new SEObject(_))

  val getParent: ISElement[_] = null

  def getType(): String =
    "Resource"

  def getNamespace(): String =
    ""

  def getLabel(): String = getType

  def getAnnotations(): java.util.List[SAnnotation[_]] =
    List.empty[SAnnotation[_]]

  def hasAnnotation(namespace: String, name: String): java.lang.Boolean =
    false

  def getAnnotation(namespace: String, name: String): SAnnotation[_] =
    null

  @BeanProperty
  var equivSpec: String = null

  @BeanProperty
  var similaritySpec: ElementSimilaritySpec = null

  def copy(): SElement[Resource] = {
    val result = new SResource(underlying)
    result.setEquivSpec(getEquivSpec())
    result.setSimilaritySpec(getSimilaritySpec())
    result.setEditScript(getEditScript())
    result
  }

  override def toString = {
    getLabel
  }
}