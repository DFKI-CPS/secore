package de.dfki.cps.secore

import de.dfki.cps.stools.{SAnnotation, SElement}
import de.dfki.cps.stools.similarityspec.ElementSimilaritySpec
import org.eclipse.emf.ecore.{EPackage, EcorePackage}
import org.eclipse.emf.ecore.resource.Resource

import scala.beans.BeanProperty
import scala.collection.JavaConverters._

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
class SResource(val underlying: Resource) extends SElement[Resource] {
  def children: Seq[SElement[_]] = underlying.getContents.asScala.map(new SObject(_))
  def label: String = "Resource"
  def namespace: String = ""
  def annotations: Seq[SAnnotation[_]] = Nil
  def parent: SElement[_] = null
  def copy(): SElement[_] = {
    val res = new SResource(underlying)
    res.setEquivSpec(equivSpec)
    res.setSimilaritySpec(similaritySpec)
    res.setEditScript(getEditScript)
    res
  }
  override def toString = "Resource"
}