package de.dfki.cps.secore

import de.dfki.cps.stools.{SAnnotation, SElement}
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.util.EcoreUtil

object SLink {
  private [secore] def getUri(obj: EObject, parent: SObject): String = {
    if (obj.eResource() != null && obj.eResource().getURI == parent.underlying.eResource().getURI) {
      EcoreUtil.getRelativeURIFragmentPath(EcoreUtil.getRootContainer(obj), obj)
      obj.eResource().getURIFragment(obj)
    } else {
      EcoreUtil.getURI(obj).toString
    }
  }
}

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class SLink(val parent: SReference, val index: Int, val underlying: EObject) extends SElement[AnyRef] {
  def children: Seq[SElement[_]] = Nil
  def label: String = SLink.getUri(underlying,parent.parent)
  override def getType = "SLink"
  def namespace: String = ""
  def annotations: Seq[SAnnotation[_]] = Nil
  def copy(): SElement[_] = {
    val res = new SLink(parent,index,underlying)
    res.setEditScript(getEditScript())
    res.setEquivSpec(equivSpec)
    res.setSimilaritySpec(similaritySpec)
    res
  }
  override def toString = s"[SLink '$label']"
}
