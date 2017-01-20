package de.dfki.cps.secore

import de.dfki.cps.stools.{SAnnotation, SElement}

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class SLiteral(val parent: SAttribute, val index: Int, val underlying: AnyRef) extends SElement[AnyRef] {
  def tpe = parent.underlying.getEAttributeType
  def factory = tpe.getEPackage.getEFactoryInstance
  def children: Seq[SElement[_]] = Nil
  def label: String = factory.convertToString(tpe,underlying)
  override def getType = "SLiteral"
  def namespace: String = ""
  def annotations: Seq[SAnnotation[_]] = Nil
  def copy(): SElement[_] = {
    val res = new SLiteral(parent,index,underlying)
    res.setEditScript(getEditScript())
    res.setEquivSpec(equivSpec)
    res.setSimilaritySpec(similaritySpec)
    res
  }
  override def toString = s"[SLiteral:${tpe.getName} '$label']"
}
