package de.dfki.cps.secore

import java.lang.Boolean
import java.util

import de.dfki.cps.stools.similarityspec.ElementSimilaritySpec
import de.dfki.cps.stools.{ISElement, SAnnotation, SAtomicString, SElement}
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.{EAttribute, EObject}

import collection.JavaConverters._
import scala.beans.BeanProperty

class SAttributeValue(val owner: SEObject, val attr: EAttribute) extends SAnnotation[String] {
  lazy val tpe = attr.getEAttributeType
  lazy val factory = tpe.getEPackage.getEFactoryInstance

  def getObject(): String =
    factory.convertToString(tpe,owner.underlying.eGet(attr))
  def getName(): String = attr.getName
  def getValue(): String = getObject()
  def getNameSpace(): String = ""
  def getParent(): ISElement[_] = owner

  override def toString = s"$getName=$getValue"
}

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class SEAttribute(val owner: SEObject, val attr: EAttribute) extends SElement[AnyRef] {
  lazy val tpe = attr.getEAttributeType
  lazy val factory = tpe.getEPackage.getEFactoryInstance

  def getObject(): AnyRef = owner.underlying.eGet(attr)

  def getChildren(): util.List[ISElement[_]] = if (attr.isMany) {
    val list = owner.underlying.eGet(attr).asInstanceOf[EList[AnyRef]]
    val reprs = list.asScala.map(factory.convertToString(tpe,_))
    reprs.map(new SAtomicString(_,null,null): ISElement[_]).asJava
  } else {
    Nil.asJava
  }

  def getType(): String = "Attribute." + (if (attr.isOrdered) "Ordered" else "Unordered")
  def getNamespace(): String = ""
  def getLabel(): String = attr.getName()

  def getAnnotations(): util.List[SAnnotation[_]] = Nil.asJava

  def hasAnnotation(namespace: String, name: String): Boolean = false
  def getAnnotation(namespace: String, name: String): SAnnotation[_] = null

  @BeanProperty var equivSpec: String = null
  @BeanProperty var similaritySpec: ElementSimilaritySpec = null

  def getParent(): ISElement[_] = owner

  def copy(): SElement[AnyRef] = {
    val result = new SEAttribute(owner,attr)
    result.equivSpec = this.equivSpec
    result.similaritySpec = this.similaritySpec
    result.setEditScript(getEditScript())
    result
  }

  override def toString: String = s"$getType[$getLabel]"
}
