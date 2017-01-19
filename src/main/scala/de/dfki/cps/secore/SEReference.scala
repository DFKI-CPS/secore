package de.dfki.cps.secore

import java.lang.Boolean
import java.util

import de.dfki.cps.stools.similarityspec.ElementSimilaritySpec
import de.dfki.cps.stools.{ISElement, SAnnotation, SAtomicString, SElement}
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.{EAttribute, EObject, EReference, EcorePackage}

import collection.JavaConverters._
import scala.beans.BeanProperty

class Null(val getParent: ISElement[_]) extends SElement[scala.Null] {
  def getObject(): scala.Null = null
  def getChildren(): util.List[ISElement[_]] = Nil.asJava
  def getType: String = "NULL"
  def getNamespace(): String = ""
  def copy(): de.dfki.cps.stools.ISElement[_] = {
    val res = new Null(getParent)
    res.equivSpec = this.equivSpec
    res.similaritySpec = this.similaritySpec
    res
  }
  def getAnnotation(namespace: String,name: String): de.dfki.cps.stools.SAnnotation[_] = null
  def getAnnotations(): java.util.List[de.dfki.cps.stools.SAnnotation[_]] = Nil.asJava
  @BeanProperty var equivSpec: String = null
  @BeanProperty var similaritySpec: ElementSimilaritySpec = null
  def getLabel(): String = getType
  def hasAnnotation(namespace: String,name: String): Boolean = false
}

class SReferenceValue(val owner: SEObject, val ref: EReference) extends SAnnotation[String] {
  lazy val tpe = attr.getEAttributeType
  lazy val factory = tpe.getEPackage.getEFactoryInstance
  lazy val value = if (owner.underlying.eIsSet(attr)) owner.underlying.eGet(attr) else attr.getDefaultValue

  def getObject(): String = {
    factory.convertToString(tpe,value)
  }
  def getName(): String = attr.getName
  def getValue(): String = Option(getObject()).getOrElse("<NULL>")
  def getNameSpace(): String = ""
  def getParent(): ISElement[_] = owner

  override def toString = s"$getName=$getValue"
}
/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class SEReference(val owner: SEObject, val ref: EReference) extends SElement[AnyRef] {
  def getObject(): AnyRef = owner.underlying.eGet(ref)

  private def getUri(obj: EObject): String = {
    if (obj.eResource().getURI == owner.underlying.eResource().getURI) {
      EcoreUtil.getRelativeURIFragmentPath(EcoreUtil.getRootContainer(obj),obj)
      obj.eResource().getURIFragment(obj)
    } else {
      EcoreUtil.getURI(obj).toString
    }
  }

  def getChildren(): util.List[ISElement[_]] = if (ref.isMany) {
    val list = owner.underlying.eGet(ref).asInstanceOf[EList[EObject]]
    val reprs = list.asScala
    if (ref.isContainment)
      reprs.map(new SEObject(_) : ISElement[_]).asJava
    else
      reprs.map(getUri).map(new SAtomicString(_,null,null) : ISElement[_]).asJava
  } else if (ref.isContainment) {
    val obj = Option(owner.underlying.eGet(ref).asInstanceOf[EObject]).map(new SEObject(_)).getOrElse(new Null(this))
    List[ISElement[_]](obj).asJava
  } else {
    Nil.asJava
  }

  def getType(): String = ref.getEContainingClass.getName + "_" + ref.getName
  def getNamespace(): String = ref.getEContainingClass.getEPackage.getNsURI
  def getLabel(): String = getType()

  val uriAnnotation = if (!ref.isMany && !ref.isContainment) Some()

  def getAnnotations(): util.List[SAnnotation[_]] = if (!ref.isMany && !ref.isContainment) {

  }
  def hasAnnotation(namespace: String, name: String): Boolean = false
  def getAnnotation(namespace: String, name: String): SAnnotation[_] = null

  @BeanProperty var equivSpec: String = null
  @BeanProperty var similaritySpec: ElementSimilaritySpec = null

  def getParent(): ISElement[_] = owner

  def copy(): SElement[AnyRef] = {
    val result = new SEReference(owner,ref)
    result.equivSpec = this.equivSpec
    result.similaritySpec = this.similaritySpec
    result.setEditScript(getEditScript())
    result
  }

  override def toString = s"$getType"
}
