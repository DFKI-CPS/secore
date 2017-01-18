package de.dfki.cps.secore

import java.lang.Boolean
import java.util

import de.dfki.cps.stools.similarityspec.ElementSimilaritySpec
import de.dfki.cps.stools.{ISElement, SAnnotation, SAtomicString, SElement}
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.{EAttribute, EObject, EReference}

import collection.JavaConverters._
import scala.beans.BeanProperty



class SEReferenceLink(ref: SEReference, obj: EObject) extends SElement[String] { self =>
  lazy val uri = if (obj.eResource().getURI == ref.owner.underlying.eResource().getURI) {
    EcoreUtil.getRelativeURIFragmentPath(EcoreUtil.getRootContainer(obj),obj)
    obj.eResource().getURIFragment(obj)
  } else {
    EcoreUtil.getURI(obj).toString
  }

  lazy val uriAnnotation = new SAnnotation[String] {
    def getObject(): String = uri
    def getName(): String = "value"
    def getValue(): String = uri
    def getNameSpace(): String = ""
    def getParent(): ISElement[_] = self
  }

  def getObject() = uri

  def getName(): String = "uri"
  def getChildren(): util.List[ISElement[_]] = Nil.asJava

  def getType(): String = "SEReferenceLink"

  def getNamespace(): String = ""

  def getLabel(): String = getType()

  def getAnnotations(): util.List[SAnnotation[_]] = List[SAnnotation[_]](uriAnnotation).asJava

  def hasAnnotation(namespace: String, name: String): Boolean = name == "value"

  def getAnnotation(namespace: String, name: String): SAnnotation[_] = if (name == "value") {
    uriAnnotation
  } else null

  @BeanProperty var equivSpec: String = null
  @BeanProperty var similaritySpec: ElementSimilaritySpec = null

  def getParent(): ISElement[_] = ref

  def copy(): SElement[String] = {
    val result = new SEReferenceLink(ref,obj)
    result.equivSpec = this.equivSpec
    result.similaritySpec = this.similaritySpec
    result.setEditScript(getEditScript())
    result
  }

  override def toString: String = s"link<$uri>"
}

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
class SEReference(val owner: SEObject, val ref: EReference) extends SElement[AnyRef] {
  def getObject(): AnyRef = owner.underlying.eGet(ref)

  def getChildren(): util.List[ISElement[_]] = if (ref.isMany) {
    val list = owner.underlying.eGet(ref).asInstanceOf[EList[EObject]]
    val reprs = list.asScala
    if (ref.isContainment)
      reprs.map(new SEObject(_) : ISElement[_]).asJava
    else
      reprs.map(new SEReferenceLink(this,_) : ISElement[_]).asJava
  } else if (ref.isContainment) {
    List[ISElement[_]](new SEObject(owner.underlying.eGet(ref).asInstanceOf[EObject])).asJava
  } else {
    List[ISElement[_]](new SEReferenceLink(this,owner.underlying.eGet(ref).asInstanceOf[EObject])).asJava
  }

  def getType(): String = "Reference." + (if (ref.isOrdered) "Ordered" else "Unordered")
  def getNamespace(): String = ""
  def getLabel(): String = ref.getName

  def getAnnotations(): util.List[SAnnotation[_]] = Nil.asJava
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

  override def toString = s"$getType[$getLabel]"
}
